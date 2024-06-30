# cut down stats for visualising
# bowling_summary <- read_csv("data/db_dump/bowling_summary.csv")
bowling_summary <- DBI::dbGetQuery(
  conn,
  "SELECT
    r.opposition,
    r.match_date,
    r.league_name,
    bowler_name,
    ball_count,
    maidens,
    runs,
    wides,
    no_balls,
    wickets
    FROM bowling b
    left join results r on b.match_id = r.id;"
)

bowling_wickets <- DBI::dbGetQuery(
  conn,
  "SELECT
    r.opposition,
    r.match_date,
    r.league_name,
    bowler_name,
    bd.clean_dismissal,
    fielder_name
    FROM bowling_wickets b
    left join batting_dismissals bd on b.how_out = bd.pc_dismissal
    left join results r on b.match_id = r.id;"
)


observeEvent(input$team_scope_bowling, {
  
  if(is.null(input$team_scope_bowling)){
    input_team_scope <- league_names
  } else{
    input_team_scope <- input$team_scope_bowling
  }


# Produce summary table of bowling stats
bowling_summary_default <- bowling_summary |>
  filter(league_name %in% input_team_scope) |> 
    group_by(bowler_name) |>
    summarise(
        ball_count = sum(ball_count, na.rm = TRUE),
        maidens = sum(maidens, na.rm = TRUE),
        runs = sum(runs, na.rm = TRUE),
        wides = sum(wides, na.rm = TRUE),
        no_balls = sum(no_balls, na.rm = TRUE),
        wickets = sum(wickets, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
        complete_overs = floor(ball_count / 6),
        residual_balls = ball_count - (6 * complete_overs),
        overs = format(as.numeric(paste0(complete_overs, ".", residual_balls)),nsmall=1),
        average = round(runs / wickets,2),
        strike_rate = round(ball_count / wickets,2),
        economy = round(runs / (complete_overs + (residual_balls / 6)),2),
        percent_runs_extras = round(((no_balls + wides) / runs)*100,2)) |>
  select(
    "Bowler" = bowler_name,
    "Overs" = overs,
    "Maidens"= maidens,
    "Runs Conceded" = runs,
    "Wickets" = wickets,
    "Average" = average,
    "Strike Rate" = strike_rate,
    "Economy" = economy,
    "Wides" = wides,
    "No Balls" = no_balls,
    "%Runs from Extras" = percent_runs_extras
  ) |> 
  arrange(desc(Wickets))
  

output$bowling_summary <- renderReactable({reactable(bowling_summary_default,
                                                     filterable = TRUE,
                                                     searchable = TRUE,
                                                     highlight = TRUE,
                                                     striped = TRUE,
                                                     showSortable = TRUE)})

# cumulative wickets over time
bowling_cum_sum <- bowling_summary |> 
  filter(league_name %in% input_team_scope) |> 
  select(match_date,
         bowler_name,
         wickets) |> 
  ungroup() |> 
  mutate(match_date = as.POSIXct(match_date, format = "%d/%m/%Y")) |> 
  filter(!is.na(wickets)) |> 
  group_by(bowler_name) |> 
  mutate(wickets_cum_sum = cumsum(wickets)) |> 
  ungroup() 

#get the cumulative match wickets for percentage of total wickets scored
bowling_cum_sum_match <- bowling_summary |> 
  filter(league_name %in% input_team_scope) |>
  select(match_date,
         wickets) |> 
  ungroup() |> 
  mutate(match_date = as.POSIXct(match_date, format = "%d/%m/%Y")) |> 
  filter(!is.na(wickets)) |> 
  group_by(match_date) |> 
  summarise(wickets_to_bowler_match = sum(wickets)) |> 
  mutate(wickets_cum_sum_match = cumsum(wickets_to_bowler_match)) |> 
  ungroup() |> 
  select(-wickets_to_bowler_match)

#get unique dates
unique_dates <- bowling_cum_sum |> 
  select(match_date_unique = match_date) |> 
  distinct() 

#get the day before the first game
day_before <- unique_dates |> slice_min(unique_dates) - (60*60*24)

#add the day before in
unique_dates <- unique_dates |> 
  rbind(day_before) |> 
  arrange(match_date_unique)

#get unique players
unique_players <- bowling_cum_sum |> 
  select(bowler_unique = bowler_name) |> 
  distinct()


#get a complete dataset of dates and players
bowling_cum_sum_all <- unique_dates |>
  cross_join(unique_players) |> 
  #join on known runs
  left_join(bowling_cum_sum,
            by = c("match_date_unique" = "match_date",
                   "bowler_unique" = "bowler_name")) |>
  left_join(bowling_cum_sum_match,
            by = c("match_date_unique" = "match_date")) |> 
  #set the first week runs to 0
  mutate(wickets_cum_sum = case_when(match_date_unique == day_before[[1]] ~ 0,
                                  TRUE ~ wickets_cum_sum),
         wickets_cum_sum_match = case_when(match_date_unique == day_before[[1]] ~ 1,
                                        TRUE ~ wickets_cum_sum_match)) |> 
  #fill in the weeks where someone didn't play
  group_by(bowler_unique) |> 
  fill(wickets_cum_sum, .direction = "downup") |> 
  ungroup() |> 
  mutate(percent_team_wickets = round(100*(wickets_cum_sum/wickets_cum_sum_match),2)) |> 
  arrange(match_date_unique, desc(wickets_cum_sum)) |> 
  group_by(match_date_unique) |> 
  #rank the players by number of runs
  mutate(Rank = rank(wickets_cum_sum, ties.method = "first")) |> 
  arrange(desc(Rank)) |> 
  ungroup() 


# plot graphs
graph_bowling_area_raw <- ggplot(bowling_cum_sum_all |>
                                   arrange(desc(match_date_unique)) |>
                                   select(Bowler = bowler_unique,
                                          `Match Date` = match_date_unique,
                                          Wickets = wickets_cum_sum),
                                 aes(x=`Match Date`,
                                     y=Wickets,
                                     fill=Bowler)) +
  geom_area(stat="identity",colour="grey2") +
  geom_vline(xintercept = c(as.numeric(unique_dates$match_date_unique))) +
  theme(legend.position="none")


graph_bowling_area_raw <- ggplotly(graph_bowling_area_raw)

output$bowling_total_area_raw<- renderPlotly({graph_bowling_area_raw})


#percent of wickets taken
graph_bowling_area_percent <- ggplot(bowling_cum_sum_all |>
                                       arrange(desc(match_date_unique)) |>
                                       select(Bowler = bowler_unique,
                                              `Match Date` = match_date_unique,
                                              Wickets = percent_team_wickets),
                                     aes(x=`Match Date`,
                                         y=Wickets,
                                         fill=Bowler)) +
  geom_area(stat="identity",colour="grey2") +
  geom_vline(xintercept = c(as.numeric(unique_dates$match_date_unique))) +
  theme(legend.position="none")


graph_bowling_area_percent <- ggplotly(graph_bowling_area_percent)

output$bowling_total_area_percent <- renderPlotly({graph_bowling_area_percent})



  

})