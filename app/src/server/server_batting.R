# cut down stats for visualising
# batting_summary <- read_csv("data/db_dump/batting_summary.csv")
batting_summary <- DBI::dbGetQuery(
  conn,
  "SELECT
    r.opposition,
    r.match_date,
    r.league_name,
    position,
    batsman_name,
    bd.clean_dismissal,
    bd.count_out,
    bd.count_innings,
    runs,
    balls,
    fours,
    sixes
    FROM batting b
    left join batting_dismissals bd on b.how_out = bd.pc_dismissal
    left join results r on b.match_id = r.id;"
)


observeEvent(input$team_scope_batting, {
  
  if(is.null(input$team_scope_batting)){
    input_team_scope <- league_names
  } else{
    input_team_scope <- input$team_scope_batting
  }

# Produce summary table of batting stats
batting_summary_default <- batting_summary |>
  filter(league_name %in% input_team_scope) |> 
    group_by(batsman_name) |>
    summarise(
        innings = sum(count_innings, na.rm = TRUE),
        dismissed = sum(count_out, na.rm = TRUE),
        runs = sum(runs, na.rm = TRUE),
        balls_faced = sum(balls, na.rm = TRUE),
        fours = sum(fours, na.rm = TRUE),
        sixes = sum(sixes, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
        runs_per_innings = round(runs / innings,2),
        average = round(runs / dismissed,2),
        strike_rate = round((runs / balls_faced) * 100,0),
        percent_runs_boundaries = round(((fours * 4 + sixes * 6) / runs)*100,2)
    ) |> 
  arrange(desc(average))


output$batting_summary <- renderReactable({reactable(batting_summary_default)})


# runs by position for each batter
runs_batter_position <- batting_summary |> 
  filter(league_name %in% input_team_scope) |> 
  group_by(batsman_name, position) |> 
  summarise(
    innings = sum(count_innings, na.rm = TRUE),
    dismissed = sum(count_out, na.rm = TRUE),
    runs = sum(runs, na.rm = TRUE),
    balls_faced = sum(balls, na.rm = TRUE),
    fours = sum(fours, na.rm = TRUE),
    sixes = sum(sixes, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    runs_per_innings = round(runs / innings,2),
    average = round(runs / dismissed,2),
    strike_rate = round((runs / balls_faced) * 100,0),
    percent_runs_boundaries = round(((fours * 4 + sixes * 6) / runs)*100,2)
  ) |>
  ungroup() |> 
  arrange(position,
          batsman_name)

output$batting_position_person <- renderReactable({reactable(runs_batter_position)})

#highest score by position for
runs_batter_position_max <- batting_summary |> 
  filter(league_name %in% input_team_scope) |> 
  arrange(position, runs, balls) |> 
  slice_max(runs, n=1, by = position) |> 
  slice_min(balls, n=1, by = position) |> 
  select(position,
         batsman_name,
         opposition,
         match_date,
         league_name,
         runs,
         balls)

#plot the greaph of the highest score by position
graph_run_position <- ggplot(runs_batter_position_max, 
                             #generate the text for the tooltip
                             aes(text = paste('</br>Batting Position: ', position,
                                              '</br>Batter: ', batsman_name,
                                              '</br>Runs: ', runs,
                                              '</br>Balls: ', balls,
                                              '</br>S/R: ', round((runs/balls)*100,0),
                                              '</br>Opposition: ', opposition,
                                              '</br>Date: ', match_date))) +
  #column chart
  geom_col(aes(x=position, y=runs)) +
  #switch x axis so that batting position 1 is at the top
  scale_x_reverse(labels = runs_batter_position_max$position, breaks = runs_batter_position_max$position) +
  #swap x and y axes
  coord_flip() 
  
graph_run_position <- ggplotly(graph_run_position, tooltip = c("text"))
output$batting_position_record <- renderPlotly({graph_run_position})


# cumulative runs over time -  Area chart
#do a cumulative sum of the batting runs
batting_cum_sum <- batting_summary |> 
  filter(league_name %in% input_team_scope) |> 
  select(match_date,
         batsman_name,
         runs) |> 
  ungroup() |> 
  filter(!is.na(runs)) |> 
  group_by(batsman_name) |> 
  mutate(runs_cum_sum = cumsum(runs),
         match_date = as.POSIXct(match_date, format = "%d/%m/%Y")) |> 
  ungroup() 

#get the cumulative match runs for percentage of total runs scored
batting_cum_sum_match <- batting_summary |> 
  filter(league_name %in% input_team_scope) |>
  select(match_date,
         runs) |> 
  ungroup() |> 
  filter(!is.na(runs)) |> 
  group_by(match_date) |> 
  summarise(runs_off_bat_match = sum(runs)) |> 
  mutate(runs_cum_sum_match = cumsum(runs_off_bat_match),
         match_date = as.POSIXct(match_date, format = "%d/%m/%Y")) |> 
  ungroup() |> 
  select(-runs_off_bat_match)

#get unique dates
unique_dates <- batting_cum_sum |> 
  select(match_date_unique = match_date) |> 
  distinct() 

#get the day before the first game
day_before <- unique_dates |> slice_min(unique_dates) - (60*60*24)

#add the day before in
unique_dates <- unique_dates |> 
  rbind(day_before) |> 
  arrange(match_date_unique)

#get unique players
unique_players <- batting_cum_sum |> 
  select(batsman_unique = batsman_name) |> 
  distinct()


#get a complete dataset of dates and players
batting_cum_sum_all <- unique_dates |>
  cross_join(unique_players) |> 
  #join on known runs
  left_join(batting_cum_sum,
            by = c("match_date_unique" = "match_date",
                   "batsman_unique" = "batsman_name")) |>
  left_join(batting_cum_sum_match,
            by = c("match_date_unique" = "match_date")) |> 
  #set the first week runs to 0
  mutate(runs_cum_sum = case_when(match_date_unique == day_before[[1]] ~ 0,
                                  TRUE ~ runs_cum_sum),
         runs_cum_sum_match = case_when(match_date_unique == day_before[[1]] ~ 1,
                                  TRUE ~ runs_cum_sum_match)) |> 
  #fill in the weeks where someone didn't play
  group_by(batsman_unique) |> 
  fill(runs_cum_sum, .direction = "downup") |> 
  ungroup() |> 
  mutate(percent_team_runs = round(100*(runs_cum_sum/runs_cum_sum_match),2)) |> 
  arrange(match_date_unique, desc(runs_cum_sum)) |> 
  group_by(match_date_unique) |> 
  #rank the players by number of runs
  mutate(Rank = rank(runs_cum_sum, ties.method = "first")) |> 
  arrange(desc(Rank)) |> 
  ungroup() 


# plot graphs
graph_batting_area_raw <- ggplot(batting_cum_sum_all |>
              arrange(desc(match_date_unique)) |>
              select(Batter = batsman_unique,
                     `Match Date` = match_date_unique,
                     Runs = runs_cum_sum),
            aes(x=`Match Date`,
                y=Runs,
                fill=Batter)) +
  geom_area(stat="identity",colour="grey2") +
  geom_vline(xintercept = c(as.numeric(unique_dates$match_date_unique))) +
  theme(legend.position="none")


graph_batting_area_raw <- ggplotly(graph_batting_area_raw)

output$batting_total_area_raw<- renderPlotly({graph_batting_area_raw})



#percent of runs scored
graph_batting_area_percent <- ggplot(batting_cum_sum_all |>
                                   arrange(desc(match_date_unique)) |>
                                   select(Batter = batsman_unique,
                                          `Match Date` = match_date_unique,
                                          Runs = percent_team_runs),
                                 aes(x=`Match Date`,
                                     y=Runs,
                                     fill=Batter)) +
  geom_area(stat="identity",colour="grey2") +
  geom_vline(xintercept = c(as.numeric(unique_dates$match_date_unique))) +
  theme(legend.position="none")


graph_batting_area_percent <- ggplotly(graph_batting_area_percent)

output$batting_total_area_percent <- renderPlotly({graph_batting_area_percent})


# cumulative runs over time - animated bars
#do a cumulative sum of the batting runs
# batting_cum_sum <- batting_summary |> 
#   ungroup() |> 
#   filter(!is.na(runs)) |> 
#   group_by(batsman_name) |> 
#   mutate(runs_cum_sum = cumsum(runs))
# 
# ggplot(batting_cum_sum) +
#   geom_col(aes(x=runs_cum_sum, y=batsman_name)) +
#   transition_states(match_date,
#                     transition_length = 3,
#                     state_length = 2)



})




