#Server page for individual statistics

#select panel for user

#load in data
bowling_individual <- DBI::dbGetQuery(
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

bowling_individual_wickets <- DBI::dbGetQuery(
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

batting_individual <- DBI::dbGetQuery(
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

partnerhip_individual <- DBI::dbGetQuery(
  conn,
  "SELECT
    r.opposition,
    r.match_date,
    r.league_name,
    wickets,
    batsman_out_name,
    batsman_in_name,
    partnership_runs
    FROM partnerships p
    left join results r on p.match_id = r.id;"
) 

observeEvent(c(input$player_scope_individual,
               input$team_scope_individual), {

#filter data by player
batting_individual <- batting_individual |> 
  filter(batsman_name == input$player_scope_individual,
         league_name %in% input$team_scope_individual)

bowling_individual <- bowling_individual |> 
  filter(bowler_name == input$player_scope_individual,
         league_name %in% input$team_scope_individual)

bowling_individual_wickets <- bowling_individual_wickets |> 
  filter(bowler_name == input$player_scope_individual,
         league_name %in% input$team_scope_individual)

# individual batting stats
## summary stats
batting_individual_summary <- batting_individual |>
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
  arrange(desc(average)) |> 
  select(Batter = batsman_name,
         `# Innings` = innings,
         `# Out` = dismissed,
         Runs = runs,
         `Balls Faced` = balls_faced,
         Fours = fours,
         Sixes = sixes,
         `Runs/Innings` = runs_per_innings,
         Average = average,
         `Strike Rate` = strike_rate,
         `% Runs from Boundaries` = percent_runs_boundaries) |> 
  select(-Batter) |> 
  pivot_longer(cols = c(`# Innings`:`% Runs from Boundaries`),
               names_to = "Metric",
               values_to = "Value")

output$individual_batting_summary <- renderReactable({reactable(batting_individual_summary,
                                                                highlight = TRUE,
                                                                striped = TRUE)})

## batting details by game
batting_individual_by_game <- batting_individual |> 
  select(`Match Date` = match_date,
         `League` = league_name,
         Opposition = opposition,
         `Batting Position` = position,
         `How Out` = clean_dismissal,
         Runs = runs,
         Balls = balls,
         Fours = fours,
         Sixes = sixes,
         count_innings,
         count_out) |> 
  mutate(`Strike Rate` = round((Runs / Balls) * 100,0),
         `% Runs from Boundaries` = round(((Fours * 4 + Sixes * 6) / Runs)*100,2))

output$individual_batting_by_game <- renderReactable({reactable(batting_individual_by_game |> 
                                                                  select(-count_innings,
                                                                         -count_out),
                                                                highlight = TRUE,
                                                                striped = TRUE,
                                                                showSortable = TRUE)})

###create bar chart for batting
batting_individual_game_graph_data <- batting_individual_by_game |> 
  filter(`How Out` != 'DNB') |> 
  mutate(`Match Number` = row_number(),
         `Cumulative Runs` = cumsum(Runs),
         `Cumulative Balls` = cumsum(Balls),
         cum_innings = cumsum(count_innings),
         cum_out = cumsum(count_out),
         `Cumulative Average` = round(`Cumulative Runs`/cum_out,2),
         `Cumulative Strike Rate` = round((`Cumulative Runs`/`Cumulative Balls`)*100,2),
         `Cumulative Runs per Innings` = round(`Cumulative Runs`/cum_innings,2))

#reactive graph depending on the data shown
observeEvent(input$batting_stat_scope_individual, {
  
  individual_batting_stat <- paste0("Cumulative ",input$batting_stat_scope_individual)
  individual_batting_stat_variable_short <- paste0(individual_batting_stat)
  
  batting_individual_game_graph_data <- batting_individual_game_graph_data |> 
    mutate(cum_data = get(individual_batting_stat_variable_short))

plotly_batting_individual <- plot_ly(batting_individual_game_graph_data, 
        x = ~`Match Number`, y = ~Runs, type = "bar", name = "Runs by Game",
        hoverinfo = "text",
        hovertext = paste("Match Date:", batting_individual_game_graph_data$`Match Date`,
                          "<br>Opposition:", batting_individual_game_graph_data$Opposition,
                          "<br>Competition:", batting_individual_game_graph_data$League,
                          "<br>Runs:", batting_individual_game_graph_data$Runs,
                          "<br>Balls:", batting_individual_game_graph_data$Balls,
                          "<br>Strike Rate:", batting_individual_game_graph_data$`Strike Rate`)) |> 
  add_trace(x = ~`Match Number`, y = ~cum_data, type = "scatter", mode = "lines+markers", yaxis = "y2", name = individual_batting_stat,
            hoverinfo = "text",
            hovertext = paste0("Matches Played: ", batting_individual_game_graph_data$`Match Number`,
                              "<br>",individual_batting_stat,": ", batting_individual_game_graph_data$cum_data)) |> 
  layout(yaxis2 = list(overlaying = "y", side = "right"))

output$individual_batting_plotly <- renderPlotly(plotly_batting_individual)

})

## summary stats by position
batting_individual_summary_position <- batting_individual |>
  group_by(batsman_name,
           position) |>
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
  arrange(desc(average)) |> 
  select(Batter = batsman_name,
         Position = position,
         `# Innings` = innings,
         `# Out` = dismissed,
         Runs = runs,
         `Balls Faced` = balls_faced,
         Fours = fours,
         Sixes = sixes,
         `Runs/Innings` = runs_per_innings,
         Average = average,
         `Strike Rate` = strike_rate,
         `% Runs from Boundaries` = percent_runs_boundaries) |> 
  select(-Batter) |> 
  arrange(Position) |> 
  mutate(Position = paste0("Number ", Position)) |> 
  pivot_longer(cols = c(`# Innings`:`% Runs from Boundaries`),
               names_to = "Metric",
               values_to = "Value") |> 
    pivot_wider(names_from = "Position",
                values_from = "Value")


output$individual_batting_summary_position <- renderReactable({reactable(batting_individual_summary_position,
                                                                         highlight = TRUE,
                                                                         striped = TRUE)})

## summary stats by opposition
batting_individual_summary_opposition <- batting_individual |>
  group_by(batsman_name,
           opposition) |>
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
  arrange(desc(average)) |> 
  select(Batter = batsman_name,
         Opposition = opposition,
         `# Innings` = innings,
         `# Out` = dismissed,
         Runs = runs,
         `Balls Faced` = balls_faced,
         Fours = fours,
         Sixes = sixes,
         `Runs/Innings` = runs_per_innings,
         Average = average,
         `Strike Rate` = strike_rate,
         `% Runs from Boundaries` = percent_runs_boundaries) |> 
  select(-Batter) |>  
  pivot_longer(cols = c(`# Innings`:`% Runs from Boundaries`),
               names_to = "Metric",
               values_to = "Value") |> 
  pivot_wider(names_from = "Opposition",
              values_from = "Value")





output$individual_batting_summary_opposition <- renderReactable({reactable(batting_individual_summary_opposition,
                                                                         highlight = TRUE,
                                                                         striped = TRUE)})


## dismissals

batting_individual_dismissals <- batting_individual |> 
  select(batsman_name,
         league_name,
         clean_dismissal) |> 
  group_by(batsman_name,
           clean_dismissal) |> 
  tally()



### take account of excluding DNBs or Not Outs

### plot pie chart

output$individual_dismissal_pie <- renderPlotly({plot_ly(data = batting_individual_dismissals,
                                    values = ~n,
                                    labels = ~clean_dismissal,
                                    type = "pie")})


# individual bowling stats
bowling_individual_summary <- bowling_individual |>
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
    overs = as.numeric(format(as.numeric(paste0(complete_overs, ".", residual_balls)),nsmall=1)),
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
  arrange(desc(Wickets)) |> 
  select(-Bowler) |> 
  pivot_longer(cols = c(`Overs`:`%Runs from Extras`),
               names_to = "Metric",
               values_to = "Value")

output$individual_bowling_summary <- renderReactable({reactable(bowling_individual_summary,
                                                                highlight = TRUE,
                                                                striped = TRUE)})

### bowling game by game
bowling_individual_by_game <- bowling_individual |>
  mutate(
    complete_overs = floor(ball_count / 6),
    residual_balls = ball_count - (6 * complete_overs),
    overs = as.numeric(format(as.numeric(paste0(complete_overs, ".", residual_balls)),nsmall=1)),
    average = round(runs / wickets,2),
    strike_rate = round(ball_count / wickets,2),
    economy = round(runs / (complete_overs + (residual_balls / 6)),2),
    percent_runs_extras = round(((no_balls + wides) / runs)*100,2)) |>
  select(
    "Bowler" = bowler_name,
    `Match Date` = match_date,
    `League` = league_name,
    Opposition = opposition,
    "Overs" = overs,
    "Maidens"= maidens,
    "Runs Conceded" = runs,
    "Wickets" = wickets,
    "Average" = average,
    "Strike Rate" = strike_rate,
    "Economy" = economy,
    "Wides" = wides,
    "No Balls" = no_balls,
    "%Runs from Extras" = percent_runs_extras,
    ball_count
  ) |> 
  select(-Bowler)

output$individual_bowling_by_game <- renderReactable({reactable(bowling_individual_by_game |> 
                                                                  select(-ball_count),
                                                                highlight = TRUE,
                                                                striped = TRUE)})

### bowling by game graph
bowling_individual_game_graph_data <- bowling_individual_by_game |> 
  #calculate individual cumulative info
  mutate(`Match Number` = row_number(),
         `Cumulative Wickets` = cumsum(Wickets),
         cum_runs = cumsum(`Runs Conceded`),
         cum_balls = cumsum(ball_count),
         `Cumulative Average` = round(cum_runs/`Cumulative Wickets`,2),
         `Cumulative Strike Rate` = round(cum_balls/`Cumulative Wickets`,2),
         cum_complete_overs = floor(cum_balls / 6),
         cum_residual_balls = cum_balls - (6 * cum_complete_overs),
         `Cumulative Economy` = round(cum_runs / (cum_complete_overs + (cum_residual_balls / 6)),2))

#reactive graph depending on the data shown
observeEvent(input$bowling_stat_scope_individual, {
  
  individual_bowling_stat <- paste0("Cumulative ",input$bowling_stat_scope_individual)
  individual_bowling_stat_variable_short <- paste0(individual_bowling_stat)
  #individual_bowling_stat_variable_full <- paste0("bowling_individual_game_graph_data$`",individual_bowling_stat,"`")
  
  bowling_individual_game_graph_data <- bowling_individual_game_graph_data |> 
    mutate(cum_data = get(individual_bowling_stat_variable_short))
  
plotly_bowling_individual <- plot_ly(bowling_individual_game_graph_data, 
                                     x = ~`Match Number`, y = ~Wickets, type = "bar", name = "Wickets by Game",
                                     hoverinfo = "text",
                                     hovertext = paste("Match Date:", bowling_individual_game_graph_data$`Match Date`,
                                                       "<br>Opposition:", bowling_individual_game_graph_data$Opposition,
                                                       "<br>Competition:", bowling_individual_game_graph_data$League,
                                                       "<br>Wickets:", bowling_individual_game_graph_data$Wickets,
                                                       "<br>Overs:", bowling_individual_game_graph_data$Overs,
                                                       "<br>Maidens:", bowling_individual_game_graph_data$Maidens,
                                                       "<br>Runs Conceded:", bowling_individual_game_graph_data$`Runs Conceded`,
                                                       "<br>Average:", bowling_individual_game_graph_data$Average,
                                                       "<br>Strike Rate:", bowling_individual_game_graph_data$`Strike Rate`,
                                                       "<br>Economy:", bowling_individual_game_graph_data$Economy)) |> 
  add_trace(x = ~`Match Number`, y = ~cum_data, type = "scatter", mode = "lines+markers", yaxis = "y2", name = individual_bowling_stat,
            hoverinfo = "text",
            hovertext = paste0("Matches Played: ", bowling_individual_game_graph_data$`Match Number`,
                              "<br>",individual_bowling_stat,": ", bowling_individual_game_graph_data$cum_data)) |> 
  layout(yaxis2 = list(overlaying = "y", side = "right"))

output$individual_bowling_plotly <- renderPlotly(plotly_bowling_individual)

})

### by opposition
bowling_individual_summary_opposition <- bowling_individual |>
  group_by(bowler_name,
           opposition) |>
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
    overs = as.numeric(format(as.numeric(paste0(complete_overs, ".", residual_balls)),nsmall=1)),
    average = round(runs / wickets,2),
    strike_rate = round(ball_count / wickets,2),
    economy = round(runs / (complete_overs + (residual_balls / 6)),2),
    percent_runs_extras = round(((no_balls + wides) / runs)*100,2)) |>
  select(
    "Bowler" = bowler_name,
    "Opposition" = opposition,
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
  arrange(desc(Wickets)) |> 
  select(-Bowler) |> 
  pivot_longer(cols = c(`Overs`:`%Runs from Extras`),
               names_to = "Metric",
               values_to = "Value") |> 
  pivot_wider(names_from = "Opposition",
              values_from = "Value")



output$individual_bowling_summary_opposition <- renderReactable({reactable(bowling_individual_summary_opposition,
                                                                highlight = TRUE,
                                                                striped = TRUE)})

#bowling individual wicket dismissals
bowling_individual_wickets_summary <- bowling_individual_wickets |> 
  select(bowler_name,
         league_name,
         clean_dismissal) |> 
  group_by(bowler_name,
           clean_dismissal) |> 
  tally()

output$individual_wickets_pie <- renderPlotly({plot_ly(data = bowling_individual_wickets_summary,
                                                         values = ~n,
                                                         labels = ~clean_dismissal,
                                                         type = "pie")})

})