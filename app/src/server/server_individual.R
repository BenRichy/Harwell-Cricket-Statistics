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



output$individual_batting_summary <- renderReactable({reactable(batting_individual_summary,
                                                     highlight = TRUE,
                                                     striped = TRUE)})

output$individual_batting_summary_position <- renderReactable({reactable(batting_individual_summary_position,
                                                                highlight = TRUE,
                                                                striped = TRUE)})

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



})