# cut down stats for visualising
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


output$batting_summary <- renderDT({datatable(batting_summary_default)})


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

output$batting_position_person <- renderDT({datatable(runs_batter_position)})

#highest score by position for
runs_batter_position_max <- batting_summary |> 
  filter(league_name %in% input_team_scope) |> 
  arrange(position, runs, balls) |> 
  filter(balls > 0) |> 
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


# cumulative runs over time


})




