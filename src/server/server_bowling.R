# cut down stats for visualising
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

observeEvent(input$team_scope_bowling, {
  
  if(is.null(input$team_scope_bowling)){
    input_team_scope <- league_names
  } else{
    input_team_scope <- input$team_scope_bowling
  }


# Produce summary table of batting stats
# TODO: allow the user to group/filter by league
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
  

output$bowling_summary <- renderDT({datatable(bowling_summary_default)})

# cumulative wickets over time
# extras percentage of runs conceded


})