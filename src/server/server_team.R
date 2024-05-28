#server_team.R

#get summary data for matches/innings
match_summary <- DBI::dbGetQuery(
  conn,
  "SELECT
    *
    FROM match_summary m
  left join results r on m.match_id = r.id;"
)


observeEvent(input$team_scope_team, {
  
  if(is.null(input$team_scope_team)){
    input_team_scope <- league_names
  } else{
    input_team_scope <- input$team_scope_team
  }
  
  team_summary_data <- match_summary |> 
    filter(league_name %in% input_team_scope) |>
    #summarise the batting data by group
    summarise(total_batting_runs_scored = sum(batting_runs_scored, na.rm = TRUE),
              total_batting_wickets_lost = sum(batting_wickets_lost, na.rm = TRUE),
              total_batting_balls_faced = sum(batting_overs_faced_overs, na.rm = TRUE)*6 + sum(batting_overs_faced_balls, na.rm = TRUE),
              #summarise the bowling data by group
              total_bowling_balls_bowled = sum(bowling_overs_bowled_overs, na.rm = TRUE)*6 + sum(bowling_overs_bowled_balls, na.rm = TRUE),
              total_bowling_runs_conceded = sum(bowling_runs_conceded, na.rm = TRUE),
              total_bowling_wickets_taken = sum(bowling_wickets_taken, na.rm = TRUE),
              total_bowling_wides_conceded = sum(bowling_wides_conceded, na.rm = TRUE),
              total_bowling_no_balls_conceded = sum(bowling_no_balls_conceded, na.rm = TRUE),
              total_bowling_byes_conceded = sum(bowling_byes_conceded, na.rm = TRUE),
              total_bowling_leg_byes_conceded = sum(bowling_leg_byes_conceded, na.rm = TRUE)) |> 
    #calculate batting statistics
    mutate(overall_batting_average = round(total_batting_runs_scored/total_batting_wickets_lost,2),
           overall_batting_strike_rate = round((total_batting_runs_scored/total_batting_balls_faced)*100,2),
           overall_batting_run_rate = round(total_batting_runs_scored/(total_batting_balls_faced/6),2),
           #calculate bowling statistics
           overall_bowling_average = round(total_bowling_runs_conceded/total_bowling_wickets_taken,2),
           overall_bowling_strike_rate = round(total_bowling_balls_bowled/total_bowling_wickets_taken,2),
           overall_bowling_economy = round(total_bowling_runs_conceded/(total_bowling_balls_bowled/6),2),
           #calculate number of overs faced batting
           total_batting_overs_faced_overs = floor(total_batting_balls_faced/6),
           total_batting_overs_faced_balls = total_batting_balls_faced-(total_batting_overs_faced_overs*6),
           total_batting_overs_faced = as.numeric(paste0(total_batting_overs_faced_overs,".",total_batting_overs_faced_balls)),
           #calculate number of overs bowled bowling
           total_bowling_overs_bowled_overs = floor(total_bowling_balls_bowled/6),
           total_bowling_overs_bowled_balls = total_bowling_balls_bowled-(total_bowling_overs_bowled_overs*6),
           total_bowling_overs_bowled = as.numeric(paste0(total_bowling_overs_bowled_overs,".",total_bowling_overs_bowled_balls))) |> 
    #select columns wanted
    select(total_batting_runs_scored,
           total_batting_wickets_lost,
           total_batting_overs_faced,
           overall_batting_average,
           overall_batting_strike_rate,
           overall_batting_run_rate,
           total_bowling_overs_bowled,
           total_bowling_runs_conceded,
           total_bowling_wickets_taken,
           overall_bowling_average,
           overall_bowling_strike_rate,
           overall_bowling_economy) |> 
    #make into one long table
    pivot_longer(cols = everything(),
                 names_to = "Metric",
                 values_to = "Value")
  
  output$team_summary <- renderDT({datatable(team_summary_data,
                                             rownames = FALSE,
                                             options = list(
                                               paging = FALSE,
                                               autoWidth = TRUE,
                                               columnDefs = list(list(width = '500px', targets = c(0)))))})
  
  
})


#total overs faced
#total runs
#total wickets
#overall average
#overall strikerate
#total 4s
#total 6s

#total overs bowled
#total maidens
#total runs conceded
#total wickets taken
#overall average
#overall economy
#overall sr
#total wides
#total noballs
#total byes
#total leg byes


#produce a map of where all of the runs have been scored
batting_summary2 <- DBI::dbGetQuery(
  conn,
  "SELECT
    *
    FROM results b;"
)