# connect to the detailed database
if(file.exists("app") == TRUE){
  batting_detail_fp <- "app/data/ball-data-dump.csv"
  match_date_fp_batting <- "app/data/match_date_to_league.csv"
} else{
  batting_detail_fp <- "data/ball-data-dump.csv"
  match_date_fp_batting <- "data/match_date_to_league.csv"
}

#get the batting data only
detailed_batting_import <- read_csv(batting_detail_fp) |> 
  filter(grepl("Harwell.*",`Batting Team`)) |> 
  left_join(read_csv(match_date_fp_batting), by = c("Date" = "match_date"))

observeEvent(input$team_scope_batting, {
  
  if(is.null(input$team_scope_batting)){
    input_team_scope <- league_names
  } else{
    input_team_scope <- input$team_scope_batting
  }
  
  detailed_batting_import <- detailed_batting_import |>
    filter(league_name %in% input_team_scope)
  
  
  source("src/server/batting_detailed/batting_distance_travelled.R",local = TRUE)
  
})

#batting stats by opening/non-opening bowler
#batting stats by over in