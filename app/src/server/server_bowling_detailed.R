# connect to the detailed database
if(file.exists("app") == TRUE){
  bowling_detail_fp <- "app/data/ball-data-dump.csv"
  match_date_fp <- "app/data/match_date_to_league.csv"
} else{
  bowling_detail_fp <- "data/ball-data-dump.csv"
  match_date_fp <- "data/match_date_to_league.csv"
}



#get the bowling data only
detailed_bowling_import <- read_csv(bowling_detail_fp) |> 
  filter(grepl("Harwell.*",`Bowling Team`)) |> 
  left_join(read_csv(match_date_fp), by = c("Date" = "match_date"))

observeEvent(input$team_scope_bowling, {
  
  if(is.null(input$team_scope_bowling)){
    input_team_scope <- league_names
  } else{
    input_team_scope <- input$team_scope_bowling
  }
  
  detailed_bowling_import <- detailed_bowling_import |>
    filter(league_name %in% input_team_scope)

source("src/server/bowling_detailed/bowling_by_end.R",local = TRUE)
source("src/server/bowling_detailed/bowling_by_batting_position.R",local = TRUE)
source("src/server/bowling_detailed/bowling_by_spell.R",local = TRUE)
source("src/server/bowling_detailed/bowling_by_over_region.R",local = TRUE)

})
