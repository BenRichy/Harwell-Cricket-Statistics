if(file.exists("app") == TRUE){
  bowling_detail_fp <- "app/data/ball-data-dump.csv"
  match_date_fp <- "app/data/match_date_to_league.csv"
} else{
  bowling_detail_fp <- "data/ball-data-dump.csv"
  match_date_fp <- "data/match_date_to_league.csv"
}



#get the bowling data only
detailed_bowling_import_individual <- read_csv(bowling_detail_fp) |> 
  filter(grepl("Harwell.*",`Bowling Team`)) |> 
  left_join(read_csv(match_date_fp), by = c("Date" = "match_date")) |> 
  filter(Bowler == input$player_scope_individual,
         league_name %in% input$team_scope_individual,
         season %in% input$year_scope_individual)


#only get the data for bowlers where the wagonwheel is available


bowling_individual_wagon <- detailed_bowling_import_individual |> 
  filter(!is.na(FieldX),
         Runs > 0) |> 
  select(Bowler,
         Runs,
         FieldX,
         FieldY,
         league_name,
         season,
         `Batting Team`,
         Date) |> 
  mutate(OriginX = 180,
         OriginY = 180,
         Runs = as.factor(Runs),
         FieldX = FieldX-180,
         FieldY = 180 - FieldY)

#get the teams against whom bowling detail is available
output$SelectTeamBowlingWagon_Individual <- renderUI({
  selectInput(
    "bowling_wagon_oppo_individual",
    tags$h4("Select Opposition",align="center"),
    choices = unique(bowling_individual_wagon$`Batting Team`),
    selected = unique(bowling_individual_wagon$`Batting Team`),
    multiple = TRUE
  )
})


observeEvent(input$bowling_wagon_oppo_individual, {
  
  bowling_individual_wagon <- bowling_individual_wagon |> 
    filter(`Batting Team` %in% input$bowling_wagon_oppo_individual)
  
  plotly_bowling_wagon <- plot_ly(bowling_individual_wagon,
                                  width = 400,
                                  height = 400) |> 
    add_segments(x = ~FieldX, 
                 y = ~FieldY, 
                 xend = 0,
                 yend = 0,
                 type = "scatter",
                 color = ~Runs,
                 hoverinfo = "text",
                 hovertext = paste("Match Date:", bowling_individual_wagon$Date,
                                   "<br>Opposition:", bowling_individual_wagon$`Batting Team`,
                                   "<br>Competition:", bowling_individual_wagon$league_name,
                                   "<br>Runs Conceded:", bowling_individual_wagon$Runs))  |> 
    layout(xaxis = list(title = '', 
                        showticklabels = FALSE,
                        showgrid = FALSE),
           yaxis = list(title = '', 
                        showticklabels = FALSE,
                        showgrid = FALSE))
  
  
  output$individual_bowling_wagon_plotly <- renderPlotly(plotly_bowling_wagon)
  
})
        
            