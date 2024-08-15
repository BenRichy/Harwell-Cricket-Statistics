if(file.exists("app") == TRUE){
  batting_detail_fp <- "app/data/ball-data-dump.csv"
  match_date_fp <- "app/data/match_date_to_league.csv"
} else{
  batting_detail_fp <- "data/ball-data-dump.csv"
  match_date_fp <- "data/match_date_to_league.csv"
}



#get the batting data only
detailed_batting_import_individual <- read_csv(batting_detail_fp) |> 
  filter(grepl("Harwell.*",`Batting Team`)) |> 
  left_join(read_csv(match_date_fp), by = c("Date" = "match_date")) |> 
  filter(Batter == input$player_scope_individual,
         league_name %in% input$team_scope_individual,
         season %in% input$year_scope_individual)



#only get the data for batters where the wagonwheel is available


batting_individual_wagon <- detailed_batting_import_individual |> 
  filter(!is.na(FieldX),
         Runs > 0) |> 
  select(Batter,
         Runs,
         FieldX,
         FieldY,
         league_name,
         season,
         `Bowling Team`,
         Date) |> 
  mutate(Runs = as.factor(Runs),
         FieldX = FieldX-180,
         FieldY = 180 - FieldY)


#get the teams against whom batting detail is available
output$SelectTeamBattingWagon_Individual <- renderUI({
  selectInput(
    "batting_wagon_oppo_individual",
    tags$h4("Select Opposition",align="center"),
    choices = unique(batting_individual_wagon$`Bowling Team`),
    selected = unique(batting_individual_wagon$`Bowling Team`),
    multiple = TRUE
  )
})


# wagon_batter <- ggplot(batting_individual_wagon, aes(x = FieldX, y = FieldY, colour = Runs)) +
#   geom_point() +
#   scale_color_discrete("Paired", name = "Runs") +
#   geom_segment(aes(xend = 0, yend = 0)) +
#   theme_dark() +
#   theme(panel.grid = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.title.y = element_blank(),
#         axis.title.x = element_blank())
# 
# ggplotly(wagon_batter)

observeEvent(input$batting_wagon_oppo_individual, {
  
  batting_individual_wagon <- batting_individual_wagon |> 
    filter(`Bowling Team` %in% input$batting_wagon_oppo_individual)

plotly_batting_wagon <- plot_ly(batting_individual_wagon,
                                width = 400,
                                height = 400) |> 
add_segments(x = ~FieldX, 
             y = ~FieldY, 
             xend = 0,
             yend = 0,
             type = "scatter",
             color = ~Runs,
             hoverinfo = "text",
             hovertext = paste("Match Date:", batting_individual_wagon$Date,
                               "<br>Opposition:", batting_individual_wagon$`Bowling Team`,
                               "<br>Competition:", batting_individual_wagon$league_name,
                               "<br>Runs:", batting_individual_wagon$Runs))  |> 
  layout(xaxis = list(title = '', 
                      showticklabels = FALSE,
                      showgrid = FALSE),
         yaxis = list(title = '', 
                      showticklabels = FALSE,
                      showgrid = FALSE))
                 

output$individual_batting_wagon_plotly <- renderPlotly(plotly_batting_wagon)

})
        
            