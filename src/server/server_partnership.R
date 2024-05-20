partnership_summary <- DBI::dbGetQuery(
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


observeEvent(input$team_scope_partnership, {
  
  if(is.null(input$team_scope_partnership)){
    input_team_scope <- league_names
  } else{
    input_team_scope <- input$team_scope_partnership
  }

# highest partnership by number
  partnership_position_max <- partnership_summary |> 
    filter(league_name %in% input_team_scope) |> 
    arrange(wickets, partnership_runs) |> 
    slice_max(partnership_runs, n=1, by = wickets) |> 
    select(wickets,
           batsman_out_name,
           batsman_in_name,
           opposition,
           match_date,
           league_name,
           partnership_runs)
  
  
  #plot the greaph of the highest score by position
  graph_partnership_position <- ggplot(partnership_position_max, 
                               #generate the text for the tooltip
                               aes(text = paste('</br>Wicket Number: ', wickets,
                                                '</br>Batters: ', paste0(batsman_out_name,"/",batsman_in_name),
                                                '</br>Runs: ', partnership_runs,
                                                '</br>Opposition: ', opposition,
                                                '</br>Date: ', match_date))) +
    #column chart
    geom_col(aes(x=wickets, y=partnership_runs)) +
    #switch x axis so that batting position 1 is at the top
    scale_x_reverse(labels = partnership_position_max$wickets, breaks = partnership_position_max$wickets) +
    #swap x and y axes
    coord_flip()
  
  graph_partnership_position <- ggplotly(graph_partnership_position, tooltip = c("text"))
  output$partnership_position_record <- renderPlotly({graph_partnership_position})

# chord graph of partnerships
  partnership_chord_data <- partnership_summary |> 
    filter(league_name %in% input_team_scope) |>
    mutate(first_bat = case_when(batsman_out_name < batsman_in_name ~ batsman_out_name,
                                 TRUE ~ batsman_in_name),
           second_bat = case_when(batsman_out_name < batsman_in_name ~ batsman_in_name,
                                  TRUE ~ batsman_out_name)) |> 
    select(first_bat, second_bat, partnership_runs) |> 
    filter(partnership_runs > 0) |> 
    arrange(first_bat, second_bat)
  
  #duplicate partnership chord data
  partnership_chord_data_dupli <- partnership_chord_data |> 
    select(second_bat = first_bat, first_bat = second_bat, partnership_runs)
  
  partnership_chord_data_all <- partnership_chord_data |> 
    bind_rows(partnership_chord_data_dupli)
  
  partnership_chord_matrix<-as.matrix(as_adjacency_matrix(as_tbl_graph(partnership_chord_data_all),attr = "partnership_runs"))
  
  chord_partnership <-chorddiag(data = partnership_chord_matrix,
                   groupnamePadding = 30,
                   groupPadding = 3,
                   groupColors = c("#ffffe5","#fff7bc","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04"),
                   groupnameFontsize = 13 ,
                   showTicks = FALSE,
                   margin=150,
                   tooltipGroupConnector = "    &#x25B6;    ",
                   chordedgeColor = "#B3B6B7"
  )
  
  output$chord_partnership_graph <- renderChorddiag({chord_partnership})
  
  
})


