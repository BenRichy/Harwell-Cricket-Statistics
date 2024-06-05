#batting_summary_position <- read_csv("data/db_dump/batting_summary_position.csv")

batting_summary_position <- DBI::dbGetQuery(
  conn,
  "SELECT
    r.league_name,
    position,
    batsman_name
  FROM batting b
    left join results r on b.match_id = r.id;"
)


#get partnership data
#partnership_summary <- read_csv("data/db_dump/partnership_summary.csv")
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
    coord_flip() +
    #label axes
    labs(y = "Runs in Partnership",
         x = "Wicket Number",
         title = "Highest Partnerships by Wicket Number")
  
  graph_partnership_position <- ggplotly(graph_partnership_position, tooltip = c("text"))
  output$partnership_position_record <- renderPlotly({graph_partnership_position})

# chord graph of partnerships
  #get average position of players
  average_position <- batting_summary_position |> 
    filter(league_name %in% input_team_scope) |>
    select(batsman_name,
           position) |> 
    summarise(avg_position = mean(position),
              .by = batsman_name)
  
  partnership_summary <- partnership_summary|> 
    #join on average position data
    left_join(average_position,
              by = c("batsman_out_name" = "batsman_name")) |> 
    rename(batsman_out_position = avg_position) |> 
    left_join(average_position,
              by = c("batsman_in_name" = "batsman_name")) |> 
    rename(batsman_in_position = avg_position)
  
  
  partnership_chord_data <- partnership_summary |> 
    filter(league_name %in% input_team_scope) |>
    mutate(first_bat = case_when(batsman_out_name < batsman_in_name ~ batsman_out_name,
                                 TRUE ~ batsman_in_name),
           second_bat = case_when(batsman_out_name < batsman_in_name ~ batsman_in_name,
                                  TRUE ~ batsman_out_name),
           first_bat_position = case_when(batsman_out_name < batsman_in_name ~ batsman_out_position,
                                 TRUE ~ batsman_in_position),
           second_bat_position = case_when(batsman_out_name < batsman_in_name ~ batsman_in_position,
                                  TRUE ~ batsman_out_position)) |> 
    select(first_bat, second_bat, first_bat_position, second_bat_position, partnership_runs) |> 
    filter(partnership_runs > 0) |> 
    arrange(first_bat_position, second_bat_position)
  
  #duplicate partnership chord data
  partnership_chord_data_dupli <- partnership_chord_data |> 
    select(second_bat = first_bat, 
           first_bat = second_bat, 
           second_bat_position = first_bat_position, 
           first_bat_position = second_bat_position, 
           partnership_runs)
  
  partnership_chord_data_all <- partnership_chord_data |> 
    bind_rows(partnership_chord_data_dupli)
  
  partnership_chord_matrix<-as.matrix(as_adjacency_matrix(as_tbl_graph(partnership_chord_data_all),attr = "partnership_runs"))
  
  chord_partnership <-chorddiag(data = partnership_chord_matrix,
                   groupnamePadding = 30,
                   groupPadding = 3,
                   groupColors = c("#058814", "#D3CA15","#229114", "#B6C115","#409B14","#98B715","#5DA414","#7BAE15"),
                   groupnameFontsize = 13 ,
                   showTicks = FALSE,
                   margin=150,
                   tooltipGroupConnector = "    &#x25B6;    ",
                   chordedgeColor = "#B3B6B7"
  )
  
  output$chord_partnership_graph <- renderChorddiag({chord_partnership})
  
  
})


