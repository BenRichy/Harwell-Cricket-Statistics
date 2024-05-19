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
  # Create an edge list: a list of connections between 10 origin nodes, and 10 destination nodes:
  origin <- partnership_summary$batsman_out_name
  destination <- partnership_summary$batsman_in_name
  data <- data.frame(origin, destination)
  
  # Transform input data in a adjacency matrix
  adjacencyData <- with(data, table(origin, destination))
  
  # Charge the circlize library
  library(circlize)
  library(igraph)
  library(tidygraph)
  library(chorddiag)
  
  
  # Make the circular plot
  chordDiagram(adjacencyData, transparency = 0.5)
  
  mig_data_filter <- partnership_summary |> 
    mutate(first_bat = case_when(batsman_out_name < batsman_in_name ~ batsman_out_name,
                                 TRUE ~ batsman_in_name),
           second_bat = case_when(batsman_out_name < batsman_in_name ~ batsman_in_name,
                                 TRUE ~ batsman_out_name)) |> 
    select(first_bat, second_bat, partnership_runs) |> 
    filter(partnership_runs > 0) |> 
    arrange(first_bat, second_bat)
  
  mig_data_filter<-as.matrix(as_adjacency_matrix(as_tbl_graph(mig_data_filter),attr = "partnership_runs", type = "upper"))
  
  mig_data_filter[mig_data_filter == 0] <- NA
    
  chordDiagramFromDataFrame(mig_data_filter)
  
  chord<-chorddiag(data = mig_data_filter,
                   type = "bipartite",
                   groupnamePadding = 30,
                   groupPadding = 3,
                   groupColors = c("#ffffe5","#fff7bc","#fee391","#fec44f","#fe9929","#ec7014","#cc4c02","#8c2d04"),
                   groupnameFontsize = 13 ,
                   showTicks = FALSE,
                   margin=150,
                   tooltipGroupConnector = "    &#x25B6;    ",
                   chordedgeColor = "#B3B6B7"
  )
  
  chord<-chorddiag(data = mig_data_filter)
  chord
  
})


titanic_tbl <- tibble::as_tibble(Titanic)
titanic_tbl <- titanic_tbl %>%
  mutate(across(where(is.character), as.factor))
by_class_survival <- titanic_tbl %>%
  group_by(Class, Survived) %>%
  summarise(Count = sum(n)) %>% 
  ungroup()
titanic.mat <- matrix(by_class_survival$Count, nrow = 4, ncol = 2, byrow = TRUE)
dimnames(titanic.mat ) <- list(Class = levels(titanic_tbl$Class),
                               Survival = levels(titanic_tbl$Survived))

groupColors <- c("#2171b5", "#6baed6", "#bdd7e7", "#bababa", "#d7191c", "#1a9641")
chorddiag(titanic.mat, type = "bipartite", 
          groupColors = groupColors,
          tickInterval = 50)
