#get just the home data for stats from different ends
detailed_bowling_home <- detailed_bowling_import |> 
  filter(Venue == "Harwell")

detailed_bowling_home <- detailed_bowling_home |> 
  #select important columns
  select(Date,
         Bowler,
         `Ground End`,
         Over,
         Ball,
         Runs,
         `Bowler Extra Runs`,
         `Extra`,
         Wicket) |> 
  #get the number of wides in a column
  mutate(Wide = case_when(Extra == "Wide" ~ `Bowler Extra Runs`,
                          TRUE ~ 0),
         #get the number of no balls in a column
         `No Ball` = case_when(Extra == "No Ball" ~ `Bowler Extra Runs`,
                               TRUE ~ 0),
         Wicket = case_when(!is.na(Wicket) ~ 1,
                            TRUE ~ 0)) |> 
  group_by(Date,
           Bowler,
           `Ground End`,
           Over) |> 
  #calculate total runs,wides, wickets, noballs
  summarise(ball_end = max(Ball),
            Runs = sum(Runs, na.rm = TRUE),
            Wide = sum(Wide, na.rm = TRUE),
            `No Ball` = sum(`No Ball`, na.rm = TRUE),
            Wicket = sum(Wicket, na.rm = TRUE)) |> 
  #calculate total overs and maidens
  mutate(Total_overs = n()) |> 
  ungroup() |> 
  mutate(Runs = (Runs + Wide + `No Ball`),
         Maiden = case_when(Runs == 0 ~ 1, TRUE ~ 0)) |> 
  group_by(Date,
           Bowler,
           `Ground End`,
           Total_overs) |> 
  mutate(ball_end = case_when(ball_end < 6 ~ ball_end,
                              TRUE ~ 0)) |> 
  summarise(ball_end = sum(ball_end, na.rm = TRUE),
            Maiden = sum(Maiden, na.rm = TRUE),
            Runs = sum(Runs, na.rm = TRUE),
            Wicket = sum(Wicket, na.rm = TRUE),
            Wide = sum(Wide, na.rm = TRUE),
            `No Ball` = sum(`No Ball`, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(Total_balls = case_when(ball_end != 0 ~ (Total_overs * 6) - (6-ball_end),
                                 TRUE ~ (Total_overs * 6)))


detailed_bowling_home_group <- detailed_bowling_home_select |> 
  group_by(Bowler,
           `Ground End`) |> 
  summarise(Total_balls = sum(Total_balls, na.rm = TRUE),
            complete_overs = floor(Total_balls / 6),
            residual_balls = Total_balls - (6 * complete_overs),
            overs = format(as.numeric(paste0(complete_overs, ".", residual_balls)),nsmall=1),
            Maiden = sum(Maiden, na.rm = TRUE),
            Runs = sum(Runs, na.rm = TRUE),
            Wicket = sum(Wicket, na.rm = TRUE),
            Wide = sum(Wide, na.rm = TRUE),
            `No Ball` = sum(`No Ball`, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(Average = round(Runs/Wicket,2),
         strike_rate = round(Total_balls / Wicket,2),
         economy = round(Runs / (complete_overs + (residual_balls / 6)),2)) |> 
  select(Bowler,
         `Ground End`,
         Overs = overs,
         Maidens = Maiden,
         Runs,
         Wickets = Wicket,
         Wides = Wide,
         `No Balls` = `No Ball`,
         Average,
         `Strike Rate` = strike_rate,
         Economy = economy)


output$bowling_detail_end <- renderReactable({reactable(detailed_bowling_home_group,
                                                        filterable = TRUE,
                                                        searchable = TRUE,
                                                        highlight = TRUE,
                                                        striped = TRUE,
                                                        showSortable = TRUE)})
