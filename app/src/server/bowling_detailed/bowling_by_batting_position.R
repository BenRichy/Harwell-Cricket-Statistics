#obtain bowling statistics for if the batter is top, middle, or lower order
#top order 1-3
#middle order 4-8
#lower order 9-11

detailed_bowling_batting_num <- detailed_bowling_import |> 
  #select important columns
  select(Date,
         Bowler,
         `Batting Position`,
         Over,
         Ball,
         `Legal Ball`,
         Runs,
         `Bowler Extra Runs`,
         `Extra`,
         Wicket) |> 
  mutate(batting_group = case_when(`Batting Position` %in% c(1:3) ~ "Top Order",
                                   `Batting Position` %in% c(4:8) ~ "Middle Order",
                                   TRUE ~ "Lower Order"),
         batting_group = as.factor(batting_group)) |> 
  #get the number of wides in a column
  mutate(Wide = case_when(Extra == "Wide" ~ `Bowler Extra Runs`,
                          TRUE ~ 0),
         #get the number of no balls in a column
         `No Ball` = case_when(Extra == "No Ball" ~ `Bowler Extra Runs`,
                               TRUE ~ 0),
         Wicket = case_when(!is.na(Wicket) ~ 1,
                            TRUE ~ 0),
         count_ball = case_when(`Legal Ball` == "Yes" ~ 1,
                                TRUE ~ 0)) |> 
  group_by(Date,
           Bowler,
           batting_group)|> 
  #calculate total runs,wides, wickets, noballs
  summarise(Balls = sum(count_ball, na.rm = TRUE),
            Runs = sum(Runs, na.rm = TRUE),
            Wide = sum(Wide, na.rm = TRUE),
            `No Ball` = sum(`No Ball`, na.rm = TRUE),
            Wicket = sum(Wicket, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(Runs = (Runs + Wide + `No Ball`))


summarised_bowling_batting_num <- detailed_bowling_batting_num |> 
  group_by(Bowler,
           batting_group) |> 
  summarise(Balls = sum(Balls, na.rm = TRUE),
            Runs = sum(Runs, na.rm = TRUE),
            Wide = sum(Wide, na.rm = TRUE),
            `No Ball` = sum(`No Ball`, na.rm = TRUE),
            Wicket = sum(Wicket, na.rm = TRUE),
            complete_overs = floor(Balls / 6),
            residual_balls = Balls - (6 * complete_overs),
            overs = format(as.numeric(paste0(complete_overs, ".", residual_balls)),nsmall=1)) |> 
  ungroup() |> 
  mutate(Average = round(Runs/Wicket,2),
         strike_rate = round(Balls / Wicket,2),
         economy = round(Runs / (complete_overs + (residual_balls / 6)),2)) |> 
  select(Bowler,
         `Batting Group` = batting_group,
         Overs = overs,
         Runs,
         Wickets = Wicket,
         Wides = Wide,
         `No Balls` = `No Ball`,
         Average,
         `Strike Rate` = strike_rate,
         Economy = economy) |> 
  arrange(Bowler,
          desc(`Batting Group`))

output$bowling_batting_num <- renderReactable({reactable(summarised_bowling_batting_num,
                                                         filterable = TRUE,
                                                         searchable = TRUE,
                                                         highlight = TRUE,
                                                         striped = TRUE,
                                                         showSortable = TRUE)})