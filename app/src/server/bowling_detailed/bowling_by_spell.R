#obtain bowling statistics by spell number

detailed_bowling_spell <- detailed_bowling_import |> 
  #select important columns
  select(Date,
         Bowler,
         `Bowling Spell`,
         Over,
         `Actual Ball`,
         `Legal Ball`,
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
                            TRUE ~ 0),
         count_ball = case_when(`Legal Ball` == "Yes" ~ 1,
                                TRUE ~ 0)) |> 
  group_by(Date,
           Bowler,
           `Bowling Spell`) |> 
  #calculate total runs,wides, wickets, noballs
  summarise(Balls = sum(count_ball, na.rm = TRUE),
            Runs = sum(Runs, na.rm = TRUE),
            Wide = sum(Wide, na.rm = TRUE),
            `No Ball` = sum(`No Ball`, na.rm = TRUE),
            Wicket = sum(Wicket, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(Runs = (Runs + Wide + `No Ball`))


summarised_bowling_spell <- detailed_bowling_spell |> 
  group_by(Bowler,
           `Bowling Spell`) |> 
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
         `Bowling Spell`,
         Overs = overs,
         Runs,
         Wickets = Wicket,
         Wides = Wide,
         `No Balls` = `No Ball`,
         Average,
         `Strike Rate` = strike_rate,
         Economy = economy) |> 
  arrange(Bowler,
          `Bowling Spell`)

output$bowling_spell <- renderReactable({reactable(summarised_bowling_spell,
                                                         filterable = TRUE,
                                                         searchable = TRUE,
                                                         highlight = TRUE,
                                                         striped = TRUE,
                                                         showSortable = TRUE)})