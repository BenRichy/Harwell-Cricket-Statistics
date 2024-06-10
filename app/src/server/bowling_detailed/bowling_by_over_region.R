#bowling stats by group of overs bowled in
#group overs into every 5 overs, e.g. 1-5, 6-10

detailed_bowling_over_region <- detailed_bowling_import |> 
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
  mutate(over_region = case_when(Over %in% c(1:5) ~ "1-5",
                                 Over %in% c(6:10) ~ "6-10",
                                 Over %in% c(11:15) ~ "11-15",
                                 Over %in% c(16:20) ~ "16-20",
                                 Over %in% c(21:25) ~ "21-25",
                                 Over %in% c(26:30) ~ "26-30",
                                 Over %in% c(31:35) ~ "31-35",
                                 Over %in% c(36:40) ~ "36-40",
                                 Over %in% c(41:45) ~ "41-45",
                                   TRUE ~ "Other"),
         over_region = factor(over_region,
                                 levels = c("1-5","6-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","Other"))) |> 
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
           over_region) |> 
  #calculate total runs,wides, wickets, noballs
  summarise(Balls = sum(count_ball, na.rm = TRUE),
            Runs = sum(Runs, na.rm = TRUE),
            Wide = sum(Wide, na.rm = TRUE),
            `No Ball` = sum(`No Ball`, na.rm = TRUE),
            Wicket = sum(Wicket, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(Runs = (Runs + Wide + `No Ball`))


summarised_bowling_over_region <- detailed_bowling_over_region |> 
  group_by(Bowler,
           over_region) |> 
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
         `Over Region`= over_region,
         Overs = overs,
         Runs,
         Wickets = Wicket,
         Wides = Wide,
         `No Balls` = `No Ball`,
         Average,
         `Strike Rate` = strike_rate,
         Economy = economy) |> 
  arrange(Bowler,
          `Over Region`)


output$bowling_over_region <- renderReactable({reactable(summarised_bowling_over_region,
                                                         filterable = TRUE,
                                                         searchable = TRUE,
                                                         highlight = TRUE,
                                                         striped = TRUE,
                                                         showSortable = TRUE)})