if(file.exists("app") == TRUE){
  bowling_detail_fp <- "app/data/ball-data-dump.csv"
  match_date_fp <- "app/data/match_date_to_league.csv"
} else{
  bowling_detail_fp <- "data/ball-data-dump.csv"
  match_date_fp <- "data/match_date_to_league.csv"
}



#get the batting data only
detailed_batting_import_individual <- read_csv(bowling_detail_fp) |> 
  filter(grepl("Harwell.*",`Batting Team`)) |> 
  left_join(read_csv(match_date_fp), by = c("Date" = "match_date"))


#only get the data for batters where the wagonwheel is available


batting_individual_wagon <- detailed_batting_import_individual |> 
  filter(!is.na(FieldX),
         Runs > 0) |> 
  select(Batter,
         Runs,
         FieldX,
         FieldY,
         league_name,
         season) |> 
  mutate(OriginX = 180,
         OriginY = 180,
         Runs = as.factor(Runs),
         FieldX = FieldX-180,
         FieldY = 180 - FieldY) |> 
  filter(Batter == "Max Schofield")


wagon <- ggplot(batting_individual_wagon, aes(x = FieldX, y = FieldY, colour = Runs)) +
  geom_point() +
  scale_color_discrete("Paired") +
  geom_segment(aes(xend = 0, yend = 0))

ggplotly(wagon)
                 
        
            