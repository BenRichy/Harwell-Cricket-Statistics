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
  left_join(read_csv(match_date_fp), by = c("Date" = "match_date"))


#only get the data for bowlers where the wagonwheel is available


bowling_individual_wagon <- detailed_bowling_import_individual |> 
  filter(!is.na(FieldX),
         Runs > 0,
         Date == "03/07/2024") |> 
  select(Bowler,
         Runs,
         FieldX,
         FieldY,
         league_name,
         season) |> 
  mutate(OriginX = 180,
         OriginY = 180,
         Runs = as.factor(Runs),
         FieldY = 360 - FieldY) |> 
  filter(Bowler == "Patrick Harland")

cart2polar <- function(x, y) {
  data.frame(r = sqrt(x^2 + y^2), theta = atan2(y, x))
}

a_test <- cart2polar(bowling_individual_wagon$FieldX, bowling_individual_wagon$FieldY) |> 
  mutate(theta_2 = (theta)*(180/pi))

ggplot(bowling_individual_wagon, aes(x = FieldX, y = FieldY, colour = Runs)) +
  geom_point() +
  #coord_radial(start = 0, end = 2 * pi) +
  scale_color_discrete("Paired")
                 

ggplot(a_test, aes(x = r, y = theta)) +
  geom_col() +
  coord_polar()

fig <- plot_ly(
  type = 'scatterpolargl',
  r = a_test$r,
  theta = (a_test$theta)*(180/pi),
  mode = 'markers'
)

fig                 
            