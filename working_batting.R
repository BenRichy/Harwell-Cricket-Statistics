

# cumulative runs over time - animated bars
#do a cumulative sum of the batting runs
batting_cum_sum <- batting_summary |> 
  select(match_date,
         batsman_name,
         runs) |> 
  ungroup() |> 
  filter(!is.na(runs)) |> 
  group_by(batsman_name) |> 
  mutate(runs_cum_sum = cumsum(runs),
         match_date = as.POSIXct(match_date, format = "%d/%m/%Y")) |> 
  ungroup()

#get unique dates
unique_dates <- batting_cum_sum |> 
  select(match_date_unique = match_date) |> 
  distinct() 

#get the day before the first game
day_before <- unique_dates |> slice_min(unique_dates) - (60*60*24)

#add the day before in
unique_dates <- unique_dates |> 
  rbind(day_before) |> 
  arrange(match_date_unique)

#get unique players
unique_players <- batting_cum_sum |> 
  select(batsman_unique = batsman_name) |> 
  distinct()



batting_cum_sum_all <- unique_dates |>
  cross_join(unique_players) |> 
  left_join(batting_cum_sum,
             by = c("match_date_unique" = "match_date",
                    "batsman_unique" = "batsman_name")) |>
  mutate(runs_cum_sum = case_when(match_date_unique == day_before[[1]] ~ 0,
                                  TRUE ~ runs_cum_sum)) |> 
  group_by(batsman_unique) |> 
  fill(runs_cum_sum, .direction = "downup")


ggplot(batting_cum_sum_all) +
  geom_col(aes(x=runs_cum_sum, y=batsman_unique)) +
  transition_states(match_date_unique,
                    transition_length = 3,
                    state_length = 2) +
  ease_aes()



a <- data.frame(group=c("A","B","C"), values=c(3,2,4), frame=rep('a',3))
b <- data.frame(group=c("A","B","C"), values=c(5,3,7), frame=rep('b',3))
data <- rbind(a,b) 
