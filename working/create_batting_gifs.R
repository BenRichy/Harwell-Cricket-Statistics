# connect to the detailed database
connect <- function(..., con = here::here("data/cricket_detail_database.sqlite")) {
  con <-
    DBI::dbConnect(RSQLite::SQLite(), con, extend_types = TRUE)
  con
}

conn <- connect()

batting_summary <- DBI::dbGetQuery(
  conn,
  "SELECT
    r.opposition,
    r.match_date,
    r.league_name,
    position,
    batsman_name,
    bd.clean_dismissal,
    bd.count_out,
    bd.count_innings,
    runs,
    balls,
    fours,
    sixes
    FROM batting b
    left join batting_dismissals bd on b.how_out = bd.pc_dismissal
    left join results r on b.match_id = r.id;"
)

all_leagues <- c(unique(batting_summary$league_name))

league_groups <- tibble(league_name =  c(list(all_leagues),
                                         all_leagues))


for (i in 1:nrow(league_groups)) {
  
  file_name <- str_replace_all(toString(str_replace_all(league_groups[[1]][[i]]," ","_")),", ","_")

# cumulative runs over time - animated bars
#do a cumulative sum of the batting runs
batting_cum_sum <- batting_summary |>
  filter(league_name %in% league_groups[[1]][[i]]) |> 
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


#get a complete dataset of dates and players
batting_cum_sum_all <- unique_dates |>
  cross_join(unique_players) |> 
  #join on known runs
  left_join(batting_cum_sum,
            by = c("match_date_unique" = "match_date",
                   "batsman_unique" = "batsman_name")) |>
  #set the first week runs to 0
  mutate(runs_cum_sum = case_when(match_date_unique == day_before[[1]] ~ 0,
                                  TRUE ~ runs_cum_sum)) |> 
  #fill in the weeks where someone didn't play
  group_by(batsman_unique) |> 
  fill(runs_cum_sum, .direction = "downup") |> 
  ungroup() |> 
  arrange(match_date_unique, desc(runs_cum_sum)) |> 
  group_by(match_date_unique) |> 
  #rank the players by number of runs
  mutate(Rank = rank(runs_cum_sum, ties.method = "first")) |> 
  arrange(desc(Rank)) |> 
  ungroup() 


staticplot = ggplot(batting_cum_sum_all, aes(Rank, group = batsman_unique, 
                                             fill = as.factor(batsman_unique), color = as.factor(batsman_unique))) +
  geom_tile(aes(y = runs_cum_sum/2,
                height = runs_cum_sum,
                width = 0.9), alpha = 0.8, color = NA) +
  theme(legend.position = "none") +
  coord_flip(clip = "off", expand = FALSE) +
  geom_text(aes(y = 0, label = paste(batsman_unique, " ")), vjust = 0.2, hjust = 1)

#staticplot + gganimate::transition_time(match_date_unique)

anim = staticplot + transition_states(match_date_unique,
                                      transition_length = 50,
                                      state_length = 10) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Runs as of : {closest_state}',  
       subtitle  =  "Leading Run Scorers over Time",
       caption  = "Harwell Cricket - All runs scored")

#anim

animate(anim, nrow(unique_dates*50), fps = 15,  width = 1200, height = 1000, 
        renderer = gifski_renderer(paste0("plots/",file_name,".gif")))

}
