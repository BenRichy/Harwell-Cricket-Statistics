# calculate the distance travelled by each batter in km

cricket_pitch_meter <- 20.12
diamond_circumference <- 561.6

#get the runs scored when the batter was a striker
batting_runs_striker <- detailed_batting_import |> 
  select(Batter,
         Runs) |> 
  filter(!Runs %in% c(0,4,6)) |> 
  mutate(on_strike = "striker")

#get the runs scored when the batter was a non-striker
batting_runs_non_striker <- detailed_batting_import |> 
  select(Batter = 'Non-Striker',
         Runs) |> 
  filter(!Runs %in% c(0,4,6)) |> 
  mutate(on_strike = "non_striker")

#calculate total distance run
batting_distance_all <- batting_runs_striker |> 
  bind_rows(batting_runs_non_striker) |> 
  mutate(distance_run = Runs * cricket_pitch_meter) |> 
  summarise(distance_run = sum(distance_run, na.rm = TRUE),
            .by = c(Batter, on_strike)) |>  
  pivot_wider(names_from = on_strike,
              values_from = distance_run) |> 
  rowwise() |>  
  mutate(total_distance = sum(striker, non_striker, na.rm = TRUE)) |> 
  mutate(diamond_circumnavigations = round(total_distance/diamond_circumference,2)) |> 
  select(Batter,
         `Distance Travelled as Striker (m)` = striker,
         `Distance Travelled as Non-Striker (m)` = non_striker,
         `Total Distance Travelled (m)` = total_distance,
         `Navigations of Diamond Light Source` = diamond_circumnavigations) |> 
  arrange(desc(`Navigations of Diamond Light Source`))

output$batting_distance_run <- renderReactable({reactable(batting_distance_all,
                                                        filterable = TRUE,
                                                        searchable = TRUE,
                                                        highlight = TRUE,
                                                        striped = TRUE,
                                                        showSortable = TRUE)})  
