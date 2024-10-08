#load in awards dataset

if(file.exists("app") == TRUE){
  awards_data <- read_csv("app/data/harwell_awards.csv")
} else{
  awards_data <- read_csv("data/harwell_awards.csv")
}

#get match Dates
match_dates <- DBI::dbGetQuery(
  conn,
  "SELECT
  id,
  match_date,
  opposition,
  league_name
  FROM results r;"
)

#awards mapping
awards_mapping <- tribble(
  ~award_id, ~award_name,
  "mhfa"   , "Matt Hills Fielding Award",
  "cm"     , "Champagne Moment",
  "motm"   , "Man of the Match",
  "dod"    , "Donkey of the Day",
  "tgfp"   , "Toby Gallington Fair Play Award"
)

#filter data based on league selection
observeEvent(c(input$team_scope_awards), {
                 
                 if(is.null(input$team_scope_awards)){
                   input_team_scope <- league_names
                 } else{
                   input_team_scope <- input$team_scope_awards
                 }
                 

#join match ids onto awards data
awards_data_all_detail <- awards_data |> 
  left_join(match_dates,
            by = c("match_date")) |> 
  left_join(awards_mapping,
            by = "award_id") |> 
  select(nominee,
         award_name,
         match_date,
         opposition,
         league_name,
         nomination_summary) |> 
  filter(league_name %in% input_team_scope)

#Create summary dataset of all awards
awards_data_all_summary <- awards_data_all_detail |> 
  count(nominee,
        award_name) |> 
  pivot_wider(names_from = award_name,
              values_from = n) |> 
  #reorder the columns
  select(Person = nominee,
         5,3,4,6,2) |> 
  #replace NA values with 0
  mutate(across(where(is.numeric), ~replace_na(., 0)),
         #calculate the total number of awards
         `Total Awards` = rowSums(across(where(is.numeric)), na.rm=TRUE)) |> 
  arrange(desc(`Total Awards`))

awards_data_all_detail <- awards_data_all_detail |> 
  select(
  "Award Winner" = nominee,
  Award = award_name,
  "Match Date" = match_date,
  Opposition = opposition,
  "League Name" = league_name,
  "Nomination Description" = nomination_summary
)

output$awards_all <- renderReactable({reactable(awards_data_all_summary, details = function(index) {
  awards_data_all_detail_filter <- awards_data_all_detail[awards_data_all_detail$`Award Winner` == awards_data_all_summary$Person[index], ]
  htmltools::div(style = "padding: 1rem",
                 reactable(awards_data_all_detail_filter, outlined = TRUE)
  )
})  
})

})






