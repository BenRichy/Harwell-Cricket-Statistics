#load in awards dataset

if(file.exists("app") == TRUE){
  awards_data <- read_csv("app/data/harwell_awards.csv")
} else{
  awards_data <- read_csv("data/harwell_awards.csv")
}

#get match IDs
match_ids <- DBI::dbGetQuery(
  conn,
  "SELECT
  id,
  match_date,
  opposition,
  league_name
  FROM results r;"
)

#join match ids onto awards data
awards_data <- awards_data |> 
  left_join(match_ids,
            by = c("match_id" = "id"))


