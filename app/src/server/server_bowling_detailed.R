# connect to the detailed database
if(file.exists("app") == TRUE){
  bowling_detail_fp <- "app/data/ball-data-dump.csv"
} else{
  bowling_detail_fp <- "data/ball-data-dump.csv"
}

#get the bowling data only
detailed_bowling_import <- read_csv(bowling_detail_fp) |> 
  filter(grepl("Harwell.*",`Bowling Team`))

source("src/server/bowling_detailed/bowling_by_end.R",local = TRUE)
source("src/server/bowling_detailed/bowling_by_batting_position.R",local = TRUE)
source("src/server/bowling_detailed/bowling_by_spell.R",local = TRUE)
source("src/server/bowling_detailed/bowling_by_over_region.R",local = TRUE)

