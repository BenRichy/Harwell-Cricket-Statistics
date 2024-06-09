#library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(here)
library(RSQLite)
#library(DT)
library(reactable)
library(stringi)
library(lubridate)
library(shinydashboard)
library(DBI)
library(plotly)
#library(gganimate)
library(igraph)
library(tidygraph)
library(chorddiag)

# connect to the detailed database
if(file.exists("app") == TRUE){
  db_fp <- "app/data/cricket_detail_database.sqlite"
} else{
  db_fp <- "data/cricket_detail_database.sqlite"
}


connect <- function(..., con = db_fp) {
  con <- DBI::dbConnect(RSQLite::SQLite(), con, extend_types = TRUE)
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

batting_match_detail <- batting_summary |> 
  mutate(
    strike_rate = round((runs / balls) * 100,0),
    percent_runs_boundaries = round(((fours * 4 + sixes * 6) / runs)*100,2)
  ) |> 
  arrange(desc(strike_rate))
