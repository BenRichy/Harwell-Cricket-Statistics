# global.R

# load in packages
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
