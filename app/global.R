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
connect <- function(..., con = here::here("app/data/cricket_detail_database.sqlite")) {
    con <- DBI::dbConnect(RSQLite::SQLite(), con, extend_types = TRUE)
    con
}
