# global.R

# load in packages
library(tidyverse)
library(shiny)
library(DT)
library(stringi)
library(lubridate)
library(shinydashboard)
library(DBI)
library(plotly)
library(gganimate)
library(igraph)
library(tidygraph)
library(chorddiag)

# connect to the detailed database
# connect <- function(..., con = here::here("data/cricket_detail_database.sqlite")) {
#     con <-
#         DBI::dbConnect(RSQLite::SQLite(), con, extend_types = TRUE)
#     con
# }
