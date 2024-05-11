# connect to database
connect <- function(..., con = here::here("data/cricket_detail_database.sqlite")) {
    con <-
        DBI::dbConnect(RSQLite::SQLite(), con, extend_types = TRUE)
    con
}
