#server_team.R

#produce a map of where all of the runs have been scored
batting_summary2 <- DBI::dbGetQuery(
  conn,
  "SELECT
    *
    FROM results b;"
)
