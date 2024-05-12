# cut down stats for visualising
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

# Produce summary table of batting stats
# TODO: allow the user to group/filter by league
batting_summary_default <- batting_summary |>
    group_by(batsman_name) |>
    summarise(
        innings = sum(count_innings, na.rm = TRUE),
        dismissed = sum(count_out, na.rm = TRUE),
        runs = sum(runs, na.rm = TRUE),
        balls_faced = sum(balls, na.rm = TRUE),
        fours = sum(fours, na.rm = TRUE),
        sixes = sum(sixes, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
        runs_per_innings = runs / innings,
        average = runs / dismissed,
        strike_rate = (runs / balls_faced) * 100,
        percent_runs_boundaries = (fours * 4 + sixes * 6) / runs
    )


# runs by position

# cumulative runs over time
