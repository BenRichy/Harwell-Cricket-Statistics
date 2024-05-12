# cut down stats for visualising
bowling_summary <- DBI::dbGetQuery(
    conn,
    "SELECT
    r.opposition,
    r.match_date,
    r.league_name,
    bowler_name,
    ball_count,
    maidens,
    runs,
    wides,
    no_balls,
    wickets
    FROM bowling b
    left join results r on b.match_id = r.id;"
)

# Produce summary table of batting stats
# TODO: allow the user to group/filter by league
bowling_summary_default <- bowling_summary |>
    group_by(bowler_name) |>
    summarise(
        ball_count = sum(ball_count, na.rm = TRUE),
        maidens = sum(maidens, na.rm = TRUE),
        runs = sum(runs, na.rm = TRUE),
        wides = sum(wides, na.rm = TRUE),
        no_balls = sum(no_balls, na.rm = TRUE),
        wickets = sum(wickets, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
        complete_overs = floor(ball_count / 6),
        residual_balls = ball_count - (6 * complete_overs),
        overs = paste0(complete_overs, ".", residual_balls),
        average = runs / wickets,
        strike_rate = ball_count / wickets,
        economy = runs / (complete_overs + (residual_balls / 6))
    )

# cumulative wickets over time
