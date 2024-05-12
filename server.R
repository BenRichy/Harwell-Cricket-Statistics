shinyServer(function(input, output) {
    conn <- connect()

    

    # cut down stats for visualising
    batting_summary <- DBI::dbGetQuery(
        conn,
        "SELECT
    r.opposition,
    r.match_date,
    position,
    batsman_name,
    batsman_id,
    bd.clean_dismissal,
    runs,
    balls,
    fours,
    sixes
    FROM batting b
    left join batting_dismissals bd on b.how_out = bd.pc_dismissal
    left join results r on b.match_id = r.id;"
    )

    # overall Bowling Stats
    bowling_all_detail <- DBI::dbGetQuery(
        conn,
        "SELECT
        r.opposition,
        r.match_date,
        b.*
        FROM bowling b
        left join results r on b.match_id = r.id;"
    )

    # cut down stats for visualising
    bowling_summary <- DBI::dbGetQuery(
        conn,
        "SELECT
    r.opposition,
    r.match_date,
    bowler_name,
    bowler_id,
    ball_count,
    maidens,
    runs,
    wickets,
    wides,
    no_balls
    FROM bowling b
    left join results r on b.match_id = r.id;"
    )
})
