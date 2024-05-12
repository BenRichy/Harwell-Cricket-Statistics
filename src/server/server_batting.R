
# overall Batting Stats
    batting_all_detail <- DBI::dbGetQuery(
        conn,
        "SELECT
        r.opposition,
        r.match_date,
        r.league_name,
        b.*
        FROM batting b
        left join results r on b.match_id = r.id;"
    )

    # cut down stats for visualising
    batting_summary <- DBI::dbGetQuery(
        conn,
        "SELECT
    r.opposition,
    r.match_date,
    r.league_name,
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
