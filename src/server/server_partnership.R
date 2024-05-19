partnership_summary <- DBI::dbGetQuery(
    conn,
    "SELECT
    r.opposition,
    r.match_date,
    r.league_name,
    wickets,
    batsman_out_name,
    batsman_in_name,
    partnership_runs
    FROM partnerships p
    left join results r on p.match_id = r.id;"
)

# highest partnership by number

# chord graph of partnerships
