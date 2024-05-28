shinyServer(function(input, output) {
    conn <- connect()

    #get the league names for filtering purposes
    league_names <- DBI::dbGetQuery(
      conn,
      "SELECT distinct league_name
      FROM results;")
    
    league_names <- as.vector(unlist(league_names))
    
    #drop down to select the team scope
    #multiple select
    
    output$SelectTeam_Team <- renderUI({
      selectInput(
        "team_scope_team",
        tags$h4("Select Team(s)",align="center"),
        choices = league_names,
        selected = league_names,
        multiple = TRUE
      )
    })
    
    output$SelectTeam_Batting <- renderUI({
      selectInput(
        "team_scope_batting",
        tags$h4("Select Team(s)",align="center"),
        choices = league_names,
        selected = league_names,
        multiple = TRUE
      )
    })
    
    
    output$SelectTeam_Bowling <- renderUI({selectInput(
      "team_scope_bowling",
      tags$h4("Select Team(s)",align="center"),
      choices = league_names,
      selected = league_names,
      multiple = TRUE
    )})
    
    output$SelectTeam_Partnership <- renderUI({selectInput(
      "team_scope_partnership",
      tags$h4("Select Team(s)",align="center"),
      choices = league_names,
      selected = league_names,
      multiple = TRUE
    )})
    
    
    source("src/server/server_team.R",local = TRUE)
    source("src/server/server_batting.R",local = TRUE)
    source("src/server/server_bowling.R",local = TRUE)
    source("src/server/server_partnership.R",local = TRUE)
    

    # # cut down stats for visualising
    # batting_summary <- DBI::dbGetQuery(
    #     conn,
    #     "SELECT
    # r.opposition,
    # r.match_date,
    # position,
    # batsman_name,
    # batsman_id,
    # bd.clean_dismissal,
    # runs,
    # balls,
    # fours,
    # sixes
    # FROM batting b
    # left join batting_dismissals bd on b.how_out = bd.pc_dismissal
    # left join results r on b.match_id = r.id;"
    # )
    # 
    # # overall Bowling Stats
    # bowling_all_detail <- DBI::dbGetQuery(
    #     conn,
    #     "SELECT
    #     r.opposition,
    #     r.match_date,
    #     b.*
    #     FROM bowling b
    #     left join results r on b.match_id = r.id;"
    # )
    # 
    # # cut down stats for visualising
    # bowling_summary <- DBI::dbGetQuery(
    #     conn,
    #     "SELECT
    # r.opposition,
    # r.match_date,
    # bowler_name,
    # bowler_id,
    # ball_count,
    # maidens,
    # runs,
    # wickets,
    # wides,
    # no_balls
    # FROM bowling b
    # left join results r on b.match_id = r.id;"
    # )
})
