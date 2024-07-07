shinyServer(function(input, output) {
    conn <- connect()

    #get the league names for filtering purposes
    # league_names <- read_csv("data/db_dump/league_names.csv")
    
    league_names <- DBI::dbGetQuery(
      conn,
      "SELECT distinct league_name
      FROM results;")
    
    league_names <- as.vector(unlist(league_names))
    
    #get years for filtering
    season_years <- DBI::dbGetQuery(
      conn,
      "SELECT distinct season
      FROM results;")
    
    season_years <- as.vector(unlist(season_years))
    
    #get list of player names
    player_names <- DBI::dbGetQuery(
      conn,
      "SELECT distinct batsman_name player
      FROM batting
      order by player;")
    
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
    
    output$SelectYear_Team <- renderUI({
      selectInput(
        "year_scope_team",
        tags$h4("Select Year(s)",align="center"),
        choices = season_years,
        selected = season_years,
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
    
    output$SelectYear_Batting <- renderUI({
      selectInput(
        "year_scope_batting",
        tags$h4("Select Year(s)",align="center"),
        choices = season_years,
        selected = season_years,
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
    
    output$SelectYear_Bowling <- renderUI({
      selectInput(
        "year_scope_bowling",
        tags$h4("Select Year(s)",align="center"),
        choices = season_years,
        selected = season_years,
        multiple = TRUE
      )
    })
    
    output$SelectTeam_Partnership <- renderUI({selectInput(
      "team_scope_partnership",
      tags$h4("Select Team(s)",align="center"),
      choices = league_names,
      selected = league_names,
      multiple = TRUE
    )})
    
    output$SelectYear_Partnership <- renderUI({
      selectInput(
        "year_scope_partnership",
        tags$h4("Select Year(s)",align="center"),
        choices = season_years,
        selected = season_years,
        multiple = TRUE
      )
    })
    
    output$SelectPlayer_Individual <- renderUI({selectInput(
      "player_scope_individual",
      tags$h4("Select Player",align="center"),
      choices = player_names
    )})
    
    output$SelectTeam_Individual <- renderUI({
      selectInput(
        "team_scope_individual",
        tags$h4("Select Team(s)",align="center"),
        choices = league_names,
        selected = league_names,
        multiple = TRUE
      )
    })
    
    output$SelectYear_Individual <- renderUI({
      selectInput(
        "year_scope_individual",
        tags$h4("Select Year(s)",align="center"),
        choices = season_years,
        selected = season_years,
        multiple = TRUE
      )
    })
    
    output$SelectBowlingStat_Individual <- renderUI({
      selectInput(
        "bowling_stat_scope_individual",
        tags$h4("Select Cumulative Stat",align="center"),
        choices = c("Wickets","Average","Strike Rate","Economy"),
        selected = "Wickets",
        multiple = FALSE
      )
    })
    
    output$SelectBattingStat_Individual <- renderUI({
      selectInput(
        "batting_stat_scope_individual",
        tags$h4("Select Cumulative Stat",align="center"),
        choices = c("Runs","Average","Strike Rate", "Runs per Innings"),
        selected = "Runs",
        multiple = FALSE
      )
    })
    
    output$SelectTeam_Awards <- renderUI({
      selectInput(
        "team_scope_awards",
        tags$h4("Select Team(s)",align="center"),
        choices = league_names,
        selected = league_names,
        multiple = TRUE
      )
    })
    
    source("src/server/server_team.R",local = TRUE)
    source("src/server/server_batting.R",local = TRUE)
    source("src/server/server_bowling.R",local = TRUE)
    source("src/server/server_partnership.R",local = TRUE)
    source("src/server/server_individual.R",local = TRUE)
    source("src/server/server_bowling_detailed.R",local = TRUE)
    source("src/server/server_batting_detailed.R",local = TRUE)
    

    
})
