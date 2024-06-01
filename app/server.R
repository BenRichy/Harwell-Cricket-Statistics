shinyServer(function(input, output) {
    #conn <- connect()

    #get the league names for filtering purposes
    league_names <- read_csv("data/db_dump/league_names.csv")
    
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
    
    
    # source("src/server/server_team.R",local = TRUE)
    # source("src/server/server_batting.R",local = TRUE)
    # source("src/server/server_bowling.R",local = TRUE)
    # source("src/server/server_partnership.R",local = TRUE)
    

    
})
