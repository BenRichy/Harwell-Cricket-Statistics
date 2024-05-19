#core, cross-cutting server related objects

#drop down to select the team scope
#multiple select

output$SelectTeam <- renderUI({
  selectInput(
    "team_scope",
    tags$h4("Select Team(s)",align="center"),
    choices = league_names,
    selected = league_names,
    multiple = TRUE
  )
  
})

