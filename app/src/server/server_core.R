#core, cross-cutting server related objects

#drop down to select the team scope
#multiple select

# SelectTeamGeneral <- selectInput(
#   "team_scope",
#   tags$h4("Select Team(s)",align="center"),
#   choices = league_names,
#   selected = league_names,
#   multiple = TRUE
# )

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

