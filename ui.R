#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


dashboardPage(
    dashboardHeader(title = "Harwell Cricket Statistics"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Team Stats", tabName = "team-stats", icon = icon("cloud")),
            menuItem("Batting Stats", tabName = "batting-stats", icon = icon("map")),
            menuItem("Bowling Stats", tabName = "bowling-stats", icon = icon("ruler")),
            menuItem("Awards", tabName = "award-stats", icon = icon("wind"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(
                tabName = "team-stats",
                h2("Team Stats"),
                br(),
                h3("Test")#,
                #uiOutput("SelectTeam_Team")
            ),

            # Second tab content
            tabItem(
                tabName = "batting-stats",
                h2("Batting Stats"),
                br(),
                uiOutput("SelectTeam"),
                br(),
                tabsetPanel(
                tabPanel("Summary",DTOutput("batting_summary")),
                tabPanel("position per person",DTOutput("batting_position_person")),
                tabPanel("position record",plotlyOutput("batting_position_record")))
            ),

            # Second tab content
            tabItem(
                tabName = "bowling-stats",
                h2("Bowling Stats"),
                br()
            ),

            # Second tab content
            tabItem(
                tabName = "award-stats",
                h2("Award Stats"),
                br()
            )
        )
    )
)
