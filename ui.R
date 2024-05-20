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
            menuItem("Partnership Stats", tabName = "partnership-stats", icon = icon("ruler")),
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

            # Batting Stats
            tabItem(
                tabName = "batting-stats",
                h2("Batting Stats"),
                br(),
                uiOutput("SelectTeam_Batting"),
                br(),
                tabsetPanel(
                tabPanel("Summary",DTOutput("batting_summary")),
                tabPanel("Runs By Position/Person",DTOutput("batting_position_person")),
                tabPanel("Runs By Position",plotlyOutput("batting_position_record")),
                tabPanel("Total Runs Over Time",plotlyOutput("batting_total_area"))
                )
            ),

            # Bowling Stats
            tabItem(
                tabName = "bowling-stats",
                h2("Bowling Stats"),
                br(),
                uiOutput("SelectTeam_Bowling"),
                br(),
                tabsetPanel(
                  tabPanel("Summary",DTOutput("bowling_summary")))
            ),
            
            # Partnership Statistics
            tabItem(
              tabName = "partnership-stats",
              h2("Partnership Stats"),
              br(),
              uiOutput("SelectTeam_Partnership"),
              br(),
              tabsetPanel(
                tabPanel("Highest Partnerships", plotlyOutput("partnership_position_record")))
            ),

            # Award Stats
            tabItem(
                tabName = "award-stats",
                h2("Award Stats"),
                br()
            )
        )
    )
)
