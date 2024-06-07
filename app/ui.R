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
            menuItem("Home Page", tabName = "team-stats", icon = icon("house")),
            menuItem("Batting Stats", tabName = "batting-stats", icon = icon("baseball-bat-ball")),
            menuItem("Bowling Stats", tabName = "bowling-stats", icon = icon("bowling-ball")),
            menuItem("Partnership Stats", tabName = "partnership-stats", icon = icon("yin-yang")),
            menuItem("Individual Stats", tabName = "individual-stats"),
            menuItem("Awards", tabName = "award-stats", icon = icon("trophy"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(
                tabName = "team-stats",
                h2("Welcome to the Home of Harwell Cricket Statistics!"),
                br(),
                h3("Please check out the other pages to view statistics"),
                uiOutput("SelectTeam_Team"),
                br(),
                div(style = "width: 700px",
                    reactableOutput("team_summary"))
            ),

            # Batting Stats
            tabItem(
                tabName = "batting-stats",
                h2("Batting Stats"),
                br(),
                uiOutput("SelectTeam_Batting"),
                br(),
                tabsetPanel(
                tabPanel("Summary",reactableOutput("batting_summary")),
                tabPanel("Runs By Position/Person",reactableOutput("batting_position_person")),
                tabPanel("Runs By Position",plotlyOutput("batting_position_record")),
                tabPanel("Total Runs Over Time (raw)",plotlyOutput("batting_total_area_raw")),
                tabPanel("Total Runs Over Time (percent)",plotlyOutput("batting_total_area_percent"))
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
                  tabPanel("Summary",reactableOutput("bowling_summary")),
                  tabPanel("Total Wickets Over Time (raw)",plotlyOutput("bowling_total_area_raw")),
                  tabPanel("Total Wickets Over Time (percent)",plotlyOutput("bowling_total_area_percent"))
                  )
            ),
            
            # Partnership Statistics
            tabItem(
              tabName = "partnership-stats",
              h2("Partnership Stats"),
              br(),
              uiOutput("SelectTeam_Partnership"),
              br(),
              tabsetPanel(
                tabPanel("Chord Diagram", chorddiagOutput("chord_partnership_graph", height = "900px")),
                tabPanel("Highest Partnerships", plotlyOutput("partnership_position_record"))
                )
            ),
            # Individual Stats
            tabItem(
              tabName = "individual-stats",
              h2("Individual Stats"),
              br(),
              uiOutput("SelectPlayer_Individual"),
              h3("Statistics under construction")
            ),
            

            # Award Stats
            tabItem(
                tabName = "award-stats",
                h2("Award Stats"),
                br(),
                h3("Statistics under construction")
            )
        )
    )
)
