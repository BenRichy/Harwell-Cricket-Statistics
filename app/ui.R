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
            menuItem("Individual Stats", tabName = "individual-stats", icon = icon("person")),
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
                uiOutput("SelectYear_Team"),
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
                uiOutput("SelectYear_Batting"),
                br(),
                tabsetPanel(
                tabPanel("Summary",reactableOutput("batting_summary")),
                tabPanel("Runs By Position/Person",reactableOutput("batting_position_person")),
                tabPanel("Runs By Position",plotlyOutput("batting_position_record")),
                tabPanel("Total Runs Over Time (raw)",plotlyOutput("batting_total_area_raw")),
                tabPanel("Total Runs Over Time (percent)",plotlyOutput("batting_total_area_percent")),
                tabPanel("Distance Run Batting",reactableOutput("batting_distance_run"))
                )
            ),

            # Bowling Stats
            tabItem(
                tabName = "bowling-stats",
                h2("Bowling Stats"),
                br(),
                uiOutput("SelectTeam_Bowling"),
                uiOutput("SelectYear_Bowling"),
                br(),
                tabsetPanel(
                  tabPanel("Summary",reactableOutput("bowling_summary")),
                  tabPanel("Total Wickets Over Time (raw)",plotlyOutput("bowling_total_area_raw")),
                  tabPanel("Total Wickets Over Time (percent)",plotlyOutput("bowling_total_area_percent")),
                  tabPanel("Bowling Figures by End", reactableOutput("bowling_detail_end")),
                  tabPanel("Bowling Figures by Batting Position", reactableOutput("bowling_batting_num")),
                  tabPanel("Bowling Figures by Bowling Spell", reactableOutput("bowling_spell")),
                  tabPanel("Bowling Figures by Over Region", reactableOutput("bowling_over_region"))
                  )
            ),
            
            # Partnership Statistics
            tabItem(
              tabName = "partnership-stats",
              h2("Partnership Stats"),
              br(),
              uiOutput("SelectTeam_Partnership"),
              uiOutput("SelectYear_Partnership"),
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
              uiOutput("SelectTeam_Individual"),
              uiOutput("SelectYear_Individual"),
              br(),
              tabsetPanel(
                tabPanel("Batting - Summary", 
                         div(style = "width: 700px", reactableOutput("individual_batting_summary"))),
                tabPanel("Batting - Game By Game", reactableOutput("individual_batting_by_game")),
                tabPanel("Batting - Manhattan", uiOutput("SelectBattingStat_Individual"), br(), plotlyOutput("individual_batting_plotly")),
                tabPanel("Batting - Dismissals", plotlyOutput("individual_dismissal_pie")),
                tabPanel("Batting - By Position", reactableOutput("individual_batting_summary_position")),
                tabPanel("Batting - By Opposition", reactableOutput("individual_batting_summary_opposition")),
                tabPanel("Batting - % Runs", "In Progress"),
                tabPanel("Bowling - Summary", 
                         div(style = "width: 700px", reactableOutput("individual_bowling_summary"))),
                tabPanel("Bowling - Game By Game", reactableOutput("individual_bowling_by_game")),
                tabPanel("Bowling - Manhattan", uiOutput("SelectBowlingStat_Individual"), br(),plotlyOutput("individual_bowling_plotly")),
                tabPanel("Bowling - Wickets", plotlyOutput("individual_wickets_pie")),
                tabPanel("Bowling - By Opposition", reactableOutput("individual_bowling_summary_opposition")),
                tabPanel("Bowling - % Wickets", "In Progress")
              )
            ),
            

            # Award Stats
            tabItem(
                tabName = "award-stats",
                h2("Award Stats - Under Construction"),
                br(),
                uiOutput("SelectTeam_Awards"),
                br(),
                tabsetPanel(
                  tabPanel("Summary",
                           tabsetPanel(
                             tabPanel("Summary"),
                             tabPanel("Detail"),
                             tabPanel("Visualise")
                           )),
                  tabPanel("Matt Hills Fielding Award",
                           tabsetPanel(
                             tabPanel("Summary"),
                             tabPanel("Detail"),
                             tabPanel("Visualise")
                           )),
                  tabPanel("Champagne Moment",
                           tabsetPanel(
                             tabPanel("Summary"),
                             tabPanel("Detail"),
                             tabPanel("Visualise")
                           )),
                  tabPanel("Dick of the Day",
                           tabsetPanel(
                             tabPanel("Summary"),
                             tabPanel("Detail"),
                             tabPanel("Visualise")
                           )),
                  tabPanel("Man of the Match",
                           tabsetPanel(
                             tabPanel("Summary"),
                             tabPanel("Detail"),
                             tabPanel("Visualise")
                           )),
                  tabPanel("Toby Gallington Fair Play Award",
                           tabsetPanel(
                             tabPanel("Summary"),
                             tabPanel("Detail"),
                             tabPanel("Visualise")
                           )))
            )
        )
    )
)
