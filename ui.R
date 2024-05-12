#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


dashboardPage(
    dashboardHeader(title = "AQ Git Management"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Inventories", tabName = "inventories", icon = icon("cloud")),
            menuItem("GIS", tabName = "gis", icon = icon("map")),
            menuItem("Measurements", tabName = "measurements", icon = icon("ruler")),
            menuItem("Modelling", tabName = "modelling", icon = icon("wind"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "batting",
                    h2("Batting Stats"),
                    br()
            ),
            
            # Second tab content
            tabItem(tabName = "bowling",
                    h2("Bowling Stats"),
                    br()
            ),
            
            # Second tab content
            tabItem(tabName = "partnerships",
                    h2("Partnership Stats"),
                    br()
            ),
            
            # Second tab content
            tabItem(tabName = "awards",
                    h2("Award Stats"),
                    br()
            )
        )
    )
)