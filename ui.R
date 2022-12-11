## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(
            sidebarMenu(
              menuItem("System I - Genre", tabName = "system1"),
              menuItem("System II - Collaborative", tabName = "system2")
            )
          ),

          dashboardBody(includeCSS("css/movies.css"),
              tabItems(
                tabItem(tabName = "system1",
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Select your favorite genre to discover movies you might like",
                      br(),
                      selectInput("slct1", "Select your favorite genre", choices = c(
                        "Action", "Adventure", "Animation", "Children's",
                        "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
                        "Film-Noir", "Horror", "Musical", "Mystery", "Romance",
                        "Sci-Fi", "Thriller", "War", "Western"
                      )),
                      withBusyIndicatorUI(
                        actionButton("btn1", "Click here to get your recommendations", class = "btn-warning")
                      ),
                      br(),
                      tableOutput("results1")
                    )
                  )  
                ),
                tabItem(tabName = "system2",
                  fluidRow(
                    box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems",
                            uiOutput("ratings")
                        )
                    )
                  ),
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Step 2: Discover movies you might like",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("btn2", "Click here to get your recommendations", class = "btn-warning")
                      ),
                      br(),
                      tableOutput("results2")
                    )
                  ) 
                )
              )
          )
    )
) 