#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(htmltools)


# Define UI for application that draws a histogram 
navbarPage(
  title = "NBA Player Data",
  tabPanel ("About",
            titlePanel( "NBA Player and team data" ),
            HTML ("<p>This app uses the data uploaded to the Kaggle website. It includes biometric, biographic and basic box score stats from 1996 to 2022 season: https://www.kaggle.com/datasets/justinas/nba-players-data",
                  "<p>Using this data, we explored the relationship between basketball player's key performance index and their biometric/biographic data, and will use players' previous season's KPI/biometric/biographic data to predict their performances in the future."
            )
            ),
  
  tabPanel ("Data Exploration",
 
    # Application title
  titlePanel("NBA Player and team Data"),
  
  # Sidebar with a slider input for which season to choose to display
  sidebarLayout(
    sidebarPanel(
      sliderInput("Season",
                  "Which season would you want to look at?",
                  min = 1996,
                  max = 2022,
                  value = 1996),
    #Get another input widget, radiobuttons, to select which data to show on the density plot. I want people to select from height, weight, gameplayed, pts, reb, ast. This could be combined with the first input slidebar.
    radioButtons (
      inputId = "properties",
      label = "What are you interested in?",
      choices = c ("player's height" = "player_height",
                   "player's weight" = "player_weight",
                   "total games played" = "gp", 
                   "points per game" = "pts",
                   "rebounds per game" = "reb", 
                   "assists per game" = "ast")
      )
    ),
    
    # Show the two scatterplots and densityplots!
    mainPanel(
      plotOutput("scatterplot1"),
      plotOutput ("scatterplot2"),
      plotOutput ("densityplot")
    )
  )
),

tabPanel ("Modelling")
)

