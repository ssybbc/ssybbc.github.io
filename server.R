#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library (ggplot2)
library (dplyr)

# Getting the data set ready to be used by the app

nbaplayerdata<-read.csv("C:\\NCSU\\Statistics\\ST558\\Final Project\\all_seasons.csv")
nbaplayerdata<-mutate (nbaplayerdata, seasonshort = substring (season, 1,4))
nbaplayerdata$age_group <- as.factor (ifelse(nbaplayerdata$age<24, "19-23",
                                             ifelse (nbaplayerdata$age<29, "24-28",
                                                     ifelse (nbaplayerdata$age<34, "29-33", "34 and up")
                                             )
)
)


# Define server logic required to draw scatter plot
function(input, output, session) {
  
  output$scatterplot1 <- renderPlot({
    
#filter to get the right dataset we are working on.
    
    nbaplayerdataseason <- filter (nbaplayerdata, seasonshort == input$Season)
    
# draw the scatterplot with player_height and player_weight
    
      ggplot (nbaplayerdataseason, aes(x=player_height, y=player_weight, color = "gold", alpha = 0.85 ))+
      geom_point()+
      facet_wrap(~team_abbreviation)+
      ggtitle ("Player's Height and Weight Data")+
      theme (plot.title = element_text(hjust =0.5))+
      xlab ("player's height")+
      ylab ("player's weight")
  })

# draw another fancier scatter plot based on the same slidebar for season to show
  
  output$scatterplot2 <- renderPlot({
    
#filter to get the right dataset we are working on.
    
nbaplayerdataseason <- filter (nbaplayerdata, seasonshort == input$Season)
    
    ggplot(nbaplayerdataseason, aes(y = pts, color = age_group, size = gp , alpha = 0.75))+
      geom_point (aes (x= reb))+
      geom_point (aes (x= -ast))+
      geom_text (aes (x= reb, y =pts, label = player_name), check_overlap = TRUE, size = 3, color = "black")+
      facet_wrap (~team_abbreviation)+
      ggtitle ("Player's Key Perfomance Indicators")+
      theme (plot.title = element_text(hjust =0.5))+
      xlab ("No. of assists (left) and No. of rebounds (right) per game")+
      ylab ("Players's points per game")
  })
  
# Time to draw the last density plot according to input
  
output$densityplot <- renderPlot({
#filter to get the right data set we are working on.
    
nbaplayerdataseason <- filter (nbaplayerdata, seasonshort == input$Season)

ggplot (nbaplayerdataseason, aes (x = !!sym(input$properties)))+
  geom_density (fill = "skyblue", color = "blue", adjust =0.5)+
  facet_wrap(~team_abbreviation)+
  labs (x=input$properties, y="Density")
  })
  }
