#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(worldfootballR)
library(stringr)
library(dplyr)

end_season_summary <- read_csv("seasonSummary.csv")
ui <- fluidPage(
  
  shinythemes::themeSelector(),

    
    titlePanel("Prototype"),
    
    sidebarPanel(
      selectInput(inputId = "Team1",
                  label = "Choose your team:", 
                  choices = c("Tottenham" = "Tottenham",
                    "Arsenal" = "Arsenal",
                    "Manchester City" = "Manchester City",
                    "Manchester United" = "Manchester Utd",
                    "Chelsea" = "Chelsea",
                    "Liverpool" = "Liverpool")),
      
      selectInput(inputId = "Team2",
                  label = "Choose your second team:", 
                  choices = c("Tottenham" = "Tottenham",
                    "Arsenal" = "Arsenal",
                    "Manchester City" = "Manchester City",
                    "Manchester United" = "Manchester Utd",
                    "Chelsea" = "Chelsea",
                    "Liverpool" = "Liverpool")),
      
      selectInput(inputId = "Statistic",
                  label = "Choose a stat to compare:",
                  choices = c("Standing" = "Rk",
                              "Wins" = "W",
                              "Loses" = "L",
                              "Draws" = "D",
                              "Goals For" = "GF",
                              "Goals Against" = "GA",
                              "Goal Difference" = "GD",
                              "Points" = "Pts",
                              "Match Attendance" = "Attendance",
                              "Expected Goals" = "xG"))
    ),
    
    mainPanel(
      plotOutput("Table")
    ),
)

server <- function(input, output) {

  output$Table <- renderPlot({
    team1 <- end_season_summary %>% group_by(Squad) %>% filter(str_detect(Squad, input$Team1))
    team2 <- end_season_summary %>% group_by(Squad) %>% filter(str_detect(Squad, input$Team2))
    toPlot<- team1 %>% rbind(team2)
    toPlot %>% ggplot( aes_string(x = "Season_End_Year", y = input$Statistic, color = "Squad", group = "Squad")) +
      geom_line() + geom_point() + labs(x = "Seasons", color = "Teams") + theme_classic() + scale_x_continuous(n.breaks = 10) + 
      scale_y_continuous(n.breaks = 10) + scale_color_manual(values=c('Blue','Red'))
  })
  
}

shinyApp(ui = ui, server = server)
