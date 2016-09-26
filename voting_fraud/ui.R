#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(rootSolve)
library(ggplot2)

# Define UI for application that draws a histogram

ui <- shinyUI(fluidPage(
  theme = "bootstrap.css",
  # Application title
  titlePanel("Voting optimum"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Pop",
                  "Размер популяции",
                  min = 4,
                  max = 100,
                  value = Population),
      
      sliderInput("Cost",
                  "Стоимость голосования",
                  min=1,
                  max = Win,
                  value=Cost),
      sliderInput("Loss",
                  "Доход меньшинства",
                  min=1,
                  max = Win-1,
                  value=Loss),
      numericInput("Win",
                   "Доход большинства",
                   min=1,
                   max = Win*2,
                   value=Win),
      sliderInput("Tie",
                  "Доход при равенстве голосов",
                  min=1,
                  max = Win-1,
                  value=Tie),
      sliderInput("Prob",
                  "Вероятность голосования своих",
                  min=0,
                  max = 1,
                  value=0.5)
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("distPlot2")
    )
  )
))
