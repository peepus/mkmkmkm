
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(png)
library(grid)
library(readxl)



shinyUI(fluidPage(
  
  tags$head(includeScript("data/google-analytics.js")),
  
  theme = "united.css",
  titlePanel("Mapy Rzutów PLK"),
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sezon", "Sezon",
                  choices = c("2012-13", "2013-14", "2014-15",
                "2015-16", "2016-17", "Wszystkie"), selected = "2016-17"),
      selectInput("chart", "Typ",
                  choices = c("HeatMapa", "Standardowy"),
                  selected = "Standardowy"),
    selectInput("zawodnik", "Zawodnik", ""),
    tableOutput("table"),
    
    h4("pulsbasketu.pl "),
    h5("Autor: Bartosz Ziarkowski"),
    h5("twitter: @bziarkowski"),
    h5("mail: bziarkowski1@gmail.com"),
    h6("Wszytskie dane pochodzą z live.fibaeurope.com")),

  
    mainPanel(
      plotOutput("plot", height = 620, width = 860)
   
    )
  )
)
)



  
  


