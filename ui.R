
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


a <- readRDS("data/a.RDS")

b <- readRDS("data/Strefy1.RDS")

zawodnicy <- unique(a$zawodnik)
zawodnicy <- sort(zawodnicy)

shinyUI(fluidPage(theme = "united.css",
  titlePanel("Mapy Rzutów PLK 2016/17"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("chart", "Typ",
                  choices = c("HeatMapa", "Standardowy"),
                  selected = "Standardowy"),
    selectInput("zawodnik", "Zawodnik",
                choices = zawodnicy, selected = zawodnicy[1]),
    tableOutput("table"),
    tableOutput("eff"),
    
    h4("pulsbasketu.pl "),
    h5("Autor: Bartosz Ziarkowski"),
    h5("twitter: @bziarkowski"),
    h5("mail: bziarkowski1@gmail.com"),
    h6("Wszyskie dane pochodzą z live.fibaeurope.com")),
  
    mainPanel(
      plotOutput("plot", height = 600, width = 800)
    
      
    )
  )
)
)




  
  


