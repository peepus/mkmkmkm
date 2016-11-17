
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(png)
library(grid)
library(readxl)

source("helpers.R")


a <- readRDS("data/a.RDS")

b <- readRDS("data/Strefy1.RDS")

boisko <- readPNG("data/boisko3.png")
boisko <- rasterGrob(boisko, width=unit(1.0,"npc"), height=unit(1.0,"npc"))

boisko2 <- readPNG("data/boisko2.png")
boisko2 <- rasterGrob(boisko2, width=unit(1.0,"npc"), height=unit(1.0,"npc"))

shinyServer(function(input, output) {
  
  data <- reactive({
  l <- subset(b, b$zawodnik == input$zawodnik)
  l$zawodnik <- NULL
  n <- l
  n$Średnia <- c("60%", "35%", "33%", "36%", "36%")
  n$FGM <- as.character(n$FGM)
  n$FGA <- as.character(n$FGA)
  n[,4] <- as.character(n[,4])
  n <- n
  })
  
  pps <- reactive({
    l <- subset(b, b$zawodnik == input$zawodnik)
    zaw <- l$zawodnik
    l$zawodnik <- NULL
    n <- l
    n$Średnia <- c("60%", "35%", "33%", "36%", "36%")
    n[,4] <- as.character(n[,4])
    n <- n
    zawPPS <- (((n[1,2]+n[2,2])*2) + ((n[3,2]+n[4,2]+n[5,2])*3))/ sum(n$FGA)
    p12 <- subset(b, b$Strefa == "Spod Kosza")
    p22 <- subset(b, b$Strefa == "Midrange")
    
    p13 <- subset(b, b$Strefa == "3pt Srodek") 
         p23 <- subset(b, b$Strefa == "3pt Lewy Naroznik")
          p33 <- subset(b, b$Strefa == "3pt Prawy Naroznik")
          
    p2 <- rbind(p12, p22)
    
    p3 <- rbind(p13, p23, p33)
    
    
          
    p2 <- p2$FGM*2
    p3 <- p3$FGM*3
    pts <- sum(p2,p3)
    fga <- sum(b$FGA)
    
    meanPPS <- pts/fga
    
    ptsps <- data.frame(zawPPS, meanPPS)
    colnames(ptsps) <- c("Punkty na rzut Zawodnika", "Średnia Ligi")
    return(ptsps)
 })
  
  
  output$table <- renderTable({
    data()
  })
  
  

  output$plot <- reactivePlot(function() {
    
h <- subset(a, a$zawodnik == input$zawodnik)

zaw <- h$zawodnik[1]
zaw <- as.character(zaw)

team <- h$druzyna[1]
team <- as.character(team)

if(input$chart == "HeatMapa") {

p <- ggplot(h, aes(x=x,y=y)) +
  stat_density2d(geom="raster", aes(fill = ..density..), contour = FALSE, na.rm = FALSE, n = 250) +
  scale_fill_gradientn(colors = col, guide = FALSE )+
  annotation_custom(boisko, -60, 1570, -10, 1150)  +
  guides(alpha = FALSE, size = FALSE, fill = FALSE) +
  annotate("text", x = 770, y = 1020, label = team , size = 8, family="sans", fontface="bold", color = "white") +
  annotate("text", x = 770, y = 1100, label = zaw , size = 8.5, family="sans", fontface="bold", color = "white") +
  xlim(-60, 1570) +
  ylim(-10, 1150) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.margin = element_blank(),
        legend.title = element_blank(),
        plot.title = element_blank())
print(p)

  }else if(input$chart == "Standardowy") {
    p <- ggplot(h, aes(x=x,y=y)) +
      annotation_custom(boisko2, -60, 1570, -10, 1150) +
      geom_point(aes(colour = trafienie), size = 4, alpha = 0.7, stroke = 2) +
      scale_color_manual(values = c("#00C853", "#f44336")) +
      guides(alpha = FALSE, size = FALSE, fill = FALSE) +
      annotate("text", x = 770, y = 1020, label = team , size = 8, family="sans", fontface="bold", color = "#263238") +
      annotate("text", x = 770, y = 1100, label = zaw , size = 10, family="sans", fontface="bold", color = "#263238") +
      xlim(-60, 1570) +
      ylim(-10, 1150) +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "none",
            legend.title = element_blank(),
            plot.title = element_blank())
    print(p)
  
}
  })
  
  output$eff <- renderTable({
    pps()
  })


})
  



  




