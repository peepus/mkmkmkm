
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


a <- readRDS("data/Rzuty.RDS")

b <- readRDS("data/Strefy.RDS")

boisko <- readPNG("data/boisko3.png")
boisko <- rasterGrob(boisko, width=unit(1.0,"npc"), height=unit(1.0,"npc"))

boisko2 <- readPNG("data/boisko2.png")
boisko2 <- rasterGrob(boisko2, width=unit(1.0,"npc"), height=unit(1.0,"npc"))

shinyServer(function(input, output, session) {
  
       observe({
         if(input$sezon == "Wszystkie"){
      g <- sort(unique(a$zawodnik)) 
      updateSelectInput(session, "zawodnik", choices = g)
      
         
        } else {
         g <- subset(a, a$Sezon == input$sezon)
         g <- sort(unique(g$zawodnik)) 
    updateSelectInput(session, "zawodnik", choices = g)
       }
  })

  
  
  data <- reactive({
    
    if(input$sezon == "Wszystkie") {
      
      
      l <- subset(b, b$zawodnik == input$zawodnik)
      l$zawodnik <- NULL
      l$Sezon <- NULL
      n <- l
      n$Średnia <- c("60%", "35%", "33%", "36%", "36%")
      podkosz <- subset(n, n$Strefa == "Spod Kosza")
      podkosz$FGM <- sum(podkosz$FGM)
      podkosz$FGA <- sum(podkosz$FGA)
      podkosz$FGpr <- podkosz$FGM/podkosz$FGA
      podkosz$FGpr <- percent(podkosz$FGpr)
      podkosz <- podkosz[1,]
      
      mid <- subset(n, n$Strefa == "Midrange")
      mid$FGM <- sum(mid$FGM)
      mid$FGA <- sum(mid$FGA)
      mid$FGpr <- mid$FGM/mid$FGA
      mid$FGpr <- percent(mid$FGpr)
      mid <- mid[1,]
      
      ps3 <- subset(n, n$Strefa == "3pt Srodek")
      ps3$FGM <- sum(ps3$FGM)
      ps3$FGA <- sum(ps3$FGA)
      ps3$FGpr <- ps3$FGM/ps3$FGA
      ps3$FGpr <- percent(ps3$FGpr)
      ps3 <- ps3[1,]
      
      pl3 <- subset(n, n$Strefa == "3pt Lewy Naroznik")
      pl3$FGM <- sum(pl3$FGM)
      pl3$FGA <- sum(pl3$FGA)
      pl3$FGpr <- pl3$FGM/pl3$FGA
      pl3$FGpr <- percent(pl3$FGpr)
      pl3 <- pl3[1,]
      
      pp3 <- subset(n, n$Strefa == "3pt Prawy Naroznik")
      pp3$FGM <- sum(pp3$FGM)
      pp3$FGA <- sum(pp3$FGA)
      pp3$FGpr <- pp3$FGM/pp3$FGA
      pp3$FGpr <- percent(pp3$FGpr)
      pp3 <- pp3[1,]
      
      n <- rbind(podkosz, mid, ps3, pl3, pp3)
      
      n[,4] <- n[,6]
      n[,6] <- NULL
      
      n[,4][n[,4] == "NaN%"] <- "0%"
      
      
      n$FGM <- as.character(n$FGM)
      n$FGA <- as.character(n$FGA)
      n[,4] <- as.character(n[,4])
      n <- n
      
      
    
  }else {
  l <- subset(b, b$Sezon == input$sezon)  
  l <- subset(l, l$zawodnik == input$zawodnik)
  l$zawodnik <- NULL
  l$Sezon <- NULL
  n <- l
  n$Średnia <- c("60%", "35%", "33%", "36%", "36%")
  n$FGM <- as.character(n$FGM)
  n$FGA <- as.character(n$FGA)
  n[,4] <- as.character(n[,4])
  n <- n
  
  }
})
  
  
  
  
  output$table <- renderTable({
    data()
  })
  
  

  output$plot <- reactivePlot(function() {
    
    
    if(input$sezon == "Wszystkie") {
h <- subset(a, a$zawodnik == input$zawodnik)

zaw <- h$zawodnik[1]
zaw <- as.character(zaw)

len <- length(h$zawodnik)
  
team <- h$druzyna[len]
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


}else{ 
  
  h <- subset(a, a$Sezon == input$sezon)
  h <- subset(h, h$zawodnik == input$zawodnik)
  
  
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
  
  
  
  
  
  
}  
  })

})
  



  




