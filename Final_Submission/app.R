library(shiny)
library(markdown)
library(RColorBrewer)
library(dslabs)
library(tidyverse)
library(dplyr)
library(readr)
dat<-read_csv("flight2017.csv")
dat <- dat %>% select(CRS_DEP_TIME, OP_UNIQUE_CARRIER, DEP_DEL15, DAY_OF_WEEK, DEP_DELAY_NEW)
dat1 <- dat %>% mutate(DEP_HOUR=as.integer(as.numeric(CRS_DEP_TIME)/100))

dat2<- dat1 %>%  filter(!is.na(DEP_DEL15)) %>%
  group_by(DEP_HOUR, DAY_OF_WEEK) %>%
  mutate(PERCENTAGE_OVERALL=mean(DEP_DEL15))
dat3<- dat1 %>% filter(!is.na(DEP_DELAY_NEW) & DEP_DEL15==1) %>%
  group_by(DEP_HOUR) %>%
  mutate(AVERAGE_OVERALL=mean(DEP_DELAY_NEW))

dat4 <- dat2 %>%  filter(!is.na(DEP_DEL15)) %>%
  group_by(OP_UNIQUE_CARRIER, DAY_OF_WEEK, DEP_HOUR) %>%
  mutate(PERCENTAGE=mean(DEP_DEL15))
dat5 <- dat3 %>% filter(!is.na(DEP_DELAY_NEW) & DEP_DEL15==1) %>%
  group_by(OP_UNIQUE_CARRIER,DEP_HOUR) %>%
  mutate(AVERAGE=mean(DEP_DELAY_NEW))

ui <- navbarPage("Effect of depature time on",
           tabPanel("Delay percentage",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("carrier1", "Airlines", c("American Airlines"="AA","Alaska Airlines"="AS","JetBlue Airlines"="B6","Delta Airlines"="DL","ExpressJet Airlines"="EV","Frontier Airlines"="F9",
                                                              "Hawaiian Airlines"="HA","Spirit Airlines"="NK","SkyWest Airlines"="OO","United Airlines"="UA","Virgin America"="VX","SouthWest Airlines"="WN"))
                      ),
                      mainPanel(
                        plotOutput("plotheat")
                      )
                    )
           ),
           tabPanel("Delay time",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("carrier2", "Airlines", c("American Airlines"="AA","Alaska Airlines"="AS","JetBlue Airlines"="B6","Delta Airlines"="DL","ExpressJet Airlines"="EV","Frontier Airlines"="F9",
                                                               "Hawaiian Airlines"="HA","Spirit Airlines"="NK","SkyWest Airlines"="OO","United Airlines"="UA","Virgin America"="VX","SouthWest Airlines"="WN"))
                      ),
                      mainPanel(
                        plotOutput("plotbar")
                      )
                    )
           )
          
)


server <- function(input, output, session) {
  
  output$plotbar <- renderPlot({ 
    dat5 %>% filter(OP_UNIQUE_CARRIER==input$carrier2) %>%
      select(DEP_HOUR,AVERAGE,OP_UNIQUE_CARRIER) %>%
      unique() %>%
      ggplot(aes(DEP_HOUR,AVERAGE))+
      geom_bar(stat="identity", fill="#720017")+
      labs(y="Average delay time (minutes)",x="Departure hour")
    
  })  
  
  
   output$plotheat <- renderPlot({ 
         dat4 %>% filter(OP_UNIQUE_CARRIER==input$carrier1) %>%
                    ggplot(aes(DEP_HOUR, DAY_OF_WEEK,  fill = PERCENTAGE)) +
                    geom_tile(color = "white") +
                    scale_fill_gradientn(colors = brewer.pal(9, "Reds"),limits=c(0,1)) +
                    scale_y_continuous(breaks=seq(1,7))+
                    theme_minimal() +  
                    theme(panel.grid = element_blank()) +
                    labs(y="Day of week",x="Departure hour")
                  
                  })
         
        
}


shinyApp(ui, server)
