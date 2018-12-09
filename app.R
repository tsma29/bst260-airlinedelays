library(shiny)
library(markdown)
library(RColorBrewer)
library(dslabs)
library(tidyverse)
library(dplyr)
library(readr)
dat<-read_csv("201701.csv")
dat1 <- dat %>% mutate(DEP_HOUR=as.numeric(substr(CRS_DEP_TIME,1,2)))

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
                        radioButtons("carrier1", "Airlines", c("AA"="AA","AS"="AS","B6"="B6","DL"="DL","EV"="EV","F9"="F9",
                                                              "HA"="HA","NK"="NK","OO"="OO","UA"="UA","VX"="VX","WN"="WN"))
                      ),
                      mainPanel(
                        plotOutput("plotheat")
                      )
                    )
           ),
           tabPanel("Delay time",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("carrier2", "Airlines", c("AA"="AA","AS"="AS","B6"="B6","DL"="DL","EV"="EV","F9"="F9",
                                                              "HA"="HA","NK"="NK","OO"="OO","UA"="UA","VX"="VX","WN"="WN"))
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