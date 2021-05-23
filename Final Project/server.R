# DATA608 Project | Abdellah Ait Elmouden
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dygraphs)
library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)
library(readr)
library(DT)
library(bslib)

# Define server logic required to draw a histogram

server = function(input, output) {
    
    output$deathTable = 
        DT::renderDataTable({
            nyc_mortality
            # DT::datatable(selections()[,c("leading_cause", "deaths", "death_rate", "age_adjusted_death_rate")],
            #              colnames = c("Leading Cause of Death", "Number of Deaths", "Death Rate", "Age-Adjusted Death Rate"),
            #             options = list(order = list(2, 'des')),
            #            rownames = FALSE
            #)
            
        })
    
######## Death by year by sex
    
    selections = reactive({
        req(input$year)
        req(input$sex)
        req(input$race)
        filter(nyc_mortality, year == input$year) %>%
            filter(sex %in% input$sex) %>%
            filter(race_ethnicity %in% input$race)})

    output$deathPlot = renderPlot({
        ggplot(data = selections(), aes(x = reorder(leading_cause, -deaths), y = deaths)) +
            geom_bar(stat = 'identity', color = 'white', fill = '#865ca8') +
            labs(
                
                x = "Causes",
                y = "Number of Deaths"
            ) +
            theme(axis.text.x = element_text(angle = 45, hjust=1))
    })
    
    output$sexplot = renderPlot(({
        
        nyc_mortality %>% group_by(year,sex) %>% summarise(Total = sum(as.numeric(deaths)), .groups = 'keep') %>%
        ggplot(aes(year,Total)) +  geom_bar(aes(fill = sex), position ="dodge",stat = "identity") + ylab('Total Death')
    }))
    
    output$raceplot = renderPlot(({
        
        nyc_mortality %>% group_by(year,sex) %>% summarise(Total = sum(as.numeric(deaths)), .groups = 'keep') %>%
            ggplot(aes(year,Total)) +  geom_bar(aes(fill = sex), position ="dodge",stat = "identity") + ylab('Total Death')
    }))
    
    
     selections2 = reactive({
         req(input$slider1)
         filter(nyc_aadr, year == input$slider1)
         })

     output$causeplot = renderPlot({

     ggplot(data = selections2(), aes(fill=leading_cause, y=Total_Age_adjusted, x=race_ethnicity)) + guides(fill=guide_legend(ncol=3)) + theme(legend.position="bottom")+
        geom_bar(position="dodge", stat="identity")

      })
    
    
}
