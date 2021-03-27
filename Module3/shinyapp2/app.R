#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(rsconnect)

# Import the Data

cdc <- read.csv("https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv")
head(cdc)

# Define UI for application

ui <- fluidPage(
    titlePanel("Crude Mortality Rate Across All States Vs National Average"), 
    sidebarPanel(
        selectInput("select1", label = strong("State"), 
                    choices = levels(as.factor(cdc$State)), 
                    selected = 1),
        
        selectInput("select2", label = strong("Cause of Death"), 
                    choices = levels(as.factor(cdc$ICD.Chapter)), 
                    selected = 1),
        
        width = "auto"
    ),
    
    mainPanel(
        plotOutput("distPlot")
    )
)


# Define server to show whether particular States are improving their mortality rates (per cause)
# faster than, or slower than, the national average.

server <- function(input, output) {
    output$distPlot <- renderPlot({
        cdc %>% 
            group_by(Year, ICD.Chapter) %>%
            mutate(N_Population = sum(Population),
                   N_Count = sum(Deaths), 
                   N_Crude_Rate = 10^5*(N_Count/N_Population)) %>% 
            group_by(Year, ICD.Chapter, State) %>%
            mutate(S_Count=sum(Deaths),
                   S_Crude_Rate=10^5*(S_Count/Population)) %>%
            select(ICD.Chapter, State, Year, N_Crude_Rate, S_Crude_Rate) %>% 
            filter(ICD.Chapter == input$select2, State == input$select1) %>% 
            ggplot() +
            geom_bar(aes(x = Year, weight = S_Crude_Rate)) +
            labs(x = "State", y = "Crude Mortality Rate") + 
            geom_line(aes(x = Year, y = N_Crude_Rate, linetype = "National Average"), col = "red", lwd = 1) +
            scale_linetype(name = NULL) +
            theme_minimal()
    }
    )
}


# Run the application 
shinyApp(ui = ui, server = server)
