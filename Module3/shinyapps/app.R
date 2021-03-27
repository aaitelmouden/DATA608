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
    titlePanel("Crude Mortality Rate Across All States by Causes"), 
    sidebarLayout(
        selectInput("select", label = h3("Causes of Death"), 
                    choices = cdc$ICD.Chapter, 
                    selected = 1,
                    width = '100%'),
        
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic required to rank States by crude mortality for each cause of death.

server <- function(input, output) {
    output$distPlot <- renderPlot({
        ggplot(cdc[cdc$ICD.Chapter == input$select,] , aes(x = reorder(State, Crude.Rate), y = Crude.Rate)) +
            labs(x = "State", y = "Crude Mortality Rate") +  
            geom_bar(stat = "identity") +
            coord_flip() +
            theme_minimal()
    }, width = 'auto', height = 'auto')
}


# Run the application 
shinyApp(ui = ui, server = server)
