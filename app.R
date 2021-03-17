#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyjs)
library(VGAM)
library(corrplot)
library(plotly)
library(DT)
library(ggplot2)



## Data uploading 


winered <- read.csv("https://query.data.world/s/yy4ie6vjf6igi2yyhty4s5ay2pmogx", header=TRUE, stringsAsFactors=FALSE)
winewhite <- read.csv("https://query.data.world/s/tmlt63lm3n3uzb2ujhlmkarlzoeo73", header=TRUE, stringsAsFactors=FALSE)

winewhite$quality <- as.factor(winewhite$quality) #make original a factor

attach(winewhite)

names(winewhite)[names(winewhite) == "quality"] <- "Quality"


# R SHINY CODE

list_choices <-  unique(winewhite$Quality)
list_choices <- list_choices[!is.na(list_choices)]
list_choices <- sort(list_choices)


# Define UI for application that draws a histogram
ui <- navbarPage("Shiny app",
                 
                 theme = shinytheme("sandstone"),
                 
                 tabPanel("Plot",
                          fluidPage(
                            sidebarLayout(sidebarPanel(
                              selectInput("select", label = h3("Plot by Quality"), 
                                          choices = list_choices,
                                          selected = 1),
                              hr(),
                              HTML(
                                paste(
                                  "*There is no values for 0, 1, 2 and 10, other words
                                     there is no wine that has no quality or is perfect.",'<br/>'))                    
                                  
                                  ), 
                              
                              sidebarPanel(
                                selectInput("plotlyXs", "Select x-axis variable", 
                                            choices=colnames(winewhite))),
                                
                              mainPanel(
                                
                              h3("Plot of Quality"),
                              
                              plotlyOutput(outputId = "hello")
                            ),
                            
                    tabPanel("Table", DT::dataTableOutput("mytable")
                                     
                            ))),
                 
                 ) #  titlePanel
) # navbarPage

col_scale <- scale_colour_discrete(limits = list_choices)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$hello <- renderPlotly({
    
    
      
      ggplotly(
        
    ggplot(winewhite %>% filter(Quality == input$select)
           , aes(x=input$plotlyXs, volatile.acidity, colour = Quality)) +
      col_scale +
      geom_point()
    
    
    ) })
    
    output$mytable = DT::renderDataTable({
      winewhite
      
    
    
  })
}

# Run the application 

shinyApp(ui = ui, server = server)
