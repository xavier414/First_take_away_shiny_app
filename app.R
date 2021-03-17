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
                      
                          sidebarLayout(
                          sidebarPanel(
                            
                            #selectInput("select", label = h3("Plot by Quality"), 
                             #            choices = list_choices,
                              #            selected = 1),
                             
                              
                              selectInput(inputId = "plotlyXs", 
                                        label = "Select x-axis variable", 
                                         choices=colnames(winewhite),
                                  selected = "volatile.acidity"),
                              
                              selectInput(inputId = "plotlyYs", 
                                          label = "Select y-axis variable", 
                                          choices=colnames(winewhite),
                                          selected = "fixed.acidity") 
                          ),
                             
                                
                        mainPanel(
                                
                              h3("Plot of Quality while comparing other variables"),
                              
                              plotlyOutput(outputId = "hello"),
                        
                         hr(),
                       HTML(
                         paste(
                         "*There are no quality values for 0, 1, 2 and 10, other words
                          there is no wine that has no quality or is perfect.",'<br/>'))                    
                        
                        ), #main panel closing
                         
                        
                          ),
                    )),
                 
                              
                    tabPanel("Table", "Table", DT::dataTableOutput("mytable"))
                            
                   ) # close navbarPage
                 
             

#col_scale <- scale_colour_discrete(limits = list_choices)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$hello <- renderPlotly({
    
     
   # %>% filter(Quality == input$select)
    
    ggplot(winewhite 
           ,aes_string(x = input$plotlyXs, y = input$plotlyYs, colour = "Quality")) +
     # col_scale +
      geom_point()
     
    
     })
    
    output$mytable = DT::renderDataTable({
      winewhite
      
    
    
  })
}

# Run the application 

shinyApp(ui = ui, server = server)
