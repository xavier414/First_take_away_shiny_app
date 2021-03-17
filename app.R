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

list_choices <- unique(winewhite$Quality)
list_choices <- list_choices[!is.na(list_choices)]
list_choices <- sort(list_choices)

#BUTTON HEADER


myHeader <- div(id="advanced",
                useShinyjs(),
                downloadButton("report", "Generate report"),
                )



# Define UI for application that draws a histogram
ui <- navbarPage("Shiny app",
                 
                 theme = shinytheme("sandstone"),
                 
                 header = myHeader,
                 
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
                                
                              h3("Plot of wine quality while comparing other variables"),
                              
                              plotlyOutput(outputId = "hello"),
                              
                        hr(),
                         
                       HTML(
                         
                         paste(
                         "*There are no quality values for 0, 1, 2 and 10, other words
                          there is no wine that has no quality or is perfect.",'<br/>'))
                       
                        
                        ), #main panel closing
                         
                        
                          ),
                    )),
                 
                              
                    tabPanel("Table", DT::dataTableOutput("mytable")),
                 
                    tabPanel("Lasso Table", DT::dataTableOutput("data_table"))
                             
                            
                   ) # close navbarPage
                 
             

# col_scale <- scale_colour_discrete(limits = list_choices)


# Define server logic required to draw a plot

server <- function(input, output) {
  
  output$hello <- renderPlotly({
    
     
   # %>% filter(Quality == input$select)
    
      p <- ggplot(winewhite 
           ,aes_string(x = input$plotlyXs, y = input$plotlyYs, colour = "Quality")) +
             # col_scale +
            geom_point()
      
      ggplotly(p,source="master")
     
    
     })
    
    output$mytable = DT::renderDataTable({
      winewhite}, filter = 'top')
    
    
    
    #LASSO TABLE
    
    selected<-reactive({
      # event_data("plotly_click", source = "master")
      event_data("plotly_selected", source = "master")
    })
    
    
    output$text <- renderPrint({ 
      list(selection=selected(),
           dims=data()$sel)
    })
    
    
    output$data_table <- DT::renderDataTable(
      data()$sel, filter = 'top', options = list(  
        pageLength = 5, autoWidth = TRUE
      )
    )
    

    #reactive data
    
    data<-reactive({
      
      tmp<-winewhite
      
      sel<-tryCatch(winewhite[(selected()$pointNumber+1),,drop=FALSE] , error=function(e){NULL})
      
      list(data=tmp,sel=sel)
      
    })
    
    #END OF LASSO TABLE
    
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
          Xs = isolate(input$plotlyXs),
          Ys = isolate(input$plotlyYs),
          winewhite = isolate(winewhite),
          data = isolate(d)
        )
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
  
} #end of server

# Run the application 

shinyApp(ui = ui, server = server)
