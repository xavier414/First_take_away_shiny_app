
## WHITE WINE APP BY XAVIER BRYANT  

library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyjs)
library(VGAM)
library(corrplot)
library(plotly)
library(DT)
library(ggplot2)



## DATA UPLOADING AND REVIEW


winewhite <- read.csv("https://query.data.world/s/tmlt63lm3n3uzb2ujhlmkarlzoeo73", header=TRUE, stringsAsFactors=FALSE)

unique(is.na(winewhite)) #no missing values

summary(winewhite)

str(winewhite)

apply(winewhite, 2, function(x) length(unique(x)))

#all clearly continuous except quality, which has 7 possible values (on a scale of 10 bizarrely)

c(table(winewhite$quality)) #number of categories, extremely unbalanced, most within 5 and 6

winewhite$quality = winewhite$quality %>% factor #make original a factor with dplyr of tidyverse

names(winewhite)[names(winewhite) == "quality"] <- "Quality"

attach(winewhite)


#R SHINY CODE

list_choices <- unique(winewhite$Quality)
list_choices <- list_choices[!is.na(list_choices)]
list_choices <- sort(list_choices)

#BUTTON 

button <- div(id="advanced",
                     useShinyjs(),
                     downloadButton("report", "Generate PDF of Plot and Data Summary"),
       
                    )



# Define UI for application that draws a histogram
ui <- navbarPage("App: White Wine Quality in Vinho Verde",
                 
                 theme = shinytheme("sandstone"),
                 
                 tabPanel("Data Overview",
                          
                          
                          fluidRow(column(h2("White Wine Quality in Vinho Verde"),
                                          
                                          hr(),
                                          br(),
                                          
                                          HTML(paste("This data set assesses the quality of 4898 white wine variants from the Portuguese Vinho Verde region based on 11 physicochemical features. The region
                                                     is in the northwest of Portugal as shown in the adjacent map to the left. The data was originally used in the paper")),                                                    
                                          tags$a(href="https://www.sciencedirect.com/science/article/abs/pii/S0167923609001377?via%3Dihub", "Modeling wine preferences by data mining from physicochemical properties"),
                                          HTML(paste("by Cortez et al. (2009).")),
                                          HTML(paste("The data set is posted on the")),
                                          tags$a(href="https://archive.ics.uci.edu/ml/datasets/wine+quality", "UCI Machine Learning Repository"), 
                                          HTML(paste(", but was sourced in this project from")),
                                          tags$a(href="https://data.world/food/wine-quality", "data.world."), 
                                          
                                          br(),
                                          br(),
                                          
                                          
                                          HTML(paste("The physicochemical properties of the white wine variants that act as the input variables:")),
                                          br(),
                                          tags$ul(
                                            tags$li("Fixed acidity"), 
                                            tags$li("Bolatile acidity"), 
                                            tags$li("Third list item"),
                                            tags$li("Citric acid"), 
                                            tags$li("Residual sugar"), 
                                            tags$li("Chlorides"), 
                                            tags$li("Free sulfur dioxide"), 
                                            tags$li("Total sulfur dioxide"), 
                                            tags$li("Density"), 
                                            tags$li("pH"), 
                                            tags$li("Sulphates"), 
                                            tags$li("Alcohol")
                                          ),
                                          
                                          br(),
                                          br(),
                                          
                                          HTML(paste("The output variable is <em> quality </em> on a scale of 1 to 10, however values 0,1,2 and 10 do not - indicating there is no wine that
                                                     is of perfect or no quality.")),
                                          
                                          br(),
                                          br(),
                                          
                                          HTML(paste("In this analysis, there is an interactive scatterplot where you can select the x and y axis from the physicochemical properties . The data is coloured by quality.
                                            The lasso feature can be used on the scatter plot to select data. This selected data can then be viewed in the <em> Lasso Table </em> tab. The entire
                                            data set can be viewed in <em> Table </em> tab and filters applied to select specific data.")), 
                                        
                                          
                                           width=8),
                                      column(h4("Map of Portuguese Wine Regions: Vinho Verde"), uiOutput("img"), width=4)
                                      
                          )
                          
                   
                 ),
                 
                 
                 tabPanel("Interactive Scatterplot",
                          
                    fluidPage(
                      
                          sidebarLayout(
                          sidebarPanel(
                            
                            
                              
                              selectInput(inputId = "plotlyXs", 
                                        label = "Select x-axis variable", 
                                         choices=colnames(winewhite),
                                  selected = "volatile.acidity"),
                              
                              selectInput(inputId = "plotlyYs", 
                                          label = "Select y-axis variable", 
                                          choices=colnames(winewhite),
                                          selected = "fixed.acidity"),
                              
                              button
                              
                              
                          ),
                        
                        
                                
                        mainPanel(
                                
                              h3("Scatterplot of white wine quality while comparing physicochemical features"),
                              
                              hr(),
                              
                              HTML(
                                
                                paste(
                                  "<b> Please use the Lasso function in the interactive scatterplot to see your selected data in the  <em> Lasso 
                           Table </em>  tab. </b> 
                           <br> </br>
                           You can see the full data set and apply filters in the <em> 
                           Table </em>  tab. ")),
                              
                              plotlyOutput(outputId = "hello"),
                              
                       
                         
                       HTML(
                         
                         paste(
                         " <br> </br> *There are no quality values for 0, 1, 2 and 10, in other words
                          there is no wine that has no quality or is perfect.",'<br/>'))
                       
                       
                       
                        
                        ), #main panel closing
                         
                        
                          ),
                    )),
                 
                 
                 
                              
                    
                 
                 tabPanel("Lasso Table", DT::dataTableOutput("data_table"),
                          
                          hr(),
                          
                          HTML(
                            
                            paste(
                              "Inspired by")),
                          tags$a(href="https://gist.github.com/dgrapov/128e3be71965bf00495768e47f0428b9", "dgrapov GitHub post")
                          
                          ),
                 
                 tabPanel("Table", DT::dataTableOutput("mytable"))
                 
                            
                   ) # close navbarPage
                 



# Define server logic required to draw a plot

server <- function(input, output) {
  
  # IMAGE
  
  output$img <- renderUI({
    tags$img(src = "https://bloximages.chicago2.vip.townnews.com/napavalleyregister.com/content/tncms/assets/v3/editorial/4/32/43209ca6-a84a-11e2-912d-001a4bcf887a/517028002c1eb.image.jpg") 
  }) #closing image
    
    
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
    
   xsummary <- reactive( summary(input$plotlyXs)   )
    ysummary <-  reactive( summary(input$plotlyYs) )
    xhead <-  reactive( summary(input$plotlyXs) )
     yhead <-  reactive( summary(input$plotlyYs)  )
   
    
    output$report <- downloadHandler(
      filename = "reportmypdf.pdf",
      content = function(file) {
        tempReport <- file.path(tempdir(), "reportmypdf.Rmd")
        file.copy("reportmypdf.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(
          Xs = isolate(input$plotlyXs),
          Ys = isolate(input$plotlyYs)
        )
      
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
  
} #end of server

# Run the application 

shinyApp(ui = ui, server = server)
