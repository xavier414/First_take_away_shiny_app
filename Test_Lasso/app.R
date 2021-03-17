#plotly box or lasso select linked to 
# DT data table
# using Wage data
# the out group: is sex:Male, region:Middle Atlantic +


library(ggplot2)
library(plotly)
library(dplyr) 
library(ISLR) 
library(shiny)
library(DT)

#reactive app
ui <- fluidPage(
    # Set theme
    # theme = shinytheme("spacelab"),
    fluidRow(
        column(12, plotlyOutput("plot", height = "600px")),
        column(12,DT::dataTableOutput('data_table'))
        # column(12, verbatimTextOutput("text"))
    )
)


server <- function(input, output){
    
    output$plot <- renderPlotly({
        req(data())
        p<-ggplot(data = data()$data, mapping = aes(x = age, y = wage)) + 
            geom_point() + theme_bw() 
        
        obj<-data()$sel
        if(nrow(obj)!=0) {
            p<-p + geom_point(data=obj,color="red",size=4)
        }
        
        ggplotly(p,source="master")
    })
    
    #selected
    selected<-reactive({
        # event_data("plotly_click", source = "master")
        event_data("plotly_selected", source = "master")
    })
    
    output$text <- renderPrint({ 
        list(selection=selected(),
             dims=data()$sel)
    })
    
    output$data_table<-DT::renderDataTable(
        data()$sel, filter = 'top', options = list(  
            pageLength = 5, autoWidth = TRUE
        )
    )
    
    
    
    #reactive data
    data<-reactive({
        tmp<-Wage 
        
        sel<-tryCatch(Wage[(selected()$pointNumber+1),,drop=FALSE] , error=function(e){NULL})
        
        list(data=tmp,sel=sel)
        
    })
    
    
}  

shinyApp(ui,server)
