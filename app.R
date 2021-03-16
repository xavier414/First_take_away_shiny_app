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


## Data uploading 

is.na(d1)
is.na(d2)
is.na(d3)

winered <- read.csv("https://query.data.world/s/yy4ie6vjf6igi2yyhty4s5ay2pmogx", header=TRUE, stringsAsFactors=FALSE)
winewhite <- read.csv("https://query.data.world/s/tmlt63lm3n3uzb2ujhlmkarlzoeo73", header=TRUE, stringsAsFactors=FALSE)

unique(is.na(winered))# no missing values
unique(is.na(winewhite)) #no missing values



#RED WINE DATA EDITS

summary(winered)

str(winered)

apply(winered, 2, function(x) length(unique(x)))

#all clearly continuous except quality, which has 6 possible values (on a scale of 10 bizarrely)

apply(winewhite, 2, function(x) length(unique(x)))

unique(winered$quality)

#Will make a dummy for the wine quality as "better" and "worse"

winered$quality2 <-ifelse((winered$quality>=4) & (winered$quality>=5),1,0)

winered$quality <- as.factor(winered$quality) #make original a factor

winered$quality2 <- as.factor(winered$quality2) #make dummy a factor

str(winered)

c(table(winered$quality2)) #number of categories, extremely unbalanced





#WHITE WINE DATA EDITS

summary(winewhite)

str(winewhite)

apply(winewhite, 2, function(x) length(unique(x)))

#all clearly continuous except quality, which has 7 possible values (on a scale of 10 bizzarely)

apply(winewhite, 2, function(x) length(unique(x)))

unique(winewhite$quality)

#Will make a dummy for the wine quality as "better" and "worse"

winewhite$quality2 <-ifelse((winewhite$quality>=4) & (winewhite$quality>=5),1,0)

winewhite$quality <- as.factor(winewhite$quality) #make original a factor

winewhite$quality2 <- as.factor(winewhite$quality2) #make dummy a factor

count(winewhite$quality2)



# Define UI for application that draws a histogram
ui <- navbarPage("Shiny app",
                 theme = shinytheme("sandstone"),
                 tabPanel("Backpack",
                          fluidPage(
                              sidebarLayout(sidebarPanel(
                                  selectInput("select", label = h3("Plot by Year"), 
                                              choices = list_choices,
                                              selected = 1)
                              ), mainPanel(
                                  h3("Plots"),
                                  plotOutput(outputId = "hello")
                              )
                              ))),
                 tabPanel("Presentation",
                          includeMarkdown("presentationlink.md")
                 ) #  titlePanel
) # navbarPage

col_scale <- scale_colour_discrete(limits = list_choices)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$hello <- renderPlot({
        ggplot(Backpack %>% filter(Year == input$select)
               , aes(BodyWeight, BackpackWeight, colour = Year)) +
            scale_x_log10() +
            col_scale +
            geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
