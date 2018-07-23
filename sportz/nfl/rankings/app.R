#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(DT)
#library(ggplotly)
#setwd('C:/Users/pwend/Documents/GitHub/DSGit/sportz/nfl/rankings')
alldat<-read_csv('seasonagg2017.csv')%>%
  rename(`Def. Adjusted Rankscore`=drank,
         `Off. Adjusted Rankscore`=orank,
         `Avg. Yards Off.`=yardso,
         `Avg. Yards Def.`=yardsd,
         Team=team)%>%
  select('Team','Avg. Yards Off.','Off. Adjusted Rankscore',
         'Avg. Yards Def.','Def. Adjusted Rankscore')
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NFL Non-Parametric Rankings"),
   
   # Sidebar with a slider input for number of bins 

      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(
           tabPanel('Plots',
             tabsetPanel(
               tabPanel('Offense',
                 plotlyOutput('oplot')
               ),
               tabPanel('Defense',
                 plotlyOutput('dplot')
               )
             )
           ),
           tabPanel('Data Table',
             dataTableOutput('table')
           )
         )
      )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$table <- renderDataTable({
     alldat
   })
   
   output$dplot<-renderPlotly({
     p<-alldat%>%ggplot(aes(x=`Avg. Yards Def.`,y=`Def. Adjusted Rankscore`,
                        text=sprintf('%s<br> Avg. Yards Def.: %.2f<br> Def. Adjusted Rankscore: %.2f',
                                     Team,`Avg. Yards Def.`,`Def. Adjusted Rankscore`))
                        )+
       geom_point()
     ggplotly(p,tooltip='text')
   })
   output$oplot<-renderPlotly({
     p<-alldat%>%ggplot(aes(x=`Avg. Yards Off.`,y=`Off. Adjusted Rankscore`,
                            text=sprintf('%s<br> Avg. Yards Off.: %.2f<br> Off. Adjusted Rankscore: %.2f',
                                         Team,`Avg. Yards Off.`,`Off. Adjusted Rankscore`))
     )+
       geom_point()
     ggplotly(p)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

