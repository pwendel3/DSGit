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
library(plotly)
# setwd('C:/Users/pwend/Documents/GitHub/DSGit/sportz/nba/finalsmvp')
invdat<-read_csv('invdats.csv')
colnames(invdat)<-str_replace(colnames(invdat),'%','_Percentage')
colnames(invdat)<-str_replace(colnames(invdat),'/','_Per_')
colnames(invdat)<-str_replace(colnames(invdat),'3','Three_')
colnames(invdat)<-str_replace(colnames(invdat),'2','Two')


invdat$ismvp<-invdat$Player==invdat$MVP
invdat%>%mutate(captioner=ifelse(ismvp,paste0('MVP: ',Player,'\n',Team),paste0(Player,'\n',Team)))->invdat
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Finals with Highest Average GameScore on Losing Team"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("year",
                  "Qualifying Year:",
                  choices=unique(invdat$Year),
                  selected=2018),
      selectInput("stat",
                  "Metric:",
                  choices=list('GameScore'='GS',
                               'Minutes'='MP',
                               'FG'='FG',
                               'FGA'='FGA',
                               'FG %'='FG_Percentage',
                               '3P'='Three_P',
                               '3PA'='Three_PA',
                               '3P %'='Three_P_Percentage',
                               'FT'='FT',
                               'FTA'='FTA',
                               'FT %'='FT_Percentage',
                               'ORB'='ORB',
                               'DRB'='DRB',
                               'TRB'='TRB',
                               'AST'='AST',
                               'STL'='STL',
                               'BLK'='BLK',
                               'TOV'='TOV',
                               'PF'='PF',
                               'PTS'='PTS'
                               ),
                  selected='GS')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("distPlot"),
      textOutput('caption')
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  plotdat<- reactive({invdat%>%filter(Year==input$year)%>%arrange(desc(ismvp))}) 
  
  output$distPlot <- renderPlotly({
    plotdat<-plotdat()
    plotdat$captioner<-factor(plotdat$captioner,levels=unique(plotdat$captioner))
    # generate bins based on input$bins from ui.R
    p<-ggplot(data=plotdat,aes_string(x='Game',y=input$stat,col='captioner'))+
      #aes(text=paste0(Player,'\n',Team,'\n',stat))+
      geom_line()+geom_point()+labs(caption='test')+ylab('')+theme(plot.caption=element_text(hjust = 0.5,size=rel(1.2)),
                                                          legend.title=element_blank() )
    
    
    # draw the histogram with the specified number of bins
    ggplotly(p,tooltip=c('y'))%>%layout(legend=list(
      orientation='h',
      yanchor='top',
      y=-0.2
    ))
  })
  
  output$caption<-renderText({
    plotdat<-plotdat()
    games<-c(4,max(plotdat$Game)-4)
    teams<-plotdat$Team%>%unique()
    champ<-plotdat$Champ%>%unique()
    loss<-teams[teams!=champ]
    titler<-paste0('\n',input$year,' Finals: ',champ,' beat ',loss,' (',games[1],'-',games[2],')')
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

