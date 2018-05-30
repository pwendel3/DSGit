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
#library(ggplotly)
#setwd('C:/Users/pwend/Documents/GitHub/DSGit/sportz/nba')
alldat<-read_csv('lot_adv.csv')
colnames(alldat)<-str_replace(colnames(alldat),'%','Percentage')
colnames(alldat)<-str_replace(colnames(alldat),'/','Per')
colnames(alldat)<-str_replace(colnames(alldat),'3','Three')
colnames(alldat)<-str_replace(colnames(alldat),'2','Two')

# colnamer<-
# 
# l<-c()
# for (i in 1:length(labelcol)){
#   l<-c(l,eval(labelcol[i])=colnamer[i])
# }

alldat%>%mutate_if(is.numeric,as.double)

# Define UI for application that draws a histogram
ui <- fluidPage(div(style="overflow-y:scroll"),
  
  # Application title
  titlePanel("Advanced Metrics by Draft Choice"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width=3,
      selectInput("var",
                  "Advanced Metric:",
                  
                 choices=colnames(alldat)[31:52],
                 selected='VORP'),
    
    sliderInput("drafts",
                "Drafts:",
                min = 1985,
                max = 2017,
                step=1,sep='',
                value = c(1985,2017)),
    
  
  sliderInput("exp",
              "Years Experience:",
              min = 1,
              max = 5,
              step=1,sep='',
              value = c(1,1)),
  # sliderInput("min",
  #             "Years Experience:",
  #             min = 1,
  #             max = 5,
  #             step=1,sep='',
  #             value = c(1,1)),
  
  checkboxGroupInput('pos','Position Filter:',
                     sort(alldat$Pos_season%>%unique()),
                     selected=alldat$Pos_season%>%unique()),
              
  sliderInput("minmin",
              "Minimum Minutes in Season:",
              min = 0,
              max = 4000,
              step=100,sep='',
              value = 500)
  
    )
  ,
  
    
  mainPanel(div(style="overflow-y:scroll"),
    plotlyOutput("advplot")
  )

# Show a plot of the generated distribution

)

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  plotdat<- reactive({alldat%>%filter(Year_draft>=input$drafts[1] & Year_draft<=input$drafts[2]&
                                        sincedrafted>=input$exp[1] & sincedrafted<=input$exp[2] &
                                        Pos_season%in%input$pos & MP_season>=input$minmin)}) 
  
  #
 
  output$advplot <- renderPlotly({
    print('through here')
    p<-ggplot(data=plotdat())+aes_string(y=input$var)+
      aes(x=Pk,color=Pos_season,text=sprintf('Player: %s<br>%.f Team: %s<br>Age: %.f<br>%.f, Pick %.f'
                                                 ,Player_season,Year_season,Tm_season,Age_season,Year_draft,Pk))+
      #geom_smooth(na.rm=TRUE,se=FALSE,size=1,linetype='dashed')+
      geom_jitter(width=0.225)+
      facet_grid(sincedrafted~.)+
      geom_smooth(aes_string(x='Pk',y=input$var),na.rm=TRUE,se=FALSE,size=1,linetype='dashed',col='black',inherit.aes=FALSE)
   p<-ggplotly(p,tooltip='text',height=500*(input$exp[2]-input$exp[1]+1))
  #p<-plotly_build(p)
  #heightin<-reactive({paste0(as.character(*500),'px')})
  #p$height=()
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

