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

hoopdat<-read_csv('hoopdat.csv')

simrps<-function(x){
  n<-x
  
  steps<-0
  apos<-1
  aposs<-c()
  bpos<-n
  bposs<-c()
  while(apos!=n & bpos!=1){
    if(apos==bpos | (bpos-apos)==1){
      #print(paste0('BATTLE at ',(apos+bpos)/2 ))
      going<-TRUE
      while(going){
        rr<-runif(1)
        if(rr<1/3){
          # print('A wins')
          #a wins
          bpos<-n
          going<-FALSE
          #apos<-apos+1
        }
        else if(rr<2/3){
          #b wins
          # print('B wins')
          apos<-1
          going<-FALSE
          #bpos<-bpos+1
        }
        else{
          # print('Tie')
          
        }
        steps<-steps+1
        aposs<-c(aposs,apos)
        bposs<-c(bposs,bpos)
      }
    }
    {
      apos<-apos+1
      bpos<-bpos-1
    }
    steps<-steps+1
    # print('')
    # print(paste0('Steps:',steps))
    # print(paste0('A position:',apos))
    # print(paste0('B position:',bpos))
    aposs<-c(aposs,apos)
    bposs<-c(bposs,bpos)
  }
  p<-tibble('A'=aposs,'B'=bposs,'seconds'=1:steps)%>%gather(player,position,-seconds)%>%ggplot(aes(x=seconds,y=position,col=player))+
    geom_line(size=1)+
    scale_x_continuous(limits=c(1,steps))+scale_y_continuous(limits=c(1,n))
  
  return(list(p,steps))
}

#x<-c()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Rock Paper Scissors Race"),
  
  # Sidebar with a slider input for number of bins 
  
  tabsetPanel(
    tabPanel('Individual',
             sidebarLayout(
               sidebarPanel(
                 sliderInput("length",
                             "Length:",
                             min=2,max=100,
                             value=8)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotlyOutput("pathplot"),#,
                 plotlyOutput('histplot')
               )
             )
    ),
    tabPanel('Aggregate',
             plotlyOutput('groupplot')
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  autoInvalidate <- reactiveTimer(2000)
  
  p<-reactive({
    per<-simrps(as.numeric(input$length))
    x<-c(x,as.numeric(p[[2]]))
    per
  })
  #p1<-p()
  
  output$pathplot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    
    
    autoInvalidate()
    #pin<-p()
    pin<-simrps(as.numeric(input$length))
    # draw the histogram with the specified number of bins
    
    feedp<-pin[[1]]+ggtitle(paste0('Simulated Game with ',input$length,' Hoops'))
    feedp%>%ggplotly()
    
  })
  output$histplot<-renderPlotly({
    hoopdatin<-hoopdat%>%filter(hoops==as.numeric(input$length))
    midder<-mean(hoopdatin$seconds,na.rm=TRUE)
    p<-hoopdatin%>%ggplot(aes(x=seconds))+
      geom_histogram(fill='white',col='light blue')+geom_vline(xintercept=midder,col='red')+
      geom_text(aes(label=round(midder,1)),y=0,x=midder)+ggtitle(paste0('Game Length Histogram for ',input$length,' Hoops'))
    ggplotly(p)
  })
  
  output$groupplot<-renderPlotly({
    groupdat<-hoopdat%>%group_by(hoops)%>%summarise(mean_time=mean(seconds,na.rm=TRUE))%>%ggplot(aes(x=hoops,y=mean_time))+
      geom_point()+geom_hline(yintercept=1800,col='red')+ggtitle('Average Game Length vs. Number of Hoops')+
      ylab('mean seconds')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

