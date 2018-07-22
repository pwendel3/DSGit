#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(plotly)


ews<-rep(1:61,rep(21,61))
xs<-rep(seq(0,by=0.1,length.out = 61),rep(21,61))

nss<-rep(rev(letters[1:21]),61)
ys<-rep(seq(0,by=0.1,length.out = 21),61)

nodetib<-tibble(ew=ews,ns=nss,x=xs,y=ys)%>%mutate(name=paste0(ns,ew))%>%
  select(name,everything())
matrix(ncol=1271,nrow=1271)

froms<-c()
tos<-c()
for(i in 1:21){
  ns<-letters[i]
  for(j in 1:60){
    froms<-c(froms,paste0(ns,j))
    tos<-c(tos,paste0(ns,j+1))
  }
}

for(i in 1:61){
  for(j in 1:20){
    froms<-c(froms,paste0(letters[j],i))
    tos<-c(tos,paste0(letters[j+1],i))
  }
}

#devtools::install_github('hadley/ggplot2')
#devtools::install_github('thomasp85/ggforce')
#devtools::install_github('thomasp85/ggraph')

edgetib<-tibble(from=froms,to=tos)%>%mutate(weight=ifelse(grepl('u',from)&grepl('u',to),0.1/200,0.1/20))

gridgraph<-graph_from_data_frame(d=edgetib,vertices=nodetib,directed=FALSE)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("PBJ Delivery"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("avenue",
                     "Avenue:",
                     letters[1:21],
                     selected='f'),
         selectInput("street",
                     "Street:",
                     1:61,
                     selected=20)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("mapplot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   plotgraph<-reactive({
     startnode<-paste0(input$avenue,input$street)
     f20<-V(gridgraph)[V(gridgraph)$name == startnode]
     f20paths<-shortest_paths(gridgraph,from=f20,weights=NULL)
     
     # vertgrid<-V(gridgraph)
     
     highway<-c()
     for(i in 1:length(f20paths$vpath)){
       spath<-f20paths$vpath[[i]]
       if(sum(grepl('u',spath$name))>=2){
         highwayin<-'yes'
       }else if(tail(spath$name,1)==startnode){
         highwayin<-'starting point'
       }else{
         highwayin<-'no'
       }
       highway<-c(highway,highwayin)
     }
     
     #gridgraph
     set_vertex_attr(gridgraph,'highway',value=highway)->gridgraph
     return(gridgraph)
   })
   output$mapplot <- renderPlotly({
     gp<-ggraph(plotgraph())+geom_node_point(aes(colour=highway,
                                                 text=paste0('Avenue:',ns,'<br>',
                                                                            'Street:',ew))
                                             ,size=2)+
       xlab('Street')+ylab('Avenue')+
       theme(axis.text=element_blank(),
             axis.title.y=element_text(angle=270))+
       labs(colour="Ultra")
     ggplotly(gp)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

