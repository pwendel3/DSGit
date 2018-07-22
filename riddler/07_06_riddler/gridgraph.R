library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(plotly)
letters[1:21]

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

#gridtab<-tbl_graph(nodes=nodetib,edges=edgetib,directed=FALSE)



startnode<-'f20'
f20<-V(gridgraph)[V(gridgraph)$name == startnode]
f20paths<-shortest_paths(gridgraph,from=f20,weights=NULL)
vertgrid<-V(gridgraph)

which(vertgrid$name=='g1')



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

set_vertex_attr(gridgraph,'highway',value=highway)->gridgraph


#ggraph(gridgraph)+geom_node_point(aes(colour=highway))
#gp
ggraph(gridgraph)+geom_node_point(aes(colour=highway),size=2)+
  xlab('1st - 61st Street')+ylab('Avenune a-z')+
  theme(axis.text=element_blank(),
        axis.title.y=element_text(angle=270))+
  labs(colour="Ultra")+ggtitle('PBJ Delivery')

lengthx$vpath
class(f2)

class(x$vpath[[1]])
ggplotly(gp)

tbl_graph%>%filter_edges()


nodeq<-'u18'
ind<-which(vertgrid$name==nodeq)

spath<-f20paths$vpath[[ind]]
spath

spathgraph<-induced_subgraph(gridgraph,spath)
if(sum(grepl('u',spath$name))>2){
  colin<-'green'
}else{
  colin<-'blue'
}


spathgraph%>%ggraph()+geom_edge_link(colour=colin)+geom_node_text(aes(label=name))+
  xlim(0,6)+ylim(0,2)
