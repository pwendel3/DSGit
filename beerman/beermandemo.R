
#set to whatever directory you have this file in
setwd('C://Users/pwendel/Documents/GitHub/DSGit/beerman')

# you may need to install it
# install.packages('tidyverse')
# install.packages('plotly')
# etc.

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(plotly)


bdat<-read_csv('R Test - WCBL.csv')

bdat$month<-1:24

mbdat<-bdat%>%gather(key='name',value='value',-month)
glimpse(mbdat)

mbdat%>%ggplot(aes(x=month,y=value))+geom_point()+ggtitle('Beerman')


#interactive plot with name on hover over
p<-plot_ly(mbdat,x=~month,y=~value,type='scatter',mode='marker',text=~name,
        hoverinfo='text')
p

# log scale for values
pl<-plot_ly(mbdat,x=~log(month),y=~log(value),type='scatter',mode='marker',text=~name,
           hoverinfo='text')
pl

#fit a lil exponential model
expmod<-lm(log(value)~log(month),data=mbdat)
summary(expmod)

expred<-exp(predict(expmod,list(month=1:24)))
plot(1:24,expred)

p2<-plot_ly(mbdat,x=~month,y=~value,type='scatter',mode='marker',text=~name,
           hoverinfo='text')%>%
  add_trace(x=1:24,y=expred,mode='lines',inherit=FALSE)
p2
