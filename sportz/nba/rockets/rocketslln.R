library(tidyverse)
library(reshape2)
library(plotly)
#install.packages('fitdistrplus')
library(fitdistrplus)
setwd('C:/Users/pwend/Documents/GitHub/DSGit/sportz/nba/rockets')

rockets<-read_csv('rockets.csv',skip=2)



colnames(rockets)<-make.names(colnames(rockets))
colnames(rockets)



hist(rockets$FT)
ftdist<-fitdist(rockets$FT,'gamma')
plot(ftdist)
cor(rockets$FTA,rockets$FGA)





rockets$X2PA<-rockets$FGA-rockets$X3PA
rockets$X2P<-rockets$FG-rockets$X3P
rockets$X2P.<-rockets$X2P/rockets$X2PA
rockets$X3sel<-rockets$X3PA/rockets$FGA


plot(density(rockets$X3PA))
plot(density(rockets$X2P.))

hist(rockets$X2P.)
rockets%>%ggplot(aes(x=X3P,y=X2P))+geom_point()
cor(rockets$X3P.,rockets$X2P.)
rockets%>%ggplot(aes(x=X2PA,y=X2P.))+geom_point()

x3dist<-fitdist(rockets$X3P.,'gamma')
x2dist<-fitdist(rockets$X2P.,'gamma')
plot(x2dist)
games<-10000
shots<-90
shotbreak<-0.3
shots3<-round(shots*shotbreak)
shots3
shots2<-shots-shots3
shots2
#points2<-
x3dist$estimate[1] 
shots3
shots2
points3<-3*(shots3*rgamma(games,shape=x3dist$estimate[1],rate=x3dist$estimate[2]))
points2<-2*(shots2*rgamma(games,shape=x2dist$estimate[1],rate=x2dist$estimate[2]))
tpoints<-points3+points2
hist(tpoints)
print(mean(tpoints))


plot(ftdist)
plot(fgdist)
hist(rockets$X3P.)
hist(rockets$X2P.)
hist(rockets$FGA)
hist(rockets$X2P)
hist(rockets$X3P)
hist(rockets$FT)

lmr<-lm(Tm~-1+X3PA*X2PA,data=rockets)
plot(rockets$Tm,predict(lmr,rockets))

rockets%>%select(Tm)

rockcorr<-rockets%>%select_if(is.numeric)%>%cor()%>%melt()
p<-rockcorr%>%ggplot(aes(x=Var1,y=Var2,fill=value))+geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                        midpoint = 0, limit = c(-1,1), space = "Lab",
                        name="Pearson\nCorrelation")
p%>%ggplotly()
#rockcorr%>%gather()


# 100 shots taken
# distribtuion of 2s/3s
# 2% 3%
# fts 
hist(rockets$FT)
rockets$X3PAP<-rockets$X3PA/rockets$FGA

rockets%>%ggplot(aes(x=FGA,y=X3PA))+geom_point()
