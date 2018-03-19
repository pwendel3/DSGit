#install.packages('dplyr')
#install.packages('ggplot2')
library(ggplot2)
#install.packages('titanic')
library(titanic)
data('titanic_train',package='titanic')
titanic<-titanic_train
library(dplyr)


#obj<-
titanic<-titanic %>% mutate(Survived=factor(Survived,
                                            levels=c(0,1),
                                            labels=c('Yes','No')))
ggplot(titanic,aes(x=Fare,y=Age,color=Survived))+geom_point()+
  #geom_text(aes(label=Ticket),color="black")+
  ggtitle('Titanic GG')+
  xlab('Fare ($ USD)')+ylab('Age (years)')

#install.packages('faraway')
#install.packages('nloptr')
library(faraway)
data(nepali)

nepali<-nepali %>% select(id,sex,wt,ht,age) %>%
  mutate(id=factor(id),
         sex=factor(sex,levels=c(1,2),
                    labels=c('Male','Female'))) %>%
  distinct(id,.keep_all=TRUE)


head(nepali)

ggplot(nepali,aes(x=ht))+geom_histogram(fill='lightblue',color='black')+ggtitle('Height of Children')+
  xlab('Height(cm)')


ggplot(nepali,aes(x=ht,y=wt,color=sex,size=age))+geom_point(data=nepali)+xlab('Height(cm)')+ylab("Weight(kg)")


ggplot(nepali,aes(x=sex,y=ht,color=sex))+geom_boxplot()


#install.packages('gridExtra')
library(gridExtra)
#install.packages('ggthemes')
library(ggthemes)

data(worldcup)
data(nepali)


#install.packages('dlnm')
library(dlnm)
data("chicagoNMMAPS")
chic<-chicagoNMMAPS
chic_july<-chic %>% filter(month==7 & year==1995)

ggplot(worldcup,aes(x=Time,y=Shots))+
  geom_point()+
  theme_fivethirtyeight()

chicago_plot<-ggplot(chic_july, aes(x=date,y=death))+
  xlab('Day in July')+ylab('All-Cause Deaths')+
  ylim(0,450)

chicago_plot+geom_area(fill='black')+theme_excel()

chicago_plot+geom_line()+theme_tufte()

worldcup%>%filter(Team %in%c('Italy','Mexico','Spain','Netherlands') )%>% 
  ggplot (aes(x=Time,y=Shots))+geom_point()+facet_grid(Team~Position)
