library(ggplot2)
library(dplyr)
#install.packages('maps')
library(maps)
us_map<-map_data('state')
head(us_map,3)

setwd('C:/Users/pwendel/Documents/GitHub/DSGit/Coursera/R_data_viz')

us_map %>% filter(region %in% c('north carolina','south carolina')) %>%
  ggplot(aes(x=long,y=lat))+geom_point()

us_map %>% filter(region %in% c('north carolina','south carolina'))%>%
  ggplot(aes(x=long,y=lat,group=group))+geom_path()

us_map %>% filter(region %in% c('north carolina','south carolina')) %>%
  ggplot(aes(x=long,y=lat,group=group,fill=region))+geom_polygon(color='black')+
  theme_void()

us_map %>% ggplot(aes(x=long,y=lat,group=group))+
  geom_polygon(fill='lightblue',color='black')+theme_void()

data(votes.repub)
head(votes.repub)

library(dplyr)
#install.packages('viridis')
library(viridis)

votes.repub%>%tbl_df()%>%mutate(state=rownames(votes.repub),state=tolower(state))%>%
right_join(us_map, by=c('state'='region')) %>%
  ggplot(aes(x=long,y=lat,group=group,fill=`1976`))+geom_polygon(color="black")+theme_void()+
  scale_fill_viridis(name='Republican\nvotes (%)')


#install.packages('tidyr')
library(tidyr)
meltvote<-votes.repub%>%tbl_df()%>%mutate(state=rownames(votes.repub),state=tolower(state))%>%gather(year,votes,-state)

meltvote%>%right_join(us_map,by=c('state'='region'))%>%ggplot(aes(x=long,y=lat,group=group,fill=votes))+
  geom_polygon(color='black')+theme_void()+scale_fill_viridis(name='Republican\nvotes (%)')+
  facet_wrap(~year)

# install.packages('readr')
library(readr)

serial<-read_csv(paste0("https://raw.githubusercontent.com/",
                "dgrtwo/serial-ggvis/master/input_data/",
                "serial_podcast_data/serial_map_data.csv"))
head(serial)

serial<-serial %>% mutate(long=-76.8854+0.00017022*x,
                          lat=39.23822+1.371014e-04*y,
                          tower=Type=='cell-site')
serial %>% slice(c(1:3,(n()-3):n()))

maryland<-map_data('county',region='maryland')
head(maryland)

baltimore<-maryland%>%filter(subregion %in% c('baltimore city','baltimore'))
head(baltimore,3)

base_bal<-ggplot(baltimore, aes(x=long,y=lat,group=group))+geom_polygon(fill='lightblue',color='black')+
  theme_void()

base_bal+geom_point(data=serial,aes(group=NULL,color=tower))+
  scale_color_manual(name='Cell tower',values=c('black','red'))

#install.packages('ggmap')
###install.packages('sp')
install.packages('devtools')
library(devtools)
install_github('dkahle/ggmap')
library(ggmap)
register_google()
beijing<-get_map("Beijing",zoom=12)
ggmap(beijing)

get_map('DFW airport',zoom=15)%>%ggmap()

get_map("Baltimore County",zoom=10,
                         source='stamen',maptype='toner')%>%
  ggmap()+
  geom_polygon(data=baltimore,aes(x=long,y=lat,group=group),
               color='navy',fill='lightblue',alpha=0.2)+
  geom_point(data=serial, aes(x=long,y=lat,color=tower))+
  scale_color_manual(name='Cell tower',values=c('black','red'))

get_map(c(-76.6,39.3),zoom=11,
        source='stamen',maptype='toner')%>%
  ggmap()+
  geom_polygon(data=baltimore,aes(x=long,y=lat,group=group),
               color='navy',fill='lightblue',alpha=0.2)+
  geom_point(data=serial, aes(x=long,y=lat,color=tower))+
  scale_color_manual(name='Cell tower',values=c('black','red'))


#install.packages('tigris')
library(tigris)
library(sp)
denver_tracts<-tracts(state='CO',county=31,cb=TRUE)

#install.packages('plotly')
library(plotly)
library(faraway)
data(worldcup)
plot_ly(worldcup,type='scatter',x=~Time,y=~Shots,color=I('blue'))

worldcup %>% mutate(Name=rownames(worldcup))%>%
  plot_ly(x=~Time,y=~Shots,color=~Position)%>%
  add_markers(text=~paste("<b>Name:</b>",Name,'<br />',
                          '<b>Team:</b>',Team),hoverinfo='text')

read_csv('data/floyd_track.csv') %>% plot_ly(x=~datetime,y=~max_wind) %>%
  add_lines() %>% rangeslider()



denver_tracts <- tracts(state = "CO", county = 31, cb = TRUE)
load("data/fars_colorado.RData")
denver_fars <- driver_data %>% 
  filter(county == 31 & longitud < -104.5)

install.packages('leaflet')

