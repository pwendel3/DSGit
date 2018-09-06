library(rvest)
library(tidyverse)
library(plotly)
library(psych)

years<-2017:1983
for(j in 1:length(years)){
  year<-years[j]
  baseurl<-paste0('https://www.pro-football-reference.com/years/',year,'/')
  preurl<-paste0(baseurl,'preseason.htm')
  
  basehtm<-read_html(baseurl)
  basetables<-basehtm%>%html_table()
  for(i in 1:length(basetables)){
    if(i==1){
      basetable<-basetables[i]
    }
    else{
      basetable<-bind_rows(basetable,basetables[i])
    }
  }
  
  #glimpse(basetable)
  basetable%>%mutate(playoffs=ifelse(grepl("[^A-Za-z0-9 .]",Tm),'yes','no'))->basetable
  basetable<-basetable%>%filter(!grepl('[A-Za-z]',W))%>%mutate(Tm=gsub("[^A-Za-z0-9 ]","",Tm))
  basetable$year<-year
  
  ps<-basehtm%>%html_nodes('p')%>%html_text()
  
  psb<-ps[grepl('Super Bowl',ps)]
  
  psb<-substring(psb, regexpr(":", psb) + 1)
  psb<-gsub("\n","",psb)%>%trimws()
  psb<-gsub("[^A-Za-z0-9 ]","",psb)
  
  basetable%>%mutate(playoffs=ifelse(Tm==psb,'champion',playoffs))->basetable
  
  prehtm<-read_html(preurl)
  pretables<-prehtm%>%html_table()
  
  for(i in 1:2){
    if(i==1){
      pretable<-pretables[i]
    }
    else{
      pretable<-bind_rows(pretable,pretables[i])
    }
  }
  
  pretable<-pretable%>%filter(!grepl('[A-Za-z]',W))%>%mutate(Tm=gsub("[^A-Za-z0-9 .]","",Tm))
  
  alltable<-basetable%>%inner_join(pretable,by='Tm',suffix=c('_reg','_pre'))
  if(j==1){
    alltables<-alltable
  }
  else{
    alltables<-bind_rows(alltables,alltable)
  }
}


glimpse(alltables)

numer<-alltables%>%select(-Tm,-playoffs)%>%colnames()
alltables%>%mutate_at(numer,as.numeric)->alltables
alltables%>%mutate(pre_win_p=W_pre/(W_pre+L_pre))%>%rename(reg_win_p=`W-L%`)->alltables


p<-alltables%>%ggplot(aes(x=pre_win_p,y=reg_win_p,col=playoffs,
                          text=paste0(year,' ',Tm,'<br>Reg:',W_reg,'-',L_reg,'<br>Pre:',W_pre,'-',L_pre)))+geom_jitter(width=.03,height=0.03)+
  xlab('Preseason Win %')+ylab('Regular Season Win %')+ggtitle('Regular Season Win % vs. Preseason Win %')+
  labs(color='Playoffs',subtitle='1983-2017')

ggplotly(p,tooltip='text')

alltables%>%ggplot(aes=playoffs)

pdist<-alltables%>%mutate(pre_win_p=cut(pre_win_p,breaks=c(-1,0.2,0.4,0.6,0.8,1.2),labels=c('0-20%','20-40%','40-60%','60-80%','80-100%')))%>%group_by(pre_win_p,playoffs)%>%summarise(count=n())%>%
  spread(playoffs,count)%>%mutate_at(2:4,~replace_na(.,0))%>%mutate(
                                                  total=champion+no+yes,
                                                  champion=champion/total,
                                                  no=no/total,
                                                  yes=yes/total)

pdist%>%mutate_at(c('champion','no','yes'),~.*total)
pdist

pd<-pdist%>%filter(total>50)%>%gather(outcome,dist,-pre_win_p,-total)%>%mutate(outcome=ifelse(outcome=='yes','playoffs',outcome))%>%ggplot(aes(x=pre_win_p,y=dist,fill=outcome,text=paste0('Outcome Probability:',dist,'<br>Total Cases:',total)))+
  geom_col(position='dodge')+xlab('Preseason Win %')+ylab('Probability')+ggtitle('Season Outcome Probability by Preseason Outcome')

ggplotly(pd,tooltip='text')

corrr<-corr.test(alltables$reg_win_p,alltables$pre_win_p,method='spearman')

corrr$p
corrr$ci

pdist$total/sum(pdist$total)
sapply(0:4,function(x)dbinom(x, 4, 0.5))



lag(alltables$reg_win_p)

alltables%>%arrange(Tm,year)%>%group_by(Tm)%>%
  mutate(lag_reg_win_p=lag(reg_win_p))->alltables

lmer<-lm(reg_win_p~lag_reg_win_p+pre_win_p,data=alltables)
summary(lmer)

alltables%>%ggplot(aes(x=pre_win_p,y=lag_reg_win_p,col=reg_win_p))+geom_point()

                   

plot_ly(alltables,z=~reg_win_p,y=~lag_reg_win_p,x=~pre_win_p,
        color=~playoffs)

write_csv(alltables,path='NFL-preseason-regseason.csv')
