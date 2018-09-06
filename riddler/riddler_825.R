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
    geom_line()+geom_point()+
    scale_x_continuous(limits=c(1,steps))+scale_y_continuous(breaks=1:n,limits=c(1,n))
  
return(list(p,steps))
}
p<-simrps(20)
p[[1]]
p[[2]]


hoops<-c()
secondss<-c()
for (i in 2:100){
  for(j in 1:1000){
    hoops<-c(hoops,i)
    p<-simrps(i)
    p[[1]]
    secondss<-c(secondss,p[[2]])
  }
}
seconds

hoopdat<-tibble(hoops,seconds=secondss)
hoopdat%>%filter(hoops==50)%>%ggplot(aes(x=seconds))+geom_histogram()

hoopdat%>%group_by(hoops)%>%summarise(mean_dat=mean(seconds))


write_csv(hoopdat,'hoopdat.csv')


hoopdat%>%group_by(hoops,seconds)%>%summarise(count=n())->groupdat

p<-plot_ly(groupdat[groupdat$hoops>10,],x=~hoops,y=~seconds,z=~count)
p

hoopdat%>%ggplot(aes(x=seconds))+geom_histogram()+
  facet_wrap(~hoops,scales='free')
