library(plotly)
library(tidyverse)

# inverse steal helper functin
invsteal<-function(x,pc,mt){(x/(pc-1))*pc+mt}
invsteal(8,7,1)



# function to check if number is a solution
checknum<-function(xin,pc,mt){
   x<-pc*xin
   solved<-0
   for(i in 1:pc){
     if(((x/(pc-1))*pc)%%1==0){
       x<-invsteal(x,pc,mt)
       #print(x)
       #print(x%%1)
       #if(x%%1>1e-10){
       if(x%%1!=0){
        #print(paste(x,' is not an integer'))
               break
       }
       else if(i==pc & x%%1==0){
         solved<-x
      }
     }else{break}
   }

  
  return(solved)
}
# steal helper function

steal<-function(x,pc,mt){((pc-1)/pc)*(x-mt)}
steal(8,7,1)
invsteal(6,7,1)

# total function
# pc-pirate count
# mt-monkey take count
# printer- print interesting output

dynacoco<-function(pc,mt,printer=FALSE){
  if(printer){
    print(paste('Pirates: ',pc))
    print(paste('Monkey keeps: ',mt ))
  }
  out<-0
  x<-1
  
  # increment until a solution is found
  while(out==0){
    out<-checknum(x,pc,mt)
    x<-x+1
  }

  # print breakdown if printer is true
  if(printer){
    if(printer){print(paste(out,' is a solution!'))}
    out2<-out
    keeps<-rep(0,pc)
    for(i in 1:pc){
      print(paste(out2,' at start of round ',i))
      out2<-out2-mt
      print(paste('monkey gets ',mt, ', ',out2,' left'))
      keep<-out2/pc
      out2<-(out2)*(pc-1)/pc
      print(paste('pirate takes ',keep,' and leaves ',out2))
      #out2<-steal(out2,pc,mt)
      #print(paste('monkey gets ',mt, ', ',out2,' left'))
      keeps[i]<-keep
      
    }
    keeps<-keeps+out2/pc
    for(i in 1:pc){
      print(paste('pirate 1 keeps ',keeps[i],' coconuts'))
    }
    print(paste('monkey keeps ',mt*pc,' coconuts'))
  }
  return(out)
}

# set pirate count
pcin<-7
# set how much the monkey takes
mtin<-1

dynacoco(pc=pcin,mt=mtin,printer=TRUE)


#empty matrix
surftib<-tibble(pirates=numeric(),monkey_take=numeric(),min_coconuts=numeric())

# simulate for 2:10 pirates, 1:10 monkeys
# (WARNING: this takes a long time)
for(i in 2:8){
  for(j in 1:8){
    coco<-dynacoco(i,j)
    surftib<-surftib%>%add_row(pirates=i,monkey_take=j,min_coconuts=coco)

  }
}




View(surftib)
bigtib<-surftib%>%filter(pirates<8)%>%spread(monkey_take,min_coconuts)
tibmat<-as.matrix(bigtib[,2:ncol(bigtib)])

p<-plot_ly(data=surftib,x=1:8,y=2:8,z=~tibmat)%>%
  add_surface()%>%layout(title='Min. Coconuts by Pirate, Monkey Take',
                               scene=list(xaxis=list(title='Monkey Takes'),
                                          yaxis=list(title='Pirates'),
                                          zaxis=list(title='Min. Coconuts'))
  )

p
api_create(p,filename = 'coconuts')

