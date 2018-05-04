# set pirate count
pc<-7
# set how much the monkey takes
mt<-1
print(paste('Pirates: ',pc))
print(paste('Monkey keeps: ',mt ))

invsteal<-function(x){(pc/(pc-1))*(x+mt)}

checknum<-function(xin){
  x<-pc*xin
  solved<-0
  for(i in 1:pc){
    x<-invsteal(x)
    if(x!=round(x)){break}
    if(i==pc){
      print(paste(x,' is a solution!'))
      solved<-x
    }
    
  }
  return(solved)
}

out<-0
y<-1
while(out==0){
  y<-y+1
  out<-checknum(y)
}

steal<-function(x){((pc-1)/pc)*x-mt}
out2<-out
keeps<-rep(0,pc)
for(i in 1:pc){
  print(paste(out2,' at start of round ',i))
  keep<-out2/pc
  leave<-out2*(pc-1)/pc
  print(paste('pirate takes ',keep,' and leaves ',leave))
  out2<-steal(out2)
  print(paste('monkey gets ',mt, ', ',out2,' left'))
  keeps[i]<-keep
  
}
keeps<-keeps+out2/7
for(i in 1:pc){
  print(paste('pirate 1 keeps ',keeps[i],' coconuts'))
}
print(paste('monkey keeps ',mt*pc,' coconuts'))
