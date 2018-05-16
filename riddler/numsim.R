ref<-as.character(0:9)

as.numeric(paste0(a[0],a[1],a[2]))

for(a in 1:10){
  aa<-ref[a]
  for(b in 1:10){
    bb<-ref[b]
    for(c in 1:10){
      cc<-ref[c]
      for(d in 1:10){
        dd<-ref[d]
        for(e in 1:10){
          ee<-ref[e]
          num1<-as.numeric(paste0(aa,bb,cc,cc,dd,ee))
          num2<-as.numeric(paste0(ee,dd,cc,cc,bb,aa))
          prod<-num1*4
          if(prod==num2 & !any(duplicated(as.numeric(c(aa,bb,cc,dd,ee))))){
          print(paste('a: ',aa))
          print(paste('b: ',bb))
          print(paste('c: ',cc))
          print(paste('b: ',dd))
          print(paste('e: ',ee))
          print(paste0(num1,'*4=',num2))
          }
        }
      }
    }
  }
}