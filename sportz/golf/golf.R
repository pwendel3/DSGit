library(rvest)
library(tidyverse)
#install.packages('NbClust')
library(NbClust)

url<-'http://www.espn.com/golf/stats/hole'
%>%html_table()

urls<-read_html(url)%>%html_nodes('.tablesm')
#%>%html_table()


tourneyurls<-urls[[1]]%>%html_children()%>%html_attr('value')



for(i in 1:length(tourneyurls)){

tourneyurl<-paste0('http:',tourneyurls[i])
read_html(tourneyurl)%>%html_table(fill=TRUE)->tourneytable


if(i==1){
  for(j in 1:length(tourneytable)){
    if(j==1){
      strokedats<-tourneytable[[j]]
      colnames(strokedats)<-strokedats[2,]
      strokedats$course<-strokedats[1,1]
      strokedats<-strokedats[3:nrow(strokedats),]
    }
    else{
      strokedat<-tourneytable[[j]]
      colnames(strokedat)<-strokedat[2,]
      strokedat$course<-strokedat[1,1]
      strokedat<-strokedat[3:nrow(strokedat),]
      strokedats<-rbind(strokedats,strokedat)
    }
  }
}
else{
  for(j in 1:length(tourneytable)){
    strokedat<-tourneytable[[j]]
    colnames(strokedat)<-strokedat[2,]
    strokedat$course<-strokedat[1,1]
    strokedat<-strokedat[3:nrow(strokedat),]
    if(ncol(strokedat)==ncol(strokedats)&nrow(strokedat)>17){
      strokedats<-rbind(strokedats,strokedat)
    }
  }
}

}

strokedats%>%mutate_at(1:(ncol(strokedats)-1),.~as.numeric(ifelse(.=='NA','0',.)))->strokedats

strokedats$played<-strokedats$EAGLES+strokedats$BIRDIES+strokedats$PARS+strokedats$BOGEYS+
  strokedats$DOUBLES+strokedats$OTHERS

colnames(strokedats)

strokedats%>%mutate_at(5:10,.~./played)->strokedats

strokedats$yardpar<-strokedats$YARDS/strokedats$PAR

strokedats%>%select(-'course',-'+/- AVG.',-'played')%>%scale()->scalestroke

dist(scalestroke)->strokedist

hclust(strokedist)->strokeclust

class(urls[[1]])


# res<-NbClust(scalestroke, distance = "euclidean", min.nc=2, max.nc=10,
#              method = "silhouette")



lista.methods = c("kl", "ch", "hartigan","mcclain", "gamma", "gplus",
                  "tau", "dunn", "sdindex", "sdbw", "cindex", "silhouette",
                  "ball","ptbiserial", "gap","frey")
lista.distance = c("metodo","euclidean", "maximum", "manhattan", "canberra")

tabla = as.data.frame(matrix(ncol = length(lista.distance), nrow = length(lista.methods)))
names(tabla) = lista.distance

# for (j in 2:length(lista.distance)){
#   for(i in 1:length(lista.methods)){
#     
#     nb = NbClust(scalestroke, distance = lista.distance[j],
#                  min.nc = 2, max.nc = 10, 
#                  method = "complete", index =lista.methods[i])
#     tabla[i,j] = nb$Best.nc[1]
#     tabla[i,1] = lista.methods[i]
#     
#   }}


nb = NbClust(scalestroke, distance='euclidean',
             min.nc = 2, max.nc =20, 
             method = "complete", index ='silhouette')


for(i in 1:nb$Best.nc[1]){
  plotdat<-strokedats%>%select(-course)%>%filter(clust==i)%>%gather(var,value,-clust)
  p[[i]]<-plotdat%>%ggplot(aes(x=value))+geom_histogram(stat='bin')+facet_wrap(vars(var),scales='free')

}
