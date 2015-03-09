setwd('C:/Users/dhuaraca/Desktop/Estadisticos Sociodemograficos')
library(foreign)
library(ff)
library(ffbase)
library(sampling)
list.files()
base <- read.csv.ffdf(file="muestras_consolidadas.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, 
                      next.rows=50000, colClasses=NA,sep=";")
class(base)
muestra <- stratified(data,5,0.01)

require(sampling)
temp <- df[order(df[group]),]
tab.group <- data.frame(table(df[,group]))
new.tab <- numeric(nrow(tab.group))
for(i in 1:nrow(tab.group)){
    if(tab.group[i,2]<=173){
      new.tab[i]<-min(tab.group[i,2],139)
    }else{
      if(tab.group[i,2]<=525){
        new.tab[i]<-min(tab.group[i,2],242)
      }else{
        if(tab.group[i,2]<=2018){
          new.tab[i]<-min(tab.group[i,2],353)
        }else{
          new.tab[i]<-min(tab.group[i,2],393)
        }
      }
    }
}

sum(new.tab)
summary(new.tab)
strato = strata(temp, stratanames = names(temp[group]), size = new.tab, method = "srswr")
dsample = getdata(temp, strato)

marca<-1*(as.numeric(row.names(dsample))==ceiling(as.numeric(row.names(dsample))))
sum(marca)
muestra_final <- data.frame(dsample,marca)

muestra_ult <- subset(muestra_final,muestra_final$marca==1)
muestra_final[1:50,]
muestra <- muestra_ult[,c(1,2)]
write.table(muestra,'C:/Users/dhuaraca/Desktop/Estadisticos Sociodemograficos/muestra_telefonica.txt',row.names=FALSE)

############ OTRA VERSION ###############

attach(data)
sub.lpdata<-data
## Create a 10% sample, stratified by PARROQUIA
sort.lpdata <- sub.lpdata[order(data$PARROQUIA_CNE),]
tab.state <- data.frame(table(data$PARROQUIA_CNE))
size.strata <- as.vector(round(ceiling(tab.state$Freq)*0.1))
muestra <-strata(sort.lpdata,stratanames="PARROQUIA_CNE",size=size.strata,method="srswor")
m = match("PARROQUIA_CNE", colnames(data))
nrow(sort.lpdata)
length(data$PARROQUIA_CNE)

by(base$ULT_DIGITO,base$PARROQUIA_CNE,mean)
split(base$SEXO,base$PARROQUIA_CNE)

write.table(data.cruce,'C:/Users/dhuaraca/Desktop/Telefonica/BDD/muestra_consolidada.txt',row.names=FALSE)
