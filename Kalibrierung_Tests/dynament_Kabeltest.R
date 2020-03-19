
#pfade definieren

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
datapfad<-paste0(hauptpfad,"Daten/Urdaten/GGA/")
sqlpfad<-paste0(hauptpfad,"Daten/aufbereiteteDaten/SQLite/")
dynamentpfad<-paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad<-paste0(hauptpfad,"/Daten/Ergebnisse/Plots/")

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

##alle csv-Dateien aus dem Dynament Ordner auflisten
dynament.files<-list.files(dynamentpfad,".csv",full.names = T)

dynament.list<-lapply(dynament.files, read.csv,skip=1,stringsAsFactors=F)

nr<-grep("20191219_091941",dynament.files)
#colnames von dynament csv Dateien einlesen
dynament.colnames<-read.csv(dynament.files[nr],nrows=1,header=F,stringsAsFactors=F)
#Zeitspalten benennen
dynament.colnames[1:2]<-c("dmy","HMS")
dynament<-dynament.list[[nr]]
colnames(dynament)<-dynament.colnames
dynament$date<-dmy_hms(paste(dynament$dmy,dynament$HMS))
dynament.wide<-dynament[,str_detect(colnames(dynament),"CO2.+|date")]

dynament.wide<-dynament.wide[-(1:200),]


dynament.long<-reshape2::melt(dynament.wide,id="date",variable.name="sensor",value.name="CO2_V")

dynament.long$diff<-abs(c(NA,diff(dynament.long$CO2)))
dynament.long$diff[dynament.long$date==min(dynament.long$date)]<-NA
dynament.long$kabel<-"alle_dick"
dynament.long$kabel[grep("Dyn_08",dynament.long$sensor)]<-"A_dick"
dynament.long$kabel[grep("Dyn_(03|18|14)",dynament.long$sensor)]<-"V_dick"
dynament.long$CO2<-(dynament.long$CO2_V-0.4)/1.6*5000
ggplot(dynament.long)+geom_line(aes(date,CO2,col=kabel))+facet_wrap(~sensor)
ggplot(dynament.long)+geom_line(aes(date,diff,col=kabel))+facet_wrap(~sensor)

ggplot(dynament.long)+geom_line(aes(date,CO2,col=sensor))
