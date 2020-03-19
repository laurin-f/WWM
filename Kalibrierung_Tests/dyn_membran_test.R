#pfade definieren

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Dynament/")

library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)
###################################################
#Daten laden

#Zeitrahmen festlegen
datelim<-data.frame(start=NA,stop=NA)

datelim[1,]<-c("2020-01-22 12:00:00","2020-01-22 18:00:00")
datelim[2,]<-c("2020-01-23 09:00:00","2020-01-23 16:00:00")
datelim[3,]<-c("2020-01-24 09:00:00","2020-01-24 13:00:00")
datelim[4,]<-c("2020-01-30 09:00:00","2020-01-30 13:00:00")
datelim[5,]<-c("2020-02-03 09:00:00","2020-02-03 13:00:00")
datelim[6,]<-c("2020-02-05 12:00:00","2020-02-05 16:00:00")

membran_nrs<-list()
membran_nrs[[1]]<-c("03","13")
membran_nrs[[2]]<-c("03","13","19")
membran_nrs[[3]]<-c("03","13","19")
membran_nrs[[4]]<-c("03","13","19","00")
membran_nrs[[5]]<-c("03","13","19","00","14","01","04")
membran_nrs[[6]]<-c("03","13","19","00","14","01","04")
kleber<-list()
kleber[[1]]<-c("03"="?","13"="?") 
kleber[[2]]<-c("03"="?","13"="?","19"="Pattex 100%") 
kleber[[3]]<-c("03"="Pattex liquid","13"="?","19"="Pattex 100%") 
kleber[[4]]<-c("03"="Pattex liquid","13"="?","19"="Pattex 100%","00"="Bindulin GEL") 
kleber[[5]]<-c("03"="Pattex liquid","13"="?","19"="Pattex 100%","00"="Pattex liquid") 
kleber[[6]]<-c("03"="Pattex liquid","13"="?","19"="Pattex 100%","00"="Pattex liquid") 

#db funktion
versuche<-apply(datelim,1, function(x) read_db("dynament.db",table.name = "dynament_test",x,korrektur_dyn = T))

data<-lapply(versuche,reshape2::melt,id=c("date"),variable.name="sensor",value.name="CO2")


for(i in 1:length(data)){
  data[[i]]<-data[[i]][!is.na(data[[i]]$CO2),]
  data[[i]]$sensor_nr<-str_extract(data[[i]]$sensor,"\\d+$")
  data[[i]]$membran<-ifelse(data[[i]]$sensor_nr %in% membran_nrs[[i]],"mit membran","ohne")
  data[[i]]$kleber<-kleber[[i]][data[[i]]$sensor_nr]
  }


ggplot(data[[1]])+geom_line(aes(date,CO2,col=membran,linetype=sensor_nr),lwd=1)
ggplot(data[[2]])+geom_line(aes(date,CO2,col=kleber,linetype=sensor_nr),lwd=1)
ggplot(data[[3]])+geom_line(aes(date,CO2,col=kleber,linetype=sensor_nr),lwd=1)
versuch4<-ggplot(data[[4]])+geom_line(aes(date,CO2,col=kleber,linetype=sensor_nr),lwd=1)
versuch5<-ggplot(data[[5]])+geom_line(aes(date,CO2,col=membran,linetype=sensor_nr),lwd=1)
versuch6<-ggplot(data[[6]])+geom_line(aes(date,CO2,col=membran,linetype=sensor_nr),lwd=1)
#verzögerung 15 min
versuch4+geom_vline(xintercept=ymd_hms(c("2020-01-30 10:00:00","2020-01-30 10:15:00")))
versuch5+geom_vline(xintercept=ymd_hms(c("2020-02-03 11:30:00","2020-02-03 11:45:00")))+xlim(ymd_hms(c("2020-02-03 11:20:00","2020-02-03 12:45:00")))+geom_line(data=subset(data[[5]],sensor_nr=="00"),aes(date,CO2),col=1)
versuch6+geom_vline(xintercept=ymd_hms(c("2020-02-05 13:24:00","2020-02-05 13:39:00")))+xlim(ymd_hms(c("2020-02-05 13:20:00","2020-02-05 13:45:00")))+geom_line(data=subset(data[[6]],sensor_nr=="00"),aes(date,CO2),col=1)

n<-5
versuch4<-apply(versuche[[4]][,-1],2,filter,rep(1/n,n))
versuch4 <- as.data.frame(versuch4)
versuch4$date<-versuche[[4]]$date

v4<-ggplot(melt(versuch4,id="date",variable.name="sensor",value.name="CO2"))+geom_line(aes(date,CO2,col=sensor))
v4
diff<-ggplot(as.data.frame(versuch4))+
  geom_line(aes(date,CO2_Dyn_00-CO2_Dyn_14,col="00"))+
  geom_line(aes(date,CO2_Dyn_03-CO2_Dyn_14,col="03"))+
  geom_line(aes(date,CO2_Dyn_13-CO2_Dyn_14,col="13"))+
  geom_line(aes(date,CO2_Dyn_19-CO2_Dyn_14,col="19"))+
  geom_line(aes(date,CO2_Dyn_00-CO2_Dyn_09,col="00"))+
  geom_line(aes(date,CO2_Dyn_03-CO2_Dyn_09,col="03"))+
  geom_line(aes(date,CO2_Dyn_13-CO2_Dyn_09,col="13"))+
  geom_line(aes(date,CO2_Dyn_19-CO2_Dyn_09,col="19"))

diff_abl<-ggplot(as.data.frame(versuch4))+
  geom_line(aes(date,abs(c(NA,diff(CO2_Dyn_00-CO2_Dyn_14))),col="00"))+
  geom_line(aes(date,abs(c(NA,diff(CO2_Dyn_03-CO2_Dyn_14))),col="03"))+
  geom_line(aes(date,abs(c(NA,diff(CO2_Dyn_13-CO2_Dyn_14))),col="13"))+
  geom_line(aes(date,abs(c(NA,diff(CO2_Dyn_19-CO2_Dyn_14))),col="19"))

gridExtra::grid.arrange(v4,diff,diff_abl,ncol=1)

