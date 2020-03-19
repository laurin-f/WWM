#pfade definieren
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

datelim<-ymd_hms(c("2019-10-25 9:00:00 UTC","2019-11-05 00:00:00 UTC"))
ylim<-c(360,680)

#subset
co2.dyn<-subset(co2.long,sensor=="dynament"&date>=datelim[1]&date<=datelim[2])

#time als minuten nach beginn
co2.dyn$time<-as.numeric(difftime(co2.dyn$date,min(co2.dyn$date)-60,units="mins"))
#nach stunden aggregieren
hour<-round_date(co2.dyn$date,"hours")
co2.dyn<-aggregate(co2.dyn,list(hour),mean)

co2.kurz<-subset(co2.dyn,date<="2019-10-29 17:00:00 UTC")

fm.sqrt<-glm(co2~sqrt(time),data=subset(co2.dyn,temp>15))
fm.log<-glm(co2~log(time),data=subset(co2.dyn,temp>15))
co2.dyn$preds.sqrt<-predict(fm.sqrt,newdata = data.frame(co2=co2.dyn$co2,time=co2.dyn$time))
co2.dyn$preds.log<-predict(fm.log,newdata = data.frame(co2=co2.dyn$co2,time=co2.dyn$time))

fm.sqrt<-glm(co2~sqrt(time),data=subset(co2.kurz,temp>20))
co2.kurz$preds.sqrt<-predict(fm.sqrt,newdata = data.frame(co2=co2.kurz$co2,time=co2.kurz$time))

# co2 ~ zeit mit temp als Farbe
ggplot(co2.dyn,aes(date,co2,col=temp))+geom_point()+
  geom_line(aes(y=preds.sqrt,linetype="sqrt"),col=1)+
  scale_color_viridis_c("temperature")+
  scale_linetype_manual("co2 ~ sqrt(time)",values=c(1,2))#+
  #ggsave(paste0(plotpfad,"temp_effekt_dynament2.pdf"),width=9,height = 6)

ggplot(co2.kurz,aes(date,co2,col=temp))+geom_point()+
  geom_line(aes(y=preds.sqrt,linetype="sqrt"),col=1)+
  scale_color_viridis_c("temperature")+
  scale_linetype_manual("co2 ~ sqrt(time)",values=c(1,2))#+
  
# Temp ~ zeit
ggplot(co2.kurz)+geom_point(aes(date,co2-preds.sqrt,col=temp))#+xlim(datelim)
ggplot(co2.dyn,aes(temp,co2-preds.sqrt,col=date))+geom_point()#+ggsave(paste0(plotpfad,"temp_effekt_co2_temp.pdf"),width=9,height = 6)#+xlim(datelim)

