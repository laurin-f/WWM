library(pkg.WWM)

#pfade definieren
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
codepfad<-paste0(hauptpfad,"Programme/Eigenentwicklung/Funktionen/")
source(paste0(codepfad,"read_db.R"))

micro<-read_GGA("micro")
gga<-read_GGA("gga")

source(paste0(codepfad,"read_dynament.R"))

#datensatz für ggplot erstellen
gga.2<-data.frame(date=gga$date,co2=gga$CO2,sensor="gga")
micro.2<-data.frame(date=micro$date,co2=micro$CO2,sensor="micro")
dynament.2<-data.frame(date=c(dynament.stecker$date,dynament$date),
                       co2=c(dynament.stecker$CO2,dynament$CO2),
                       sensor="dynament",
                       temp=c(dynament.stecker$`Hygro S3 Temperatur`,dynament$`Hygro S3 Temperatur`))
gga.2<-merge(gga.2,dynament.2[,c("date","temp")],by="date",all.x = T)
micro.2<-merge(micro.2,dynament.2[,c("date","temp")],by="date",all.x = T)
#long format
co2.long<-rbind(gga.2,micro.2,dynament.2)
#wide format
co2.wide.1<-merge(micro.2[,colnames(micro.2)!="sensor"],dynament.2[,colnames(dynament.2)!="sensor"],by="date",all = T)
co2.wide<-merge(co2.wide.1,gga.2[,colnames(gga.2)!="sensor"],by="date",all=T)
colnames(co2.wide)<-str_replace_all(colnames(co2.wide),c("(?<!x|y|e)$"=".gga","x"="micro",
                                                         "y"="dynament"))

#limits für plot
#######################
vorversuche<-read_xlsx(paste0(hauptpfad,"Daten/Urdaten/Vorversuche/Vorversuche.xlsx"))

datelims<-as.data.frame(t(vorversuche[,2:3]))
for(i in 1:ncol(datelims)){
  datelims[,i]<-ymd_hms(datelims[,i])
}

#temp dyn mic
#ganzer versuch
datelim<-ymd_hms(c("2019-10-17 08:00:00 UTC","2019-10-22 10:30:00 UTC"))
#erster kurzer 
datelim<-ymd_hms(c("2019-10-17 08:00:00 UTC","2019-10-17 14:00:00 UTC"))
#zweiter länger
datelim<-ymd_hms(c("2019-10-17 14:00:00 UTC","2019-10-18 10:30:00 UTC"))
#dritter
datelim<-ymd_hms(c("2019-10-21 09:00:00 UTC","2019-10-22 10:30:00 UTC"))

plot(co2.wide$co2.micro,co2.wide$co2.dynament)
#################
#temp dyn zu
#erster

#datelim<-ymd_hms(c("2019-10-22 12:00:00 UTC","2019-10-23 12:30:00 UTC"))

ylim<-c(300,600)
co2.sub<-subset(co2.wide,date>=datelim[1]&date<=datelim[2])

# Temp ~ zeit
ggplot(co2.long)+geom_line(aes(date,temp,col=sensor))+xlim(datelim)


#ggplot
ggplot(co2.long)+geom_line(aes(date,co2,col=sensor),lwd=1)+
  xlim(datelims[,4])+theme_classic()+scale_color_manual(values=1:3)+
  labs(x="Zeit",y=expression("CO"[2]*" [ppm]"))+ylim(ylim)#+
#ggsave(paste0(plotpfad,"co2_sensoren_test.pdf"),width=9,height = 6)

# co2 ~ zeit mit temp als Farbe
ggplot(co2.long)+geom_point(aes(date,co2,col=temp))+xlim(datelim)+ylim(c(340,450))+scale_color_viridis_c()

#co2 ~ temp
ggplot(co2.sub)+geom_point(aes(temp.dynament,co2.dynament))+ylim(c(340,450))
ggplot(co2.sub)+geom_point(aes(temp.dynament,co2.micro))+ylim(c(200,600))

#scatterplot

lims<- c(300,600)

ggplot(co2.sub)+
  geom_point(aes(co2.micro,co2.dynament,col=temp.dynament),size=2,alpha=0.5)+
  #xlim(c(380,500))+ylim(c(300,410))+
  xlim(lims)+ylim(lims)+
    geom_abline(slope=1,intercept=0)+scale_color_viridis_c()+theme_classic()#+
#ggsave(paste0(plotpfad,"co2_sensoren_scatterplot.pdf"),width=9,height = 6)
min(co2.wide$temp.dynament,na.rm=T)

plot(co2$co2.x,co2$co2.y,xlim = lims,ylim = lims,
     xlab = co2$sensor.x[1],ylab = "dynament")
abline(0,1)

lims2<- c(350,1500)
plot(co2$co2.x,co2$co2,xlim = lims2,ylim = lims2,
     xlab = co2$sensor.x[1],ylab = "gga")
abline(0,1)

max(micro$date)
