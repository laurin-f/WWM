#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

datelim <- ymd_hms("2020.06.26 08:00:00","2020.06.26 13:15:00")
datelim2 <- ymd_hms("2020.06.26 12:30:00","2020.06.30 08:30:00")

data <- read_sampler(datelim=datelim)
data2 <- read_sampler(datelim=datelim2)
change1 <- ymd_hm("2020.06.26 10:15")
change1.2 <- ymd_hm("2020.06.26 12:30")
change2 <- ymd_hm("2020.06.29 16:15")
data$pos <- NA
data$pos[data$date > change1 + 1800 & data$date < change1.2] <- "2"
data$pos[data$date < change - 1800 | data$date > change1.2] <- "1"
data_agg <- aggregate(data[,paste0("CO2_smp",1:2)],list(pos=data$pos,tiefe=data$tiefe),mean,na.rm=T)
for(i in unique(data2$tiefe)){
data2$CO2_smp2[is.na(data2$CO2_smp2)&data2$tiefe == i] <- max(data2$CO2_smp2[data2$tiefe==i],na.rm = T)
}
data2$pos <- NA
data2$pos[data2$date > change2 + 1800] <- "2"
data2$pos[data2$date < change2 - 1800] <- "1"
data2_agg <- aggregate(data2[,paste0("CO2_smp",1:2)],list(pos=data2$pos,tiefe=data2$tiefe),mean,na.rm=T)

max(data$date)
ggplot(subset(data,tiefe!=0))+
  geom_vline(xintercept = change1.2)+
  annotate("text",x=change1,y=Inf,label="vertauscht",hjust=0,vjust=1)+
  geom_vline(xintercept = change1)+
  geom_hline(data = subset(data_agg,pos=="1" & tiefe != 0),aes(yintercept = CO2_smp1,col="smp1 pos inj"))+
  geom_hline(data = subset(data_agg,pos=="1"& tiefe != 0),aes(yintercept = CO2_smp2,col="smp2 pos ref"))+
  geom_line(aes(date,CO2_smp1,col="smp1"))+
  geom_line(aes(date,CO2_smp2,col="smp2"))+
  facet_wrap(~tiefe)+ggsave(paste0(plotpfad,"sampler_pos_vergleich.pdf"),width=9,height=9)

ggplot(data_agg)+
  geom_line(aes(CO2_smp1,tiefe,col=paste("smp1_pos",pos)))+
  geom_line(aes(CO2_smp2,tiefe,col=paste("smp2_pos",pos)))


ggplot(subset(data2,tiefe!=0))+
  geom_vline(xintercept = change2)+
  annotate("text",x=change2,y=Inf,label="vertauscht",hjust=0,vjust=1)+
  #geom_hline(data = subset(data2_agg,pos=="1" & tiefe != 0),aes(yintercept = CO2_smp1,col="smp1 pos inj"))+
  #geom_hline(data = subset(data2_agg,pos=="1"& tiefe != 0),aes(yintercept = CO2_smp2,col="smp2 pos ref"))+
  geom_line(aes(date,CO2_smp1,col="smp1"))+
  geom_line(aes(date,CO2_smp2,col="smp2"))+
  facet_wrap(~tiefe)+ggsave(paste0(plotpfad,"sampler_pos_vergleich2.pdf"),width=12,height=9)

ggplot(data2_agg)+
  geom_line(aes(CO2_smp1,tiefe,col=paste("smp1_pos",pos)),orientation = "y")+
  geom_line(aes(CO2_smp2,tiefe,col=paste("smp2_pos",pos)),orientation = "y")
