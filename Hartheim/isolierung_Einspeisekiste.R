#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

datelim <- ymd_h("2020.05.29 13")

load(file=paste0(klimapfad,"klima_data.RData"))

data <- read_sampler(datelim=datelim)
data <- merge(data,klima,by="date",all.x = T)

heizung_aus <- ymd_h("2020.06.09 13")
isolierung <- ymd_h("2020.06.18 13")
heizung_an <- ymd_h("2020.06.24 13")
thermostat_veschoben <- ymd_h("2020.06.26 13")
min(data$date[!is.na(data$T_C)])
changes <- data.frame(date=ymd_h(c("2020.05.29 13","2020.06.09 13","2020.06.18 13","2020.06.24 13","2020.06.26 13")),label=c("ohne Isolierung","heimatte aus","isolierung","heizmatte\nan","thermostat\nverschoben"),stringsAsFactors = F)
 for(i in seq_along(changes$date)){
   data$label[data$date >= changes$date[i]] <- changes$label[i] 
 }
data$period <- approx(changes$date,seq_along(changes$label),data$date,method="constant",rule=1:2)$y
ggplot(data)+
  geom_vline(xintercept = changes$date)+
  geom_text(data =changes, aes(x = date,y=Inf,label = label),hjust=0,vjust=1)+
  geom_line(aes(date,T_C,col="kiste"))+
  geom_line(aes(date,Ta_2m,col="atm"))+
  ggsave(paste0(plotpfad,"isolierung_Kiste.pdf"),width=14,height=9)
lbl <- as_labeller(setNames(changes$label,seq_along(changes$label)))
ggplot(data)+
  geom_line(aes(date,T_C,col="kiste"))+
  geom_line(aes(date,Ta_2m,col="atm"))+
  facet_grid(~period,scales="free",labeller =lbl)

ggplot(subset(data,period%in%4))+
  geom_line(aes(date,T_C,col="kiste"))
ggplot(subset(data,period%in%5))+
  geom_line(aes(date,T_C,col="kiste"))
