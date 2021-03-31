


#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_ms <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Methodenpaper/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
aufbereitete_ds<-paste0(hauptpfad,"Daten/aufbereiteteDaten/DS_Labor/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","ggpubr","png","cowplot","magick","tictoc","RcppRoll")
check.packages(packages)
theme_set(theme_classic())

#daten laden
load(file=paste0(klimapfad,"klima_data.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))

###############
#fast rollapply
###############


##zoo
width <- 60*4
tic()
data$Wind <- zoo::rollapply(data$WindVel_30m_ms,width,mean,fill=NA)
toc()

## statts::filter
tic()
for(i in 1:5){
data$Wind_filter <- stats::filter(data$WindVel_30m_ms, rep(1 / width, width), sides = 2)
}
toc()

tic()
for(i in 1:5){
data$Wind_dt <- data.table::frollmean(data$WindVel_30m_ms,width,fill=NA,align = "center")
}
toc()

tic()
for(i in 1:5){
data$Wind_rcpp <- RcppRoll::roll_mean(data$WindVel_30m_ms,width,fill=NA)
}
toc()


ggplot(subset(data,Position == 8))+
  geom_line(aes(date,Wind,col="zoo"))+
  geom_line(aes(date,Wind_filter,col="filter"))+
  geom_line(aes(date,Wind_dt,col="dt"))+
  geom_line(aes(date,Wind_rcpp,col="rcpp"))+
  xlim(ymd_h(c("2020.07.19 00","2020.07.19 12")))
