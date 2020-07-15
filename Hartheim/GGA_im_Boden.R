#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

datelim <- ymd_hm("2020.07.06 14:00","2020.07.08 13:00")
datelim2 <- ymd_hm("2020.07.08 16:30","2020.07.10 10:20")
datelim3 <- ymd_hm("2020.07.10 13:00","2020.07.10 19:00")
datelim_50m_1 <- ymd_hm("2020.07.10 11:55","2020.07.10 12:50")#micro bei T4 (siehe Foto) 
datelim_50m_2 <- ymd_hm("2020.07.14 16:49","2020.07.14 18:00")#micro bei Haupthütte



gga1 <- read_db("GGA.db","gga",datelim)
gga2 <- read_db("GGA.db","gga",datelim2)
gga3 <- read_db("GGA.db","gga",datelim3)
micro1 <- read_db("GGA.db","micro",datelim)
micro2 <- read_db("GGA.db","micro",datelim2)
micro3 <- read_db("GGA.db","micro",datelim3)
micro <- rbind(micro1,micro2,micro3)
gga <- rbind(gga1,gga2,gga3)

micro_50m_1 <- read_db("GGA.db","micro",datelim_50m_1)
micro_50m_2 <- read_db("GGA.db","micro",datelim_50m_2)
ggplot(micro_50m_1)+geom_line(aes(date,CO2))
ggplot(micro_50m_2)+geom_line(aes(date,CO2))
ggplot(micro)+geom_line(aes(date,CO2))

gga_50m_1 <- read_db("GGA.db","gga",datelim_50m_1)
gga_50m_2 <- read_db("GGA.db","gga",datelim_50m_2)
ggplot(gga_50m_1)+geom_line(aes(date,CO2))
ggplot(gga_50m_2)+geom_line(aes(date,CO2))
ggplot()+
  geom_line(data= gga_50m_1,aes(date,CO2))+
  geom_line(data= micro_50m_1,aes(date,CO2))
ggplot()+
  geom_line(data= gga_50m_2,aes(date,CO2))+
  geom_line(data= micro_50m_2,aes(date,CO2))
######################################
#loess
micro$date_int <- as.integer(micro$date)
fm_loess <- loess(CO2 ~ date_int,micro,span=0.1)
micro$CO2_loess <- predict(fm_loess)
micro$CO2_rauschen <- micro$CO2 - micro$CO2_loess
fm_loess <- loess(CH4 ~ date_int,micro,span=0.1)
micro$CH4_loess <- predict(fm_loess)
micro$CH4_rauschen <- micro$CH4 - micro$CH4_loess

gga$date_int <- as.integer(gga$date)
fm_loess <- loess(CO2 ~ date_int,gga,span=0.1)
gga$CO2_loess <- predict(fm_loess)
gga$CO2_rauschen <- gga$CO2 - gga$CO2_loess

########################################
#plots
##########################################

T_plot <- ggplot(micro)+geom_line(aes(date,AmbT_C))
T_plot_gga <- ggplot(gga)+geom_line(aes(date,AmbT_C))

Co2_plot <- ggplot(micro)+
  geom_line(aes(date,CO2,col="raw"))+
  geom_line(aes(date,CO2_loess,col="loess"),lwd=1)+
  labs(col="",title="CO2")
rauschplot <- ggplot(micro)+geom_line(aes(date,CO2_rauschen))
CH4_plot <- ggplot(micro)+
  geom_line(aes(date,CH4,col="raw"))+
  geom_line(aes(date,CH4_loess,col="loess"),lwd=1)+
  labs(col="",title="CH4")
CH4rauschplot <- ggplot(micro)+geom_line(aes(date,CH4_rauschen))


Co2_plot_gga <- ggplot(gga)+
  geom_line(aes(date,CO2,col="raw"))+
  geom_line(aes(date,CO2_loess,col="loess"),lwd=1)+
  labs(col="",title="tiefe 4cm")
rauschplot_gga <- ggplot(gga)+geom_line(aes(date,CO2_rauschen))
egg::ggarrange(Co2_plot,rauschplot,T_plot)

egg::ggarrange(Co2_plot,rauschplot,CH4_plot,CH4rauschplot,ncol=1)
egg::ggarrange(rauschplot,CH4rauschplot,T_plot)

egg::ggarrange(Co2_plot_gga,rauschplot_gga,T_plot_gga)
egg::ggarrange(rauschplot,T_plot)

gga_plot <- ggplot(gga)+geom_line(aes(date,CO2))
egg::ggarrange(gga_plot+geom_vline(xintercept = ymd_hm("2020.07.09 16:16")),T_plot)

ggamicro_plot <- ggplot()+
  geom_line(data= gga,aes(date,CO2,col="4 cm"))+
  geom_line(data= micro,aes(date,CO2,col="0 cm"))+
  labs(col="tiefe")
write

write.csv(micro,file=paste0(datapfad_harth,"gga_tiefe0cm.txt"))
write.csv(gga,file=paste0(datapfad_harth,"gga_tiefe4cm.txt"))
