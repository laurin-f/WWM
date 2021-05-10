#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_FVAgarten <- paste0(hauptpfad,"Daten/aufbereiteteDaten/FVA_Garten/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

load(file = paste(datapfad_FVAgarten,"injectionrates.RData"))

datelim <- ymd_h("2021.03.30 15")

data_sampler1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")
data_sampler3 <-  read_sampler("sampler3",datelim = datelim, format = "long")

colnames(data_sampler1u2)
colnames(data_sampler3) <- str_replace(colnames(data_sampler3),"CO2","CO2_smp3")
data <- merge(data_sampler1u2,data_sampler3,all=T)


smp1 <- ggplot(data_sampler1u2)+
  geom_line(aes(date,CO2_smp1,col=as.factor(tiefe)))+guides(col=F)
smp2 <- ggplot(data_sampler1u2)+
  geom_line(aes(date,CO2_smp2,col=as.factor(tiefe)))
smp3 <- ggplot(data)+
  geom_line(aes(date,CO2_smp3,col=as.factor(tiefe)))+guides(col=F)
range(data$date,na.rm = T)
egg::ggarrange(smp1,smp2,smp3,ncol=1)
egg::ggarrange(smp1,smp2,bf_plot+xlim(range(data$date)),ncol=1)
