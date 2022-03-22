#test
#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")

datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_FVAgarten <- paste0(hauptpfad,"Daten/aufbereiteteDaten/FVA_Garten/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","ggforce","readODS")
check.packages(packages)

pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)
################
#daten einlesen
#################
datelim_falsch <- ymd_h("2021.07.21 06","2021.07.24 03")
datelim <- ymd_h("2022.02.28 01")

#range1 <- ymd_h(c("2021.04.01 01","2021.04.28 00"))
data_probe1u2falsch <- read_sampler("sampler1u2",datelim = datelim_falsch, format = "long")
data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")
t_diff <- min(data_probe1u2$date)-(max(data_probe1u2falsch$date)-10*60)
data_probe1u2falsch$date <- data_probe1u2falsch$date + t_diff
data_probe1u2 <- rbind(data_probe1u2falsch,data_probe1u2)
data_probe1u2$tiefe <- abs(data_probe1u2$tiefe)

# data_probe3 <-  read_sampler("sampler3",datelim = datelim, format = "long")
# range(data_probe3$date)
# range(data_probe1u2$date)
# names(data_probe3)
# data_probe3_na <- subset(data_probe3,!is.na(CO2))
# ggplot(data_probe3_na)+geom_line(aes(date,CO2,col=as.factor(tiefe)))+
#   geom_vline(xintercept = ymd_h("2022.03.03 12","2022.03.03 17"))+
#   xlim(ymd_h("2022.03.02 00","2022.03.05 17"))

ggplot(data_probe1u2)+
  geom_line(aes(date,CO2_smp1,col=as.factor(tiefe),linetype="smp1"))+
  geom_line(aes(date,CO2_smp2,col=as.factor(tiefe),linetype="smp2"))

ggplot(data_probe1u2)+
  geom_rect(data=pp_chamber[1,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"))+
  geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe)))+
  #geom_line(aes(date,CO2_smp1,col=as.factor(tiefe),linetype="smp1"))+
  #geom_vline(xintercept = ymd_h("2022.03.03 12","2022.03.03 17"))+
  xlim(ymd_h("2022.03.02 00","2022.03.05 17"))+
  theme_bw()+
  labs(alpha="",col="tiefe")+
  ggsave(paste0(plotpfad_PPchamber,"Messung1_2DPP.png"),width = 8,height = 5)


for(i in 1:nrow(pp_chamber)){
ggplot(data_probe1u2)+
  geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.2)+
  geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe)))+
  geom_line(aes(date,CO2_smp1,col=as.factor(-tiefe)))+
  #geom_vline(xintercept = ymd_hm("2022.03.07 12:00","2022.03.07 14:30"))+
  #geom_vline(xintercept = ymd_hm("2022.03.08 11:30","2022.03.08 15:30"))+
  xlim(c(pp_chamber$Start[i]-3600*24*1.5,pp_chamber$Ende[i]+3600*24))+
  theme_bw()+
  scale_fill_manual(values = "black")+
  labs(fill="",col="tiefe")+
  ggsave(paste0(plotpfad_PPchamber,"Messung",i,".png"),width = 8,height = 5)
}
  
