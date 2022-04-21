hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")

chamber_arduino_pfad <- paste0(hauptpfad,"/Daten/Urdaten/Kammermessungen_Arduino/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)


pp_bemerkungen <- read_ods(paste0(metapfad_PP,"PP_Kammer_Bemerkungen.ods"))
pp_bemerkungen$Start <- dmy_hm(pp_bemerkungen$Start)

pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)+3600
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)+3600
i <- 9
micro <- read_GGA(table.name = "micro",datelim=t(pp_chamber[i,c("Start","Ende")]))
gga <- read_GGA(table.name = "gga",datelim=t(pp_chamber[i,c("Start","Ende")]))

pp_bemerk <- sub_daterange(pp_bemerkungen,range(micro$date),"Start")

datelim1 <- ymd_hm("2022.04.06 14:48","2022.04.06 14:54")
datelim2 <- ymd_hm("2022.04.06 16:00","2022.04.06 16:06")
#gesamt <- 
  ggplot(micro)+
  geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  geom_rect(xmin=datelim1[1],xmax=datelim1[2],ymin=-Inf,ymax=Inf,alpha=0.008)+
  geom_rect(xmin=datelim2[1],xmax=datelim2[2],ymin=-Inf,ymax=Inf,alpha=0.008)+
  geom_vline(data=pp_bemerk,aes(xintercept=Start))+
  geom_text(data=pp_bemerk[1,],aes(x=Start,y=Inf,label="Extra Kammerbelüftung angeschaltet"),hjust=0,vjust=1)+
  geom_line(aes(date,CO2))+
  labs(x="",y=expression(CO[2]~(ppm)),fill="")#+
#  xlim(ymd_hm("2022.04.13 13:40","2022.04.13 15:20"))




date_seq <- seq(min(micro$date),max(micro$date),by=1)
micro_2 <- merge(micro,data.frame(date=date_seq),all=T)
micro_2$CO2 <- imputeTS::na_interpolation(micro_2$CO2)

t_diff <- as.numeric(median(diff_time(micro_2$date,"secs"),na.rm = T))


#####################
#P_filter und PPC
fs <- 1 / round(t_diff,1)#1/s = Hz
fpass <- c(0.003,0.1)
wpass <- fpass / (fs /2)

bpfilter <- gsignal::butter(n=4,w=wpass,type="pass")

micro_2$CO2_filter <- gsignal::filtfilt(bpfilter,micro_2$CO2)
micro$CO2_roll <- RcppRoll::roll_mean(micro$CO2,40,fill=NA)
micro$CO2_fluct <- micro$CO2 - micro$CO2_roll
#datelim <- ymd_hm("2022.04.04 16:00","2022.04.04 18:00")


vorher <- ggplot(sub_daterange(micro_2,datelim1))+
  geom_line(aes(date,CO2_filter))+

  #geom_vline(data=pp_bemerkungen,aes(xintercept=Start))+
  scale_x_datetime(date_breaks = "1 mins",date_labels = "%H:%M")+
  labs(subtitle="ohne Belüftung",y=expression(CO["2 filter"]~(ppm)),x="")



nachher <- 
  ggplot(sub_daterange(micro_2,datelim2))+
  geom_line(aes(date,CO2_filter))+
  #geom_line(aes(date,CO2_filter))+
  #geom_vline(data=pp_bemerkungen,aes(xintercept=Start))+
  scale_x_datetime(date_breaks = "1 mins",date_labels = "%H:%M")+
  labs(subtitle="mit Belüftung",y=expression(CO["2 filter"]~(ppm)),x="")

vor_nach <- egg::ggarrange(vorher,nachher,ncol=2)

vor_nach <- ggpubr::ggarrange(vorher,nachher,ncol = 2)

ggpubr::ggarrange(gesamt,vor_nach,ncol = 1)+ggsave(paste0(plotpfad_PPchamber,"Kammerbelüftung_CO2_fluctuations.png"),width=8,height=8)

