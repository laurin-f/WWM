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
ggplot(micro)+
  geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
#  geom_point(aes(date,CO2))+
  geom_vline(data=pp_bemerk,aes(xintercept=Start))+
  geom_text(data=pp_bemerk,aes(x=Start,y=Inf,label=Bemerkung),hjust=0,vjust=1)+
  geom_line(aes(date,CO2))


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

datelim <- ymd_hm("2022.04.06 14:48","2022.04.06 14:55")
vorher <- ggplot(sub_daterange(micro_2,datelim))+
  geom_line(aes(date,CO2_filter))+

  geom_vline(data=pp_bemerkungen,aes(xintercept=Start))+
  xlim(datelim)

datelim <- ymd_hm("2022.04.06 16:00","2022.04.06 16:07")

nachher <- ggplot(sub_daterange(micro_2,datelim))+
  geom_line(aes(date,CO2_filter))+
  #geom_line(aes(date,CO2_filter))+
  geom_vline(data=pp_bemerkungen,aes(xintercept=Start))+
  xlim(datelim)

egg::ggarrange(vorher,nachher)
range(micro$date)
