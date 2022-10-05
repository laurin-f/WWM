detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","rdwd")
check.packages(packages)

#load(file = paste(datapfad_FVAgarten,"injectionrates.RData"))
#load(file = paste(datapfad_FVAgarten,"swc_long.RData"))
#load(file=paste(datapfad_FVAgarten,"chamber_flux.RData"))



#meta_link <- selectDWD("Freiburg",res="10_minutes",var=c("wind","precipitation"),per="meta_data")
#meta<-lapply(meta_link,dataDWD,dir=datapfad_FVAgarten,read=F)


link <- selectDWD("Freiburg",res="10_minutes",var=c("wind","precipitation"),per="recent")

klima_wind <- dataDWD(link[[1]],dir=datapfad_PP_Kammer,varnames=T,force=NA,overwrite = T)
klima_P <- dataDWD(link[[2]],dir=datapfad_PP_Kammer,varnames=T,force=T,overwrite = T)

klima <- merge(klima_P[,c(2,4,5,6)],klima_wind[,c(2,4,5)])
klima$date <- ymd_hms(klima$MESS_DATUM)
klima <- klima[,-1]

range <- ymd_hms(c("2022-03-01 00:00:00 UTC"))
klima <- subset(klima,date > range[1])
colnames(klima) <- c("P_dauer","P_mm","P_ind","wind","windrichtung","date")


klima$P24tot <- RcppRoll::roll_sumr(klima$P_mm,n=6*24,fill=NA)

#ggplot(klima)+
#  geom_line(aes(date,wind))
#ggplot(klima)+
#  geom_line(aes(date,P_mm))
save(klima,file = paste(datapfad_PP_Kammer,"klima_DWD.RData"))


