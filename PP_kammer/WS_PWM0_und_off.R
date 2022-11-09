#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
datapfad_ws<- paste0(hauptpfad,"Daten/Urdaten/windspeed_testo/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","data.table")
check.packages(packages)

bemerkungen <- readODS::read_ods(paste0(metapfad_PP,"PP_Kammer_Bemerkungen_hartheim.ods"))
dates <- bemerkungen[4:11,]
dates$Start <- dmy_hm(dates$Start)
dates$Ende <- dmy_hm(dates$Ende)
datelim <- range(c(dates$Start,dates$Ende)) + 60 *10 *c(-1,2)

data <- read_PP(datelim,corfac = F)
data <- subset(data,id != 6)
setDT(data)
data[,P_roll := RcppRoll::roll_mean(P,20,fill=NA),by = id]
dates$id <- rep(c("cal","PWM 0","off","PWM 0"),2)
ggplot(data)+
  geom_vline(data = dates, aes(xintercept = Start),alpha=0.4,linetype=2)+
  geom_line(aes(date,P_roll,col=factor(id)))+
  geom_text(data = dates, aes(Start,-1,label = id,hjust = 0))

