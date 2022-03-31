#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
datapfad_WS<- paste0(hauptpfad,"Daten/Urdaten/windspeed_testo/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(datapfad_WS,full.names = T)

data_ls <- lapply(files,read.csv2,
               skip=1,
               stringsAsFactors = F,
               col.names = c("Datum_chr","T_C","WS",""),
               colClasses = c("character","numeric","numeric","NULL"),
               na.strings = c("testo405i","---"))

data <- do.call(rbind,data_ls)

data$date <- dmy_hms(data$Datum_chr)
data <- data[!is.na(data$date),]
data <- data[!duplicated(data$date),]


ggplot(data)+geom_line(aes(date,WS))+
  geom_hline(yintercept = 1)# WS durch Ventilatoren in PP-Kammer ohne PP ca. 1m/s



ggplot(data)+geom_line(aes(date,T_C))
#test 12
data_ws <- data
save(data_ws,file = paste(datapfad_PP_Kammer,"data_ws.RData"))
