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


#ggplot(data)+geom_line(aes(date,WS))+
#  geom_hline(yintercept = 1)# WS durch Ventilatoren in PP-Kammer ohne PP ca. 1m/s



#ggplot(data)+geom_line(aes(date,T_C))
#test 12
data_ws <- data
save(data_ws,file = paste(datapfad_PP_Kammer,"data_ws.RData"))

date_seq <- seq(datelim[1],datelim[2],by=2)
ws_sub <- subset(data_ws,format(date,"%m.%d") %in% c("","04.11"))
ws_sub <- merge(ws_sub,data.frame(date=date_seq),all=T)
datelim_ws1 <- ymd_hm("2022.04.11 11:35","2022.04.11 12:00")
datelim_ws2 <- ymd_hm("2022.04.12 15:17","2022.04.12 15:25")
ws_sub1 <- sub_daterange(data_ws,datelim_ws2)
ws_sub2 <- sub_daterange(data_ws,datelim_ws1)
ws_sub <- rbind(ws_sub1,ws_sub2)
ggplot(ws_sub)+
  geom_line(aes(date,WS),alpha=0.1)+
  geom_point(aes(date,RcppRoll::roll_median(WS,20,fill=NA)))
