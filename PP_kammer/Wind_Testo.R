#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
datapfad_WS<- paste0(hauptpfad,"Daten/Urdaten/windspeed/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(datapfad_WS,full.names = T)

file <- files[1]
data <- read.csv2(file,
                  skip=1,
                  stringsAsFactors = F,
                  col.names = c("Datum_chr","T_C","WS",""))[,-4]
data$date <- dmy_hms(data$Datum_chr)
data <- data[!is.na(data$date),]
class(data$T_C)

data$T_C <- str_replace(data$T_C,",",".") %>% as.numeric()

ggplot(data)+geom_line(aes(date,WS))+
  geom_hline(yintercept = 1)# WS durch Ventilatoren in PP-Kammer ohne PP ca. 1m/s


ggplot(data)+geom_line(aes(date,T_C))

