hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
Sommerpfad <- paste0(datapfad,"Sommerzeit/")

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(Sommerpfad,full.names = F)
csvs <- lapply(paste0(Sommerpfad,files),read.csv,stringsAsFactors=F)
for(i in seq_along(csvs)){
date <- dmy_hms(paste0(csvs[[i]]$X,csvs[[i]]$X.1))
date_summer <- date-3600
csvs[[i]]$X.1 <- format(date_summer,"%H:%M:%S")
csvs[[i]]$X <- format(date_summer,"%d.%m.%Y")
write.csv(csvs[[i]],file=paste0(datapfad,files[i]),quote=F,row.names = F)
}

