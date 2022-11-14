hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
datapfad_PP<- paste0(hauptpfad,"Daten/Urdaten/PP_Arduino/")

Sommerpfad <- paste0(datapfad_PP,"Sommerzeit/")


detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)

packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)


files <- list.files(Sommerpfad,full.names = F)
csvs <- lapply(paste0(Sommerpfad,files),read.table,sep = ";",stringsAsFactors=F)


for(i in seq_along(csvs)){
  date_summer <- ymd_hms(str_remove(csvs[[i]]$V7,".0$"))
  date <- date_summer-3600
  
  csvs[[i]]$V7 <- paste0(date,".0")
  colnames(csvs[[i]])
  write.table(csvs[[i]],file=paste0(datapfad_PP,files[i]),quote=F,sep=";",row.names = F,col.names = F)
}
