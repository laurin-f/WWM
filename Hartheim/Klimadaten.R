#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"

klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)
dirs <- list.dirs(klimapfad,recursive = F)
files <- list.files(klimapfad,recursive = T,full.names = T)

colnames_list <- lapply(str_subset(files,"HARTHM_MET"),read.csv,skip=1,nrow=1,header=F,stringsAsFactors=F)
CR1000 <- lapply(str_subset(files,"HARTHM_MET"),read.csv,skip=3)
klima_data_list <- mapply(function(x,y){ 
  colnames(x) <- y
  return(x)
},x=CR1000,y=colnames_list,SIMPLIFY =F)
klima_data <- do.call("rbind",klima_data_list)
klima_data$date <- ymd_hms(klima_data$TIMESTAMP)
colnames_list[[1]]
ggplot(klima_data)+geom_line(aes(date,MET_Precip_Intensity_CS125_mmhr))
