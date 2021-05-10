#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad_bf<- paste0(hauptpfad,"Daten/Urdaten/Bodenfeuchte_FVA_Garten/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(datapfad_bf,pattern = ".xls$",full.names = T)

data_ls <- lapply(files,readxl::read_xls,skip=2,col_types = c("date",rep("numeric",5)))
for(i in seq_along(data_ls)){
  if(any(grepl("m³",colnames(data_ls[[i]])))){
    data_ls[[i]][,-1] <- data_ls[[i]][,-1]*100
  }
  colnames(data_ls[[i]])<- c("date",paste("bf_",c("1a","1b","2a","2b","3a")))
}

data <- do.call(rbind,data_ls)
colnames(data) <- c("date",paste("bf_",c("1a","1b","2a","2b","3a")))

#data$date <- ymd_hms(data$date)
data <- subset(data, date > ymd("2021.04.01"))
range(data$date)
data_long <- tidyr::pivot_longer(data,matches("bf"),names_to = "tiefe",values_to = "bf",names_prefix = "bf_")
bf_plot <- ggplot(data_long)+
  geom_line(aes(date,bf,col=tiefe))
