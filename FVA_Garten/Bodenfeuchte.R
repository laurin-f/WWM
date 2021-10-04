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
datapfad_FVAgarten <- paste0(hauptpfad,"Daten/aufbereiteteDaten/FVA_Garten/") 
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(datapfad_bf,pattern = ".xls$",full.names = T)

swc_ls <- lapply(files,readxl::read_xls,skip=2,col_types = c("date",rep("numeric",5)))
for(i in seq_along(swc_ls)){
  if(any(grepl("m³",colnames(swc_ls[[i]])))){
    swc_ls[[i]][,-1] <- swc_ls[[i]][,-1]*100
  }
  colnames(swc_ls[[i]])<- c("date",paste("bf_",c("1a","1b","2a","2b","3a")))
}

swc <- do.call(rbind,swc_ls)
colnames(swc) <- c("date",paste("swc_",c("1a","1b","2a","2b","3a")))

#swc$date <- ymd_hms(swc$date)
swc <- subset(swc, date > ymd("2021.04.01"))

swc_long <- tidyr::pivot_longer(swc,matches("swc"),names_to = "tiefe",values_to = "swc",names_prefix = "swc_")
swc_long$swc[which(swc_long$swc < -1)] <- NA
swc_plot <- ggplot(swc_long)+
  geom_line(aes(date,swc,col=tiefe))

swc_plot
save(swc_long,file = paste(datapfad_FVAgarten,"swc_long.RData"))
