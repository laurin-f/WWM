#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad_bf<- paste0(hauptpfad,"Daten/Urdaten/Bodenfeuchte_FVA_Garten/")

samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(datapfad_bf,pattern = "22-\\d{4}.xls$",full.names = T)



swc_ls <- lapply(files,readxl::read_xls,skip=2,col_types = c("date",rep("numeric",5)))
for(i in seq_along(swc_ls)){
  if(any(grepl("m³",colnames(swc_ls[[i]])))){
    swc_ls[[i]][,-1] <- swc_ls[[i]][,-1]*100
  }
  colnames(swc_ls[[i]])<- c("date",paste0("swc_",c("1a","1b","2a","2b","3")))
}
swc_ls[[1]]
swc <- do.call(rbind,swc_ls)
colnames(swc) 

#swc$date <- ymd_hms(swc$date)
for(i in 1:2){
  swc[paste0("swc_",i)] <- apply(swc[paste0("swc_",i,c("a","b"))],1,mean,na.rm=T)
}
# swc$swc_1 <- (swc$swc_1a + swc$swc_1b)/2
# 
# swc$swc_2 <- (swc$swc_2a + swc$swc_2b)/2
swc_long <- tidyr::pivot_longer(swc[,!grepl("swc_\\d[ab]",colnames(swc))],matches("swc_\\d$"),names_to = "tiefenstufe",values_to = "swc",names_prefix = "swc_")
swc_long$swc[which(swc_long$swc < -1)] <- NA

swc_long$tiefe <- as.numeric(swc_long$tiefenstufe) * 7

#load(file = paste(datapfad_FVAgarten,"HH2_long.RData"))
#swc_plot <- 
# ggplot(swc_long)+
#   geom_line(data=swc_long,aes(date,swc,col=as.factor(tiefe)))
#  geom_point(data=HH2_long,aes(date,SWC,col=as.factor(tiefe),shape=as.character(plot)))
  
swc_wide <- swc[grep("date|swc_\\d$",colnames(swc))] %>% as.data.frame()
swc_wide[,-1] <- sapply(swc_wide[,-1],as.numeric,simplify = T)
colnames(swc_wide) <- c("date",  "swc_21", "swc_7", "swc_14")
#swc_plot
save(swc_long,swc_wide,file = paste(datapfad_PP_Kammer,"swc_long.RData"))


