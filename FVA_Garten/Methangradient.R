#pfade definieren


detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<-paste0(hauptpfad,"Daten/Metadaten/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
#flux.kammer<-function(ort="Schauinsland",
#                      messnr){


#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","readxl","egg","dplyr")
check.packages(packages)
flux <- list()
datelim <- vector("list")
datelim[[1]] <- ymd_hm(c("2021.04.26 11:00","2021.04.26 11:30"))
datelim[[2]] <- ymd_hm(c("2021.05.04 11:00","2021.05.04 11:30"))
datelim[[3]] <- ymd_hm(c("2021.05.10 10:40","2021.05.10 11:30"))
gga_ls <- lapply(datelim,function(x) read_GGA(datelim=x))
CH4_gradient_ls <-lapply(gga_ls,function(x) data.frame(date=mean(x$date),CH4_soil=min(x$CH4,na.rm = T),CH4_atm=max(x$CH4,na.rm = T)))
CH4_gradient <- do.call(rbind,CH4_gradient_ls)
ggplot(CH4_gradient)+
  geom_point(aes(date, CH4_soil,col="soil"))+
  geom_point(aes(date, CH4_atm,col="atm"))
ggplot(data)+geom_line(aes(date,CH4))
