#pfade definieren


detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<-paste0(hauptpfad,"Daten/Metadaten/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
datapfad_FVAgarten <- paste0(hauptpfad,"Daten/aufbereiteteDaten/FVA_Garten/") 


#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","readxl","egg","dplyr")
check.packages(packages)

#datum
datelim <- vector("list")
datelim[[1]] <- ymd_hm(c("2021.04.26 11:00","2021.04.26 11:30"))
datelim[[2]] <- ymd_hm(c("2021.05.04 11:00","2021.05.04 11:30"))
datelim[[3]] <- ymd_hm(c("2021.05.10 10:40","2021.05.10 11:30"))

#daten einlesen
gga_ls <- lapply(datelim,function(x) read_GGA(datelim=x))

#einzelne messungen
ggplot(gga_ls[[1]])+geom_line(aes(date,CH4))
ggplot(gga_ls[[2]])+geom_line(aes(date,CH4))
ggplot(gga_ls[[3]])+geom_line(aes(date,CH4))

########################
#read minimum und maximum CH4 values
#######################################


CH4_gradient_ls <-lapply(gga_ls,function(x) data.frame(date=mean(x$date),CH4_soil=min(x$CH4,na.rm = T),CH4_atm=max(x$CH4,na.rm = T)))

CH4_gradient <- do.call(rbind,CH4_gradient_ls)

CH4_gradient <- CH4_gradient %>% mutate(CH4_d = CH4_atm - CH4_soil)
ggplot(CH4_gradient)+
  geom_point(aes(date, CH4_soil,col="soil"))+
  geom_point(aes(date, CH4_atm,col="atm"))

#ggplot(data)+geom_line(aes(date,CH4))
save(CH4_gradient,file = paste(datapfad_FVAgarten,"CH4_gradient.RData"))
