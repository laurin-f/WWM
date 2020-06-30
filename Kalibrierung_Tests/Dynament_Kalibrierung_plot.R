#pfade definieren

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Dynament/")

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

###################################################
#Daten laden

#Zeitrahmen festlegen
datelim<-c("2020-01-26 13:00:00","2020-01-28 10:00:00")

#db funktion
dynament_raw<-read_db("dynament.db","dynament_test",datelim,korrektur_dyn=F)
dynament_korr<-read_db("dynament.db","dynament_test",datelim,korrektur_dyn=T)
write.csv(dynament_korr,file=paste0(hauptpfad,"Dynament_Kalibrierung.txt"))
data_long <- tidyr::pivot_longer(dynament_raw,grep("CO2",colnames(dynament_korr)),names_to = "Sensor_Nr",names_prefix = "CO2_",values_to = "CO2")
ggplot(data_long)+geom_line(aes(date,CO2,col=Sensor_Nr))
