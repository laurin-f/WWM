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
datelim <- ymd_h(c("2021.04.26 11","2021.04.26 12"))
data <- read_GGA(datelim=datelim)
ggplot(data)+geom_line(aes(date,CO2))
  flux <- chamber_flux(mess_dir = "FVA_Garten",aggregate = F,closing_lim=20,t_min=1,messnr = 1,t_max=1.5,t_init=0.5)

ggplot(flux)+geom_point(aes(date,CO2_ml_per_min,col=kammer))
  