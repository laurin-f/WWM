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
datelim1 <- ymd_hm(c("2021.04.26 11:00","2021.04.26 11:30"))
datelim2 <- ymd_hm(c("2021.05.04 11:00","2021.05.04 11:30"))
datelim3 <- ymd_hm(c("2021.05.10 10:50","2021.05.10 11:30"))
data <- read_GGA(datelim=datelim3)
ggplot(data)+geom_line(aes(date,CO2))
