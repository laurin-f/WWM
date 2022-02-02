#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","svMisc")
check.packages(packages)

datelim <- "2022-01-31 10:55:00"
datelim <- "2022-02-01 13:40:00"

data <- read_PP(datelim = datelim)

ggplot(data)+geom_line(aes(date,p_Pa,col=id))


