#pfade definieren

rm(list=ls())
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<-paste0(hauptpfad,"Daten/Metadaten/")

#flux.kammer<-function(ort="Schauinsland",
#                      messnr){

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","readxl","egg")
check.packages(packages)

#Metadaten laden

  Kammer<-read_xlsx(paste0(metapfad,"/Kammermessungen/Kammer_Meta.xlsx"),sheet="manuelle Kammer")

messnr <- 1
GGA <- "gga"
  Messungen<-read_xlsx(paste0(metapfad,"Vorgarten/Kammermessungen.xlsx"))

beginn<-ymd_hm(paste(Messungen$Datum[messnr],format(Messungen$beginn[messnr],"%H:%M")))
ende<-ymd_hm(paste(Messungen$Datum[messnr],format(Messungen$ende[messnr],"%H:%M")))

data.raw<-read_db("GGA.db",GGA,datelim=c(beginn,ende))



#dataset<-split.chamber(data=data.agg,closing=5,opening = -30,t_max=9)
dataset<-split_chamber(data=data.raw,
                       closing_before  = 20,
                       closing_after  = 40,
                       opening_before = 10,
                       opening_after = 0,
                       t_max=4,
                       t_init = 1,
                       t_min = 2)


datelim <- range(c(Pumpzeiten$start, Pumpzeiten$ende),na.rm = T)
datelim[2] <- datelim[2]+3600*12

data <- read_sampler("sampler1",datelim = datelim, format = "long")
data_wide <- read_sampler("sampler1",datelim = c("2020-05-14 11:45:00 UTC", "2020-05-15 08:00:00 UTC"), format = "wide")

