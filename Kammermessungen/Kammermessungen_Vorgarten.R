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











####################################################################
sampler1 <- read_sampler("sampler1",datelim = c("2020-05-14 11:45:00 UTC", "2020-05-15 20:00:00 UTC"), format = "long")
sampler2 <- read_sampler("sampler2",datelim = c("2020-05-14 11:45:00 UTC", "2020-05-15 20:00:00 UTC"), format = "long")


ggplot(subset(sampler1,!is.na(tiefe)))+geom_line(aes(date,CO2,col=as.factor(tiefe)))#+
  xlim(ymd_h("2020-05-15 10","2020-05-15 18"))
ggplot(subset(sampler2,!is.na(tiefe)))+geom_line(aes(date,CO2,col=as.factor(tiefe)))

ggplot(subset(data,variable=="T_C"))+geom_line(aes(date,CO2))
