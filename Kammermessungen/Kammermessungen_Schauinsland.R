
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



flux_schaui <- chamber_flux(mess_dir = "Schauinsland",aggregate = F,
                            closing_lim=0,
                            opening_lim=0,
                            t_min=2,adj_openings=T)



ggplot(flux_schaui)+geom_line(aes(date,CO2_ppm_per_min,col=kammer))
