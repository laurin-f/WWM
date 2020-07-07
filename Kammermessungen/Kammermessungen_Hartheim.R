#pfade definieren


detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<-paste0(hauptpfad,"Daten/Metadaten/")

#flux.kammer<-function(ort="Schauinsland",
#                      messnr){

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","readxl","egg")
check.packages(packages)
flux <- list()
for(i in 1:3){
flux[[i]] <- chamber_flux(mess_dir = "Hartheim",aggregate = F,closing_before=30,closing_after=30,messnr = i)
}
flux[[4]] <- chamber_flux(mess_dir = "Hartheim",aggregate = F,closing_before=45,closing_after=45,messnr = 4,adj_openings=T,t_max=3)

CO2_flux_list <- lapply(flux, function(x) x[["CO2"]][[1]])
CH4_flux_list <- lapply(flux, function(x) x[["CH4"]][[1]])
CO2_flux <- do.call(rbind,CO2_flux_list)
CH4_flux <- do.call(rbind,CH4_flux_list)
CO2_flux1 <- flux[["CO2"]][[1]]
CH4_flux1 <- flux[["CH4"]][[1]]

CH4_split <- flux[[1]][["CH4"]][[2]]



range(CO2_flux$mol_per_min_m2)
mean(CO2_flux$date)
range(CO2_flux$ml_per_min_m2)

ggplot(CO2_flux)+geom_line(aes(date,ml_per_min_m2,col=kammer))
ggplot(CH4_flux)+geom_line(aes(date,ml_per_min_m2,col=kammer))
ggplot(CH4_split)+geom_point(aes(date,CH4,col=kammer))
ggplot(CH4_split)+geom_point(aes(zeit,CH4_tara,col=kammer))


#Metadaten laden



####################################################################
sampler1 <- read_sampler("sampler1",datelim = c("2020-05-14 11:45:00 UTC", "2020-05-15 20:00:00 UTC"), format = "long")
sampler2 <- read_sampler("sampler2",datelim = c("2020-05-14 11:45:00 UTC", "2020-05-15 20:00:00 UTC"), format = "long")


ggplot(subset(sampler1,!is.na(tiefe)))+geom_line(aes(date,CO2,col=as.factor(tiefe)))#+
xlim(ymd_h("2020-05-15 10","2020-05-15 18"))
ggplot(subset(sampler2,!is.na(tiefe)))+geom_line(aes(date,CO2,col=as.factor(tiefe)))

ggplot(subset(data,variable=="T_C"))+geom_line(aes(date,CO2))
