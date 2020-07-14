#pfade definieren


detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<-paste0(hauptpfad,"Daten/Metadaten/")

#flux.kammer<-function(ort="Schauinsland",
#                      messnr){

#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","readxl","egg","dplyr")
check.packages(packages)
flux <- list()

for(i in c(1:3,6:7)){
flux[[i]] <- chamber_flux(mess_dir = "Hartheim",aggregate = F,closing_before=30,closing_after=30,messnr = i,t_max=3)
}
flux[[4]] <- chamber_flux(mess_dir = "Hartheim",aggregate = F,closing_before=45,closing_after=45,messnr = 4,adj_openings=T,t_max=3)
flux[[5]] <- chamber_flux(mess_dir = "Hartheim",aggregate = F,closing_before=20,closing_after=20,messnr = 5,adj_openings=T,t_max=3)
flux[[8]] <- chamber_flux(mess_dir = "Hartheim",aggregate = F,closing_before=15,closing_after=15,messnr = 8,adj_openings=T,t_max=3)

CO2_flux_list <- lapply(flux, function(x) x[["CO2"]][[1]])
CH4_flux_list <- lapply(flux, function(x) x[["CH4"]][[1]])
CO2_flux <- do.call(rbind,CO2_flux_list)
CH4_flux <- do.call(rbind,CH4_flux_list)

CH4_split <- flux[[1]][["CH4"]][[2]]

CO2_flux$day <- as_date(CO2_flux$date)
CH4_flux$day <- as_date(CH4_flux$date)
CO2_flux_agg <- CO2_flux %>%
  group_by(day,kammer) %>%
  summarise(CO2flux = mean(ml_per_min_m2),CO2flux_min = min(ml_per_min_m2),CO2flux_max = max(ml_per_min_m2),date = mean(date))
CH4_flux_agg <- CH4_flux %>%
  group_by(day,kammer) %>%
  summarise(CH4flux = mean(ml_per_min_m2),CH4flux_min = min(ml_per_min_m2),CH4flux_max = max(ml_per_min_m2),date = mean(date))
Kammer_flux <- merge(CO2_flux_agg,CH4_flux_agg)

range(CO2_flux$mol_per_min_m2)
mean(CO2_flux$date)
range(CO2_flux$ml_per_min_m2[round_date(CO2_flux$date,"days") == ymd("2020-06-09")])
range(CO2_flux$ml_per_min_m2)
ggplot(CO2_flux)+geom_line(aes(date,ml_per_min_m2,col=kammer))
ggplot(CO2_flux)+geom_boxplot(aes(kammer,ml_per_min_m2,fill=kammer))
ggplot(CH4_flux)+geom_line(aes(date,ml_per_min_m2,col=kammer))
ggplot(CH4_split)+geom_point(aes(date,CH4,col=kammer))
ggplot(CH4_split)+geom_point(aes(zeit,CH4_tara,col=kammer))

ggplot(Kammer_flux)+
  geom_line(aes(date,CO2flux,col=kammer))+
  geom_ribbon(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max,fill=kammer),alpha=0.3)
ggplot(Kammer_flux)+
  geom_line(aes(date,CH4flux,col=kammer))+
  geom_ribbon(aes(x=date,ymin=CH4flux_min,ymax=CH4flux_max,fill=kammer),alpha=0.3)

#Metadaten laden



####################################################################
sampler1 <- read_sampler("sampler1",datelim = c("2020-05-14 11:45:00 UTC", "2020-05-15 20:00:00 UTC"), format = "long")
sampler2 <- read_sampler("sampler2",datelim = c("2020-05-14 11:45:00 UTC", "2020-05-15 20:00:00 UTC"), format = "long")


ggplot(subset(sampler1,!is.na(tiefe)))+geom_line(aes(date,CO2,col=as.factor(tiefe)))#+
xlim(ymd_h("2020-05-15 10","2020-05-15 18"))
ggplot(subset(sampler2,!is.na(tiefe)))+geom_line(aes(date,CO2,col=as.factor(tiefe)))

ggplot(subset(data,variable=="T_C"))+geom_line(aes(date,CO2))
