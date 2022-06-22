hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Kammermessungen_Arduino"
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)


injection_dates <- read_ods(paste0(metapfad_PP,"injektionen.ods"))
injection_dates$Start <- dmy_hm(injection_dates$Start)
injection_dates$Ende <- dmy_hm(injection_dates$Ende)

#i <- nrow(injection_dates)

#datelim <- c(injection_dates$Start[i],injection_dates$Ende[i])
# datelim <- ymd_hm("22/05/09 13:00","22/05/12 16:00")
# datelim <- ymd_hm("22/04/12 00:00","22/04/15 16:00")
# dates_ls <- vector("list",nrow(injection_dates))
# for(i in 1:nrow(injection_dates)){
#   dates_ls[[i]] <- c(injection_dates$Start[i],injection_dates$Ende[i])
# }
#liste mit Start und Endzeitpunkten der injektionen
dates_ls <- split(injection_dates[,-3],nrow(injection_dates)) %>% 
  lapply(.,function(x) as_datetime(as.numeric(x)))

inj_ls <- lapply(dates_ls,injection_arduino,                                                               plot="facets",
                 return_ls = F,
                 t_init=2)
inj <- do.call(rbind,inj_ls)
#inj <- injection_arduino(datelim,plot="flux",return_ls = T,t_init=2)
A_inj <- 1^2*pi /10^6 #m2
CO2_mol_per_s <- inj$CO2_mumol_per_s / 10^6
inj$CO2_mol_m2_s <- CO2_mol_per_s/A_inj


save(inj,dates_ls,file = paste(datapfad_PP_Kammer,"injectionrates.RData"))

