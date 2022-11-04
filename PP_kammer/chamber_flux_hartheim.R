hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 


klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

chamber_arduino_pfad <- paste0(hauptpfad,"/Daten/Urdaten/Kammermessungen_Arduino/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)
theme_set(theme_classic())


flux_start <- chamber_arduino(datelim=ymd_h("22.09.27 10","22.09.27 12"),
                              gga_data = T,
                              t_init=0,
                              plot="timeline",
                              t_offset = -80,
                              t_min = 2,
                              t_max=10)
flux_2 <- chamber_arduino(datelim=ymd_h("22.10.18 10","22.10.18 12"),
                          gga_data = T,
                          t_init=0,
                          plot="timeline",
                          t_offset = -300,
                          t_min = 2,
                          t_max=10)
flux_3 <- chamber_arduino(datelim=ymd_hm("22.10.18 12:30","22.10.18 15:00"),
                          gga_data = T,
                          t_init=0,
                          plot="timeline",
                          t_offset = 120,
                          t_min = 2,
                          t_max=10)
flux_4 <- chamber_arduino(datelim=ymd_hm("22.10.24 08:00","22.10.24 10:00"),
                          gga_data = T,
                          t_init=0,
                          plot="timeline",
                          t_offset = 60,
                          t_min = 2,
                          t_max=10)
flux_5 <- chamber_arduino(datelim=ymd_hm("22.11.03 10:30","22.11.03 11:30"),
                          gga_data = T,
                          t_init=0,
                          plot="timeline",
                          t_offset = -50,
                          t_min = 2,
                          t_max=10)

t_offset_df <- data.frame(date = 
                            ymd_hm("2022-09-27 10:00","22.10.18 12:00","22.10.18 12:30","22.10.24 10:00","22.11.03 11:30"),
                          offset = c(-80,-300,120,60,-50))
save(t_offset_df,file = paste0(datapfad_PP_Kammer,"t_offset_df.RData"))
