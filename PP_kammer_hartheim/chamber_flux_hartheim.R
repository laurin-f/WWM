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
flux_6 <- chamber_arduino(datelim=ymd_hm("22.11.03 13:30","22.11.03 14:30"),
                          gga_data = T,
                          t_init=0,
                          plot="timeline",
                          t_offset = -20,
                          t_min = 2,
                          t_max=10)
flux_7 <- chamber_arduino(datelim=ymd_hm("22.11.06 00:30","22.11.06 02:00"),
                          gga_data = T,
                          t_init=0,
                          plot="timeline",
                          t_offset = -50,
                          gas = "CO2_GGA",
                          t_min = 2,
                          t_max=10)
flux_8 <- chamber_arduino(datelim=ymd_hm("22.11.09 07:00","22.11.09 08:30"),
                          gga_data = T,
                          t_init=0,
                          plot="timeline",
                          t_offset = -90,
                          t_min = 2,
                          t_max=10)
flux_8 <- chamber_arduino(datelim=ymd_hm("22.11.21 00:20","22.11.22 10:10"),
                          gga_data = T,
                          t_init=0,
                          plot="timeline",
                          t_offset = -200,
                          t_min = 2,
                          t_max=10)

flux_9 <- chamber_arduino(datelim=ymd_hm("22.12.09 09:20","22.12.09 10:00"),
                          gga_data = T,
                          t_init=0,
                          plot="timeline",
                          t_offset = -390,
                          t_min = 2,
                          t_max=10)
flux_10 <- chamber_arduino(datelim=ymd_hm("23.02.16 14:00","23.02.16 17:00"),
                          gga_data = T,
                          t_init=0,
                          plot="timeline",
                          t_offset = -1250,
                          t_min = 2,
                          t_max=10)
flux_11 <- chamber_arduino(datelim=ymd_hm("23.02.22 06:00","23.02.22 09:00"),
                          gga_data = T,
                          t_init=0,
                          plot="timeline",
                          t_offset = -1300,
                          t_min = 2,
                          t_max=10)

t_offset_df <- data.frame(date = 
                            ymd_hm("2022-09-27 10:00","22.10.18 12:00","22.10.18 12:30","22.10.24 10:00","22.11.03 11:30","22.11.03 13:30","22.11.09 08:30","22.11.22 10:10","22.12.09 10:00","23.02.16 14:00","23.02.22 09:00"),
                          offset = c(-80,-300,120,60,-50,-20,-90,-200,-390,-1250,-1300))
save(t_offset_df,file = paste0(datapfad_PP_Kammer,"t_offset_df.RData"))
