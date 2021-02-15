#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 



#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","dplyr")
check.packages(packages)

datelim <- list()
#datelim[[1]] <- ymd_h(c("2020.12.22 00","2020.12.22 04"))
#datelim[[1]] <- ymd_h(c("2021.02.09 10","2021.02.09 17"))
datelim[[1]] <- ymd_h(c("2021.02.11 10","2021.02.11 13"))
datelim[[2]] <- ymd_h(c("2021.02.12 09","2021.02.12 13"))
datelim[[3]] <- ymd_h(c("2021.02.13 08","2021.02.13 10"))
datelim[[4]] <- ymd_h(c("2021.02.14 08","2021.02.14 10"))
datelim[[5]] <- ymd_h(c("2021.02.15 08","2021.02.15 10"))
datelim[[6]] <- ymd_h(c("2021.02.15 17","2021.02.15 19"))

Pumpstufen_chr <- list()
Pumpstufen_chr[[1]] <- c(paste0("Tiefe=",c(3,3,3,2,2,2,1,1),"PSt=1.5"),"tracer")
Pumpstufen_chr[[2]] <- c("tracer1","tracer1",paste0("Tiefe=",1:3,"PSt=1.5"),"tracer2","tracer2")
Pumpstufen_chr[[3]] <- c("tracer","tracer",paste0("Tiefe=",1:3,"PSt=1.5"))
Pumpstufen_chr[[4]] <- c(paste0("Tiefe=",1:3,"PSt=1.5"))
Pumpstufen_chr[[5]] <- c(paste0("Tiefe=",1:2,"PSt=1.5"))
Pumpstufen_chr[[6]] <- c(paste0("Tiefe=",1,"PSt=1.5"))


#injectionrate should be > 0.26 ml/min

injections_list <- mapply(injectionrate,datelim = datelim,Pumpstufen = Pumpstufen_chr,group="Pumpstufe",t_init = 1,aggregate = T,closing_before = 100,closing_after = 100,t_min=2,SIMPLIFY = F)

injections <- do.call(rbind,injections_list)
injections

ggplot(injections)+geom_line(aes(date,ml_per_min,col=Pumpstufe))

