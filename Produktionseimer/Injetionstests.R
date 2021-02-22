#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad_prod<- paste0(hauptpfad,"Daten/Metadaten/Produktionseimer/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 



#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","dplyr")
check.packages(packages)

datelim_ls <- list()
#datelim_ls[[1]] <- ymd_h(c("2020.12.22 00","2020.12.22 04"))
#datelim_ls[[1]] <- ymd_h(c("2021.02.09 10","2021.02.09 17"))
datelim_ls[[1]] <- ymd_h(c("2021.02.11 10","2021.02.11 13"))
datelim_ls[[2]] <- ymd_h(c("2021.02.12 09","2021.02.12 13"))
datelim_ls[[3]] <- ymd_h(c("2021.02.13 08","2021.02.13 10"))
datelim_ls[[4]] <- ymd_h(c("2021.02.14 08","2021.02.14 10"))
datelim_ls[[5]] <- ymd_h(c("2021.02.15 08","2021.02.15 10"))
datelim_ls[[6]] <- ymd_h(c("2021.02.15 17","2021.02.15 19"))
datelim_ls[[7]] <- ymd_h(c("2021.02.16 15","2021.02.16 17"))
datelim_ls[[8]] <- ymd_h(c("2021.02.17 10","2021.02.17 11"))
datelim_ls[[9]] <- ymd_h(c("2021.02.18 16","2021.02.18 17"))
datelim_ls[[10]] <- ymd_hm(c("2021.02.19 0830","2021.02.19 0930"))
datelim_ls[[11]] <- ymd_h(c("2021.02.19 23","2021.02.20 01"))
datelim_ls[[12]] <- ymd_h(c("2021.02.21 08","2021.02.21 09"))
datelim_ls[[13]] <- ymd_h(c("2021.02.22 07","2021.02.22 09"))

Pumpstufen_ls <- list()
Pumpstufen_ls[[1]] <- c(paste0("Tiefe=",c(1,1,1,2,2,2,3,3),"PSt=1.5"),"tracer")
Pumpstufen_ls[[2]] <- c("tracer1","tracer1",paste0("Tiefe=",3:1,"PSt=1.5"),"tracer","tracer")
Pumpstufen_ls[[3]] <- c("tracer","tracer",paste0("Tiefe=",3:1,"PSt=1.5"))
Pumpstufen_ls[[4]] <- c(paste0("Tiefe=",3:1,"PSt=1.5"))
Pumpstufen_ls[[5]] <- c(paste0("Tiefe=",3:2,"PSt=1.5"))
Pumpstufen_ls[[6]] <- c(paste0("Tiefe=",3,"PSt=1.5"))
Pumpstufen_ls[[7]] <- c(paste0("Tiefe3undTiefe2PSt=1.5"))
Pumpstufen_ls[[8]] <- c(paste0("Tiefe2undTiefe1PSt=1.5"))
Pumpstufen_ls[[9]] <- c(paste0("Tiefe1PSt=1.5"),paste0("Tiefe",1:2,"PSt=3"))
Pumpstufen_ls[[10]] <- c(paste0("Tiefe",c(1,"2u3"),"PSt=3"))
Pumpstufen_ls[[11]] <- c(paste0("Tiefe",c("1u2",3),"PSt=3"))
Pumpstufen_ls[[12]] <- c(paste0("Tiefe",c(2,3,1),"PSt=3"))
Pumpstufen_ls[[13]] <- c(paste0("Tiefe",c(1,2,3),"PSt=3"))

injection_ls_file <- paste0(metapfad_prod,"injection_list.RData")
injection_file <- paste0(metapfad_prod,"injection_rates.txt")

if(file.exists(injection_ls_file)){
  load(injection_ls_file)
  list_old <- injections_list
}else{
  list_old <- NULL
}
#injectionrate should be > 0.26 ml/min
grundflaeche <- 15^2*pi

new_meas <- which(!seq_along(datelim_ls) %in% seq_along(list_old))
if(length(new_meas)){
  
list_new <- mapply(injectionrate,datelim = datelim_ls[new_meas],Pumpstufen = Pumpstufen_ls[new_meas],group="Pumpstufe",t_init = 1,aggregate = T,closing_before = 100,closing_after = 100,t_min=2,Grundfl = grundflaeche,SIMPLIFY = F)

injections_list <- c(list_old,list_new)
}
save(injections_list,file = injection_ls_file)

injections <- do.call(rbind,injections_list)
list_new

##########################
write.csv(injections,file=injection_file,row.names = F)

ggplot(injections)+
  geom_line(aes(date,ml_per_min,col=Pumpstufe))+
  geom_point(aes(date,ml_per_min,col=Pumpstufe))

