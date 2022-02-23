###################################
#dateien für COMSOL exportieren####

#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
plotpfad_comsol <- paste0(hauptpfad,"Dokumentation/Berichte/plots/COMSOL/")
COMSOL_exepath <- "C:/Program Files/COMSOL/COMSOL52a/Multiphysics/bin/win64/"
COMSOL_progammpath <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Programme/Fremdprogramme/COMSOL/"
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","ggforce","dplyr","tidyr","tictoc")

check.packages(packages)

load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))


data$date_hour <- round_date(data$date,"60 mins")



mod_dates <- sort(unique(data$date[data$Position %in% 7:8 & data$Pumpstufe != 0 & data$date %in% data$date_hour& data$date > ymd_h("2020.07.10 00")]))




# data_sub <- subset(data, date %in% mod_dates)
# inj_range <- range(data_sub$inj_mol_m2_s)
# inj_seq <- seq(inj_range[1],inj_range[2],len=10)
# data_i <- data_sub
# DS_list <- vector("list",length(inj_seq))
# for(i in seq_along(inj_seq)){
#   data_i$inj_mol_m2_s <- inj_seq[i]
#   DS_list[[i]] <- run_comsol(data=data_i,mod_dates = mod_dates[100],offset_method = "SWC_T",overwrite = T,read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
# }
# 
# 
# glm(DS3~inj,data=DS_df)
# glm(DS1~inj,data=DS_df)
# DS_df <- do.call(rbind,DS_list)
# DS_df$inj <- inj_seq
# ggplot(DS_df)+geom_point(aes(inj,DS3))

tic()
old <- run_comsol(data=data,mod_dates = mod_dates,offset_method = "drift",overwrite = F,read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
toc()
#list.files(comsolpfad,pattern = "Diffusion_freeSoil_anisotropy_optim_3DS_drift(_\\d{2}){4}.txt")
tic()
loop <- run_comsol_nruns(data=data,mod_dates = mod_dates,offset_method = "drift",overwrite = T,read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS_50runs",nruns=50)
toc()
interp <- interp_comsol_inj(data=data,mod_dates = mod_dates,offset_method = "drift",overwrite = F,read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS_50runs",nruns=50)

# approx_df <- test[[1]][,c("date","tiefe")]
# 
# 
# for(i in 1:nrow(approx_df)){
# approx_xy <- approx(x=c(test[["min"]][i,"mod_inj_rate"],test[["max"]][i,"mod_inj_rate"]),
#        y=c(test[["min"]][i,"DSD0"],test[["max"]][i,"DSD0"]),
#        xout = as.numeric(data$inj_mol_m2_s[data$date == test[["min"]][i,"date"]][1])
#        )
# approx_df$DSD0[i] <- approx_xy$y
# approx_df$inj_mol_m2_s[i] <- approx_xy$x
# }

ggplot()+
  geom_line(data = test[[1]],aes(date,DSD0,col=tiefe,linetype="min"))+
  geom_line(data = test[[2]],aes(date,DSD0,col=tiefe,linetype="max"))

approx(1:2,xout=1.5)
names(loop)
ggplot()+
  geom_line(data=loop,aes(date,mod_inj_rate,col="loop",group=tiefe))
ggplot()+
  geom_line(data=interp,aes(date,DSD0,col="interp",group=tiefe))+
  geom_line(data=loop,aes(date,DSD0,col="loop",group=tiefe))+
  geom_line(data=old,aes(date,DSD0,col="old",group=tiefe),linetype=2)

plot(interp$DSD0-old$DSD0,type="l")
plot(loop$DSD0-old$DSD0,type="l",col=2)
