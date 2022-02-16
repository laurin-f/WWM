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

#data$CO2_tracer_drift2 <- data$CO2_tracer_drift

data$date_hour <- round_date(data$date,"60 mins")
data$date_3_hours <- round_date(data$date,"3 hours")


mod_dates <- sort(unique(data$date[data$Position %in% 7:8 & data$Pumpstufe != 0 & data$date %in% data$date_hour& data$date > ymd_h("2020.07.10 00")]))

mod_dates_short <- sort(unique(data$date[data$Position %in% 7:8 & data$Pumpstufe != 0 & data$date %in% data$date_3_hours & data$date > ymd_h("2020.07.10 00")]))
mod_dates_inj1 <- sort(unique(data$date[data$Position %in% 7 & data$Pumpstufe == 1.5 & data$date %in% data$date_3_hours]))



ggplot(subset(data,Position==8&Pumpstufe==0))+
  geom_line(aes(date,CO2_tracer_drift,col=as.factor(tiefe)))+
  geom_hline(yintercept = 0)+
  scale_x_datetime(limits=ymd_h("2020-07-18 00", "2020-07-21 12"))
range(subset(data,Position==8&Pumpstufe==0)$date)
  #geom_line(aes(date,preds_drift,group=as.factor(tiefe)))
test <- data %>%
  filter(Position == 8&Pumpstufe==0&tiefe!=0) %>% 
  select(date,tiefe,CO2_tracer_drift) %>% 
  mutate(tiefe_pos=-tiefe) %>%
  pivot_wider(date,values_from=CO2_tracer_drift,names_from=tiefe_pos,names_prefix = "tracer_")

cor_mat <- cor(test[,paste0("tracer_",1:7*3.5)],use="complete")
corrplot::corrplot(cor_mat,method="number")




ggplot(subset(data_uncert,Pumpstufe!=0 & tiefe %in% (1:7*-3.5)))+
  geom_line(aes(date,CO2_tracer_drift,group=tiefe,col="tracer"))+
  geom_line(aes(date,CO2_tracer_drift_mingradient,group=tiefe,col="mingradient"))+
  geom_line(aes(date,CO2_tracer_drift_maxgradient,group=tiefe,col="maxgradient"))#+
  facet_wrap(~tiefe,scales="free")
# data_min_inj <- data %>% 
#   filter(Position == 8 & as.numeric(inj_mol_m2_s) > 0) %>% 
#   mutate(inj_mol_m2_s = min(inj_mol_m2_s, na.rm=T), CO2_tracer_min_inj = CO2_tracer_drift)
# data_max_inj <- data %>% 
#   filter(Position == 8 & as.numeric(inj_mol_m2_s) > 0) %>% 
#   mutate(inj_mol_m2_s = max(inj_mol_m2_s, na.rm=T), CO2_tracer_max_inj = CO2_tracer_drift)
# 
# DS_anisotrop_min_inj <- run_comsol(data=data_min_inj,mod_dates = mod_dates_short,offset_method = "min_inj",overwrite = F,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
# DS_anisotrop_max_inj <- run_comsol(data=data_max_inj,mod_dates = mod_dates_short,offset_method = "min_inj",overwrite = F,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
date_pattern <- "\\d{2}(_\\d{2}){2,3}"

data_sub <- subset(data, date %in% mod_dates)
inj_range <- range(data_sub$inj_mol_m2_s)
inj_seq <- seq(inj_range[1],inj_range[2],len=10)
 data_i <- data_sub
 DS_list <- vector("list",length(inj_seq))
for(i in seq_along(inj_seq)){
  data_i$inj_mol_m2_s <- inj_seq[i]
DS_list[[i]] <- run_comsol(data=data_i,mod_dates = mod_dates[100],offset_method = "SWC_T",overwrite = T,read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
}

 
 glm(DS3~inj,data=DS_df)
 glm(DS1~inj,data=DS_df)
DS_df <- do.call(rbind,DS_list)
DS_df$inj <- inj_seq
ggplot(DS_df)+geom_point(aes(inj,DS3))

tic()
test <- run_comsol(data=data,mod_dates = mod_dates,offset_method = "drift",overwrite = F,read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
toc()
list.files(comsolpfad,pattern = "Diffusion_freeSoil_anisotropy_optim_3DS_drift(_\\d{2}){4}.txt")
tic()
loop <- run_comsol_nruns(data=data,mod_dates = mod_dates,offset_method = "drift",overwrite = F,read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS_50runs",nruns=50)
toc()
test <- interp_comsol_inj(data=data,mod_dates = mod_dates[1:10],offset_method = "drift",overwrite = F,read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS_50runs",nruns=50)

test

approx(1:2,xout=1.5)

ggplot()+
  geom_line(data=loop,aes(date,DSD0,col="loop",group=tiefe))+
  geom_line(data=test,aes(date,DSD0,col="old",group=tiefe),linetype=2)

DS_anisotrop_SWC_T <- run_comsol(data=data,mod_dates = (mod_dates),offset_method = "SWC_T",overwrite = F,read_all = T,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")

DS_anisotrop_drift <- run_comsol(data=data,mod_dates = (mod_dates),offset_method = "drift",overwrite = F,read_all = T,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
DS_anisotrop_drift_max <- run_comsol(data=data_uncert,mod_dates = (mod_dates),offset_method = "drift_max",overwrite = F,read_all = T,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
DS_anisotrop_drift_min <- run_comsol(data=data_uncert,mod_dates = (mod_dates),offset_method = "drift_min",overwrite = F,read_all = T,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")


DS_anisotrop_roll <- run_comsol(data=data,mod_dates = (mod_dates_inj1),offset_method = "roll4",overwrite = F,read_all = T,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")

# 
# DS_anisotrop_drift_test <- run_comsol(data=data,mod_dates = (mod_dates_short[10]),offset_method = "drift2",overwrite = T,read_all = T,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS",file_suffix = "test")

# DS_anisotrop_no_ref <- run_comsol(data=data,mod_dates = rev(mod_dates),offset_method = "no_ref",overwrite = F,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
# DS_anisotrop <- run_comsol(data=data,mod_dates = rev(mod_dates),offset_method = "gam",overwrite = F,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_anisotropy_optim_3DS")
# 
# 
# DS_df <- run_comsol(data=data,mod_dates = mod_dates,offset_method = "gam",overwrite = F,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_optim_3DS")
# 
#bei disturbance ist eine 20cm Schicht unter der Sonde als disturbed im Modell (beim einbau) und hat einen höheren DS3 (DS3*30)
# DS_dist <- run_comsol(data=data,mod_dates = mod_dates,offset_method = "gam",overwrite = F,plot=F,optim_method = "snopt",read_all = F,modelname = "Diffusion_freeSoil_disturbance_optim_3DS")
# 

######################
#long-format
#######################


# DS_long <- tidyr::pivot_longer(DS_df,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# DS_dist_long <- tidyr::pivot_longer(DS_dist,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# DS_anisotrop_long <- tidyr::pivot_longer(DS_anisotrop,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# DS_anisotrop_long_min_inj <- tidyr::pivot_longer(DS_anisotrop_min_inj,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# DS_anisotrop_long_no_ref <- tidyr::pivot_longer(DS_anisotrop_no_ref,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# colnames(DS_anisotrop_drift)

# DS_anisotrop_drift[,paste0("DS_roll",1:3)] <- zoo::rollapply(DS_anisotrop_drift[,paste0("DS",1:3)],width=10,mean,fill=NA)
 DS_anisotrop_drift[,paste0("DSD0_roll",1:3)] <- zoo::rollapply(DS_anisotrop_drift[,paste0("DSD0",1:3)],width=10,mean,fill=NA)

DS_anisotrop_long_drift <- tidyr::pivot_longer(DS_anisotrop_drift,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))


DS_anisotrop_roll <- DS_anisotrop_roll %>% 
  subset(date %in% data$date[!is.na(data$CO2_tracer_roll4)] )
DS_anisotrop_long_roll <- tidyr::pivot_longer(DS_anisotrop_roll,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe")) 

# DS_anisotrop_long_drift_test <- tidyr::pivot_longer(DS_anisotrop_drift_test,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# DS_anisotrop_long_drift_amp <- tidyr::pivot_longer(DS_anisotrop_drift_amp,matches("DS"),names_pattern = "(.+)(\\d)",names_to = c(".value","tiefe"))
# 
# save(DS_anisotrop_long,DS_anisotrop,file=paste0(comsolpfad,"DS_anisotrop_gam.RData"))
# save(DS_anisotrop_long_no_ref,DS_anisotrop_no_ref,file=paste0(comsolpfad,"DS_anisotrop_no_ref.RData"))

save(DS_anisotrop_long_drift,DS_anisotrop_drift,file=paste0(comsolpfad,"DS_anisotrop_drift.RData"))
save(DS_anisotrop_long_roll,DS_anisotrop_roll,file=paste0(comsolpfad,"DS_anisotrop_roll.RData"))

#save(DS_anisotrop_long_drift_amp,DS_anisotrop_drift,file=paste0(comsolpfad,"DS_anisotrop_drift_amp.RData"))





#############################################
#           PLOTS                           #
#############################################

ggplot()+
  #  geom_line(data=DS_anisotrop_long,aes(date,DSD0,col=tiefe,linetype="gam"))+
  geom_line(data=DS_anisotrop_long_roll,aes(date,DSD0,col=tiefe,linetype="roll"))+
  geom_line(data=DS_anisotrop_long_drift,aes(date,DSD0,col=tiefe,linetype="drift"))+
#  geom_vline(xintercept=ymd_h(c("2020.07.23 05","2020.07.23 09")))+
  scale_x_datetime(date_label="%b %d",breaks="1 days")+theme_bw()
ggplot()+
  #  geom_line(data=DS_anisotrop_long,aes(date,DSD0,col=tiefe,linetype="gam"))+
  geom_line(data=DS_anisotrop_long_drift,aes(date,DSD0,col=tiefe,linetype="drift"))#+
  geom_point(data=DS_anisotrop_long_drift_test,aes(date,DSD0,col=tiefe,shape="drifttest"))
  
  
#  geom_line(data=DS_anisotrop_long_min_inj,aes(date,DSD0,col=tiefe,linetype="min_inj"))#+
ggplot(DS_long)+
  geom_line(aes(date,DSD0,col=tiefe,linetype="no disturbance"))+
  geom_line(data=DS_dist_long,aes(date,DSD0,col=tiefe,linetype="with disturbance"))
ggplot(DS_long)+
  geom_line(aes(date,DSD0,col=tiefe,linetype="isotrop"))+
  geom_line(data=DS_anisotrop_long,aes(date,DSD0,col=tiefe,linetype="anisotrop"))+ggsave(paste0(plotpfad_comsol,"DS_anisotrop.png"),width=5,height=3)
ggplot()+
  geom_line(data=DS_anisotrop_long,aes(date,DSD0,col=tiefe,linetype="gam"))+
  geom_line(data=DS_anisotrop_long_no_ref,aes(date,DSD0,col=tiefe,linetype="no ref"))#+
  xlim(range(DS_anisotrop_long_no_ref$date))
  #ggsave(paste0(plotpfad_comsol,"DS_anisotrop.png"),width=5,height=3)



ggplot()+
  geom_line(data=DS_anisotrop_long,aes(date,Fz,col=tiefe,linetype="gam"))+
  geom_line(data=DS_anisotrop_long_drift,aes(date,Fz,col=tiefe,linetype="drift"))+
  geom_line(data=DS_anisotrop_long_no_ref,aes(date,Fz,col=tiefe,linetype="no_ref"))#+

range(DS_anisotrop$DSD03)
range(DS_df$DSD03)
ggplot(DS_dist)+geom_line(aes(date,DS3))
run_comsol(data,mod_dates,)
