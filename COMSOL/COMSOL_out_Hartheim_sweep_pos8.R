rm(list=ls())
#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_tracer<- paste0(metapfad,"Tracereinspeisung/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","ggforce","units","egg","dplyr")
sweep_long
check.packages(packages)

######################################
#data_agg
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(comsolpfad,"sweep_extend.RData"))
load(paste0(datapfad_harth,"PPC_DS.RData"))
load(paste0(comsolpfad,"plotdata_Methodenpaper_roll.RData"))
sweep_wide <- extend_wide
# sweep_long <- sweep_extend
# rm(sweep_extend)
#######################
#einheiten anpassen für COMSOl
#Tracersignal in COMSOL Einheit umrechnen
offset_method <- "drift"
######################

data$CO2_mol_per_m3 <- ppm_to_mol(data[,paste0("CO2_tracer_",offset_method)],"ppm",p_kPa = data$PressureActual_hPa/10,T_C = data$T_soil)
data$CO2_mol_per_m3[data$tiefe == 0]<- 0
#data$CO2_mol_per_m3[data$tiefe == 0]<- NA
data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0]<- 0
#data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0]<- NA

data$date_hour <- round_date(data$date,"hours")
mod_dates <- sort(unique(data$date[data$Position %in% 7:8 & data$Pumpstufe != 0 & data$date %in% data$date_hour]))
#mod_dates <- sort(unique(data$date[data$Position %in% 8 & data$Pumpstufe != 0 & data$date %in% data$date_hour]))
#mod_dates <- sort(unique(data$date[data$Position %in% 7:8 & data$Pumpstufe != 0 ]))


n_DS <- 3
pars <- c("injection_rate",paste0("DS_",1:n_DS))
#Parameter file lesen
pars_fs <- read.table((paste0(metapfad,"COMSOL/parameter_freeSoil.csv")),sep=";",stringsAsFactors = F)
#Modell tiefe als Character
z_soil_ch <- str_split(pars_fs[pars_fs[,1] == "z_soil",2],"\\s",simplify = T)
#Einheit aus File übernehmen
unit <- str_remove_all(z_soil_ch[,2],"\\[|\\]")
z_soil <- set_units(as.numeric(z_soil_ch[,1]),unit,mode="standard")
z_soil_cm <- as.numeric(set_units(z_soil,"cm"))
#z_soil_cm <- 200

#############
#data auf stunden aggregieren
data_sub <- subset(data, date %in% mod_dates)
#data_mod_range <- subset(data,date > min(mod_dates) & date < max(mod_dates))
#data_agg_mod <- data_mod_range %>% group_by(date_hour,tiefe) %>% summarise_all(mean)



#########################################################

#modell mit obs vergleichen

########################################################
offset_methods <- c(#"SWC_T",
                    # "SWC_T_min",
                    # "SWC_T_mingradient",
                    # "SWC_T_maxgradient",
                    # "SWC_T_max",
                    "drift_min",
                    #"drift_mingradient",
                    #"drift_maxgradient",
                    "drift_max",
                    "drift"
                    )
# 
# offset_method <- "SWC_T"
# offset_method <- "drift_min"
# offset_method <- "drift_mingradient"
# offset_method <- "drift_maxgradient"
# offset_method <- "drift_max"
# offset_method <- "drift"
DS_long_list <- vector("list",length(offset_methods))
names(DS_long_list) <- offset_methods
F_df_list <- DS_long_list

######################

minmax_cols <- grep("CO2_tracer_drift_.*(min|max)",colnames(data_uncert),value = T)
data[,minmax_cols] <- NA
data[data$date %in% data_uncert$date,minmax_cols] <- data_uncert[,minmax_cols]

for(offset_method in offset_methods){
  print(offset_method)

data$CO2_mol_per_m3 <- ppm_to_mol(data[,paste0("CO2_tracer_",offset_method)],"ppm",p_kPa = data$PressureActual_hPa/10,T_C = data$T_soil)
data$CO2_mol_per_m3[data$tiefe == 0]<- 0
#data$CO2_mol_per_m3[data$tiefe == 0]<- NA
data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0]<- 0

data <- data %>% 
  group_by(tiefe) %>% 
  mutate(CO2_mol_per_m3 = imputeTS::na_interpolation(CO2_mol_per_m3)) %>% 
  as.data.frame()


n_best <- 1

#Dataframe in den Flux und DS Werte reinkommen
F_df <- data.frame(date=mod_dates,Fz=NA)
F_df[,paste0("DS_",rep(c("","min_","max_"),each=n_DS),1:n_DS)]<- NA

pb <-  txtProgressBar(min = 0, max = length(mod_dates), initial = 0,style=3) 
######################################
#Schleife in der obs mit mod für unterschiedliche Zeiten verglichen werden
for(k in seq_along(mod_dates)){
  #fortschritt angeben
  setTxtProgressBar(pb,k)

  #k-tes datum
  kammer_date <- mod_dates[k]
  
  #######################
  #gemesssenes CO2 profil am k-ten datum
  #CO2_obs <- subset(data_agg_mod,date_hour== kammer_date)
  CO2_obs <- subset(data,date== kammer_date)
  D0_CO2_m2 <- mean(D0_T_p(T_C = CO2_obs$T_soil,p_kPa = CO2_obs$PressureActual_hPa/10,unit="m2/s")) #20°C m2/s
  CO2_obs$z <- z_soil_cm + CO2_obs$tiefe
  #umsortieren
  CO2_obs <- CO2_obs[order(-CO2_obs$tiefe),]
  
  #plot des gemessenen Tiefenprofils 
  #ggplot(CO2_obs)+geom_path(aes(CO2_mol_per_m3,tiefe))
  
  ###########
  #mod und obs vergleichen
  if(any(CO2_obs$CO2_mol_per_m3>0,na.rm = T)){
    #Injecitonsrate bei Kammermessungen
    injection_rate_obs <-signif(unique(CO2_obs$inj_mol_m2_s),2)
    
    # sweep_inj_rates <- as.numeric(unique(sweep_long$injection_rate))
    # #die modellierte injection rate die am nächsten an der k-ten liegt
    # injection_rate_i <- sweep_inj_rates[which.min(abs(injection_rate_obs - sweep_inj_rates))]
    # #subset des Sweeps mit nur der richtigen injektionsrate
     sweep_sub_id <- grep(paste0("injection_rate=",injection_rate_obs),colnames(sweep_wide))
     sweep_sub <- sweep_wide[,sweep_sub_id]
    
    #rmse jedes Sweeps berechnen
    rmse <- apply(sweep_sub,2,RMSE,CO2_obs$CO2_mol_per_m3)
    #rmse <- apply(sweep_sub,2,function(x) RMSE(x[1:4],CO2_obs$CO2_mol_per_m3[1:4]))
    
    #zu den RMSE werten die jeweiligen DS sets
    DS_mat_ch <- str_extract_all(names(rmse),"(?<=DS_\\d=)\\d(\\.\\d+)?(E|e)-\\d+",simplify = T)
    DS_mat <- as.data.frame(apply(DS_mat_ch,2,as.numeric))
    
    colnames(DS_mat) <- str_subset(pars,"DS")
    #dataframe mit RMSE und DS Sets
    DS_wide <- cbind(rmse,DS_mat)
    #nur die Paramtersets mit aufsteigendem DS
    #4 ds
    #DS_sorted <- DS_wide[which(DS_wide[,2] >= DS_wide[,3] & DS_wide[,3] >= DS_wide[,4] &DS_wide[,4] >= DS_wide[,5] ),]
    #3Ds
    
    #DS im long format
    DS_long <- reshape2::melt(DS_wide, id = "rmse",variable="Schicht",value.name="DS")
    
    ##########################################
    #dottyplot
    #ggplot(subset(DS_long,rmse < sort(unique(rmse))[200]))+geom_point(aes(DS,rmse))+facet_wrap(~Schicht,scales="free")
    #ggplot(subset(DS_long))+geom_point(aes(DS,rmse))+facet_wrap(~Schicht,scales="free")
    
    #########################################
    #Bester RMSE
    ########################################
    best.fit.id <- which.min(rmse)
    good.fit.id <- which(rmse <= sort(rmse)[n_best])
    
    #Bester Parametersatz
    best_DS <- as.numeric(DS_mat[best.fit.id,])
    names(best_DS) <- colnames(DS_mat)
    
    ##################
    #range der DS-Werte die fast genauso gut waren
    good_DS_chr <- DS_mat[good.fit.id,]
    #min
    min_DS <- apply(good_DS_chr,2,function(x) min(as.numeric(x)))
    names(min_DS) <- paste0("DS_min_",1:length(min_DS))
    #max
    max_DS <- apply(good_DS_chr,2,function(x) max(as.numeric(x)))
    names(max_DS) <- paste0("DS_max_",1:length(max_DS))
    
    #alle in einen Vector
    DS_vec <- c(best_DS,max_DS,min_DS)
    #und in Data.fram
    F_df[F_df$date == mod_dates[k],names(DS_vec)] <- DS_vec
    
    ################
    #flux
    #Co2-gradient zwischen 0 und 7 cm
    slope_0_7cm <- glm(CO2_ref ~ tiefe, data= subset(CO2_obs,tiefe >= -7))#ppm/cm
    dC_dz <- -slope_0_7cm$coefficients[2] #ppm/cm
    #einheit in mol / m3 /cm
    dC_dz_mol <- ppm_to_mol(dC_dz,"ppm",out_class = "units",T_C = CO2_obs$T_soil[CO2_obs$tiefe == -3.5],p_kPa = unique(CO2_obs$PressureActual_hPa)/10)#mol/m^3/cm
    
    #Ficks Law
    Fz_mumol_per_s_m2 <- best_DS[1]  * dC_dz_mol * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2
    names(Fz_mumol_per_s_m2) <- "Fz"
    #in data frame
    F_df$Fz[F_df$date == mod_dates[k]] <- Fz_mumol_per_s_m2
  }#ende if
}#ende loop
#########################################
#ende for loop
close(pb)
#speichern
#save(F_df,file=paste0(comsolpfad,"F_df_glm_3DS_pos8_ext.RData"))

# load(file=paste0(comsolpfad,"F_df_gam_3DS_pos8.RData"))
# F_df_pos8 <- F_df
# load(file=paste0(comsolpfad,"F_df_gam_3DS_2.RData"))
# F_df<-rbind(F_df,F_df_pos8)
# colnames(F_df)
# colnames(F_df_pos8)
# F_df_pos8#######
#DS_long

#Zeitraum bis steady state abschneiden 
 for(i in 1:nrow(Pumpzeiten)){
  F_df[F_df$date > (round_date(Pumpzeiten$start,"hours")[i]-3600) & F_df$date < (round_date(Pumpzeiten$start,"hours")[i]+12*3600),c(grep("Fz|DS",colnames(F_df)))]<-NA
}
#moving average
#F_df$Fz_roll <- RcppRoll::roll_mean(F_df$Fz,n=120,fill=NA)
F_df$Fz_roll <- RcppRoll::roll_mean(F_df$Fz,n=3,fill=NA)


DS_long <-tidyr::pivot_longer(F_df,starts_with("DS"),names_pattern = "(\\w+)_(\\d)",names_to = c(".value","id"))


DS_long$T_soil <- NA
for(i in 1:3){
  DS_long$T_soil[DS_long$id == i] <- data_sub[data_sub$tiefe == c(-3.5,-14,-24.5)[i],"T_soil"]
}
DS_long$DSD0 <- DS_long$DS/D0_T_p(DS_long$T_soil,unit="m^2/s")
#nrow(DS_long)
#DS_long$T_C <- as.numeric(factor(DS_long$date,levels = data$date, labels = data$T_C)) 
DS_long_list[[offset_method]] <- DS_long 
F_df_list[[offset_method]] <- F_df 
}

#F_df_drift <- F_df
#DS_long_drift <- DS_long
#F_df_drift_min <- F_df
#DS_long_drift_min <- DS_long
#F_df_drift_mingradient <- F_df
#DS_long_drift_mingradient <- DS_long
#F_df_drift_maxgradient <- F_df
#DS_long_drift_maxgradient <- DS_long
#F_df_drift_max <- F_df
#DS_long_drift_max <- DS_long
#F_df_SWC_T <- F_df
#DS_long_SWC_T <- DS_long

minmax_vec <- c("min","max")
for(i in minmax_vec){
DS_long_list[["drift"]][,paste0("DSD0_",i)] <-  DS_long_list[[paste0("drift_",i)]]$DSD0
}
DS_long_list$drift <- DS_long_list$drift %>% group_by(id) %>% mutate(across(paste0("DSD0",c("","_min","_max")),list(roll=~RcppRoll::roll_mean(.,n=5,fill=NA)))) %>% as.data.frame()
DS_long_list$SWC_T <- DS_long_list$SWC_T %>% group_by(id) %>% mutate(across("DSD0",list(roll=~RcppRoll::roll_mean(.,n=5,fill=NA)))) %>% as.data.frame()


#save(DS_long_list,file=paste0(datapfad_harth,"DS_long_list.RData"))
save(DS_long_list,file=paste0(datapfad_harth,"DS_long_list_poly1.RData"))
#DS_long_list$drift <- DS_long_list$drift[,1:9]
# DS_long_drift$DS_min <- DS_long_drift_min$DS
# DS_long_drift$DS_max <- DS_long_drift_max$DS
# DS_long_drift$DS_maxgradient <- DS_long_drift_maxgradient$DS
# DS_long_drift$DS_mingradient <- DS_long_drift_mingradient$DS
##################################
#plot

#F_plot
ggplot()+
  #geom_point(data=subset(F_df),aes(date,Fz))+
  geom_line(data=subset(F_df_drift),aes(date,Fz,col="drift"),alpha=1)+
  geom_line(data=subset(F_df_SWC_T),aes(date,Fz,col="SWC_T"),alpha=1)+
  #geom_line(data=subset(F_df),aes(date,Fz_roll,col="moving avg"))+
  #scale_color_manual("gradient method",values=1:2)+
  #xlim(c(min(F_df$date[-1]),max(F_df$date[])+3600*5))+
  #xlim(ymd_h(c("2020.07.04 8","2020.07.14 19")))+
  #xlim(ymd_h(c("2020.06.07 0","2020.06.09 19")))+
  labs(title=paste0(offset_method," ",n_DS,"DS"),y=expression(CO[2]*"flux ["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))#+
  #ggsave(paste0(plotpfad,"Flux_Kammer_Comsol_gam_3DS_pos8.png"),width=8,height = 4)
#F_df_gam <- F_df

# #DS_plot
# ggplot(DS_long_drift)+
#   geom_ribbon(aes(x=date,ymin=DS_min,ymax=DS_max,fill=id),alpha=0.2)+
# 
# #  geom_ribbon(aes(x=date,ymin=DS_mingradient,ymax=DS_maxgradient,col=id),alpha=0.1)+
#   geom_line(aes(date,DS,col=id))
#   DS_long_list$SWC_T
#   DS_long
  
id <- which(is.na(data_sub$CO2_mol_per_m3))
data_sub$date[id]
ggplot(DS_long_list$drift)+
  geom_ribbon(aes(x=date,ymin=DSD0_min_roll,ymax=DSD0_max_roll,group=id,fill="sweep"),alpha=0.2)+
  geom_line(aes(date, DSD0_roll,group=id,col="sweep"))+
  geom_line(data=DS_long_list$SWC_T,aes(date, DSD0_roll,group=id,col="SWC_T"))+
  geom_line(data=DS_long_roll,aes(date,DSD0_roll,group=id,col="SNOPT"))
  
  #geom_line(aes(date,DS_sorted,col=id))#+
  #ggsave(paste0(plotpfad,"DS_zeit_gam_3DS_pos8.png"),width=8,height = 4)
colnames(PPC_DS)
#mal anschauen
paste(names(best_DS),best_DS)

data_plot <- data %>%
  group_by(tiefe,date_hour=round_date(data$date,"hours")) %>%
  summarise(DSD0_PTF_min = min(DSD0_PTF_min,na.rm=T),DSD0_PTF_max = max(DSD0_PTF_max),DSD0_PTF= mean(DSD0_PTF),date=mean(date))

DS_long$tiefe <- as.numeric(DS_long$id)*-7
ggplot(subset(data_plot))+
  geom_ribbon(aes(x=date,ymin=DSD0_PTF_min,ymax=DSD0_PTF_max,fill=as.factor(tiefe)),alpha=0.2)+
  geom_line(aes(date,DSD0_PTF,col=as.factor(tiefe)))+
  ggnewscale::new_scale_color()+
  geom_line(data=DS_long,aes(date,DS/D0_T_p(unit="m2/s"),col=as.factor(tiefe),linetype="COMSOL"))+
  xlim(range(DS_long$date))

  #geom_line(data=F_Comsol_snopt_long,aes(date,DSD0,col=name,linetype="snopt"))+

######################################################
#1:1 plot kammer und Comsol Flux vorbereiten
Kammer_agg <- Kammer_flux %>% group_by(day) %>% summarise(kammer_min = min(CO2flux_min),kammer_max = max(CO2flux_max), kammerflux = mean(CO2flux),date =mean(date))

Kammer_agg <- subset(Kammer_agg,day%in%ymd(c("2020.07.08","2020.07.10","2020.07.14")))
Kammer_agg$Fz_mod <- NA
Kammer_agg$date_mod <- as_datetime(NA)
F_sub <- subset(F_df, !is.na(Fz_roll))
for(i in 1:nrow(Kammer_agg)){
  Kammer_agg$Fz_mod[i] <- F_sub$Fz_roll[which.min(abs(F_sub$date - Kammer_agg$date[i]))]
  Kammer_agg$date_mod[i] <- F_sub$date[which.min(abs(F_sub$date - Kammer_agg$date[i]))]
}
Kammer_agg$Fz_mod[2] <- F_sub$Fz_roll[which.min(abs(F_sub$date - (Kammer_agg$date[2]+24*3600)))]
Kammer_agg$date_mod[2] <- F_sub$date[which.min(abs(F_sub$date - (Kammer_agg$date[2]+24*3600)))]

ggplot(Kammer_agg)+
  geom_errorbar(aes(x=Fz_mod,ymin=kammer_min,ymax=kammer_max))+
  geom_abline(slope=1,intercept = 0)
ggplot(Kammer_agg)+
  geom_errorbar(aes(x=date,ymin=kammer_min,ymax=kammer_max,col="obs"))+
  geom_point(aes(date_mod,Fz_mod,col="mod"))

Kammer_sub <- subset(CO2_flux,day%in%ymd(c("2020.07.08","2020.07.10","2020.07.14")) & kammer != "D")
Kammer_sub$Fz_mod <- NA
F_sub <- subset(F_df, !is.na(Fz_roll))
for(i in 1:nrow(Kammer_sub)){
  Kammer_sub$Fz_mod[i] <- F_sub$Fz_roll[which.min(abs(F_sub$date - Kammer_sub$date[i]))]
}
f_range <- range(Kammer_sub[,c("Fz_mod","mumol_per_s_m2")])

#######################
#1:1 plot
ggplot(Kammer_sub)+
  #geom_errorbar(aes(x=Fz_mod,ymin=CO2flux_min,ymax=CO2flux_max,col=kammer))+
  geom_point(aes(x=Fz_mod,y=mumol_per_s_m2,col=kammer))+
  geom_abline(slope=1,intercept = 0)+lims(x = f_range, y = f_range)
ggplot(Kammer_sub)+
  geom_point(aes(date,mumol_per_s_m2,col=kammer))+
  geom_point(aes(date,Fz_mod,col="mod"))
ggplot(Kammer_sub)+
  geom_point(aes(date,kammerflux,col="obs"))+
  geom_errorbar(aes(x=date,ymin=kammer_min,ymax=kammer_max,col="obs"))+
  geom_point(aes(date,Fz_mod,col="mod"))

F_Comsol$day <- as_date(F_Comsol$date)
F_vergleich <- merge(F_Comsol,Kammer_agg,by="day")
ggplot(F_vergleich)+geom_point(aes(kammerflux,Fz))+geom_abline(intercept = 0,slope=1)+geom_errorbar(aes(xmin=kammer_min,xmax=kammer_max,y=Fz))+xlim(c(1,5.5))+ylim(c(1,5.5))


#######################################################

#Parameter für sweep in COMSOL exportieren

###########################################################

#list mit subset jedes 
#data_sub <- lapply(mod_dates,function(x) subset(data[,c("tiefe","tiefenstufe","date","CO2_mol_per_m3","inj_mol_m2_s","DSD0_PTF_max","DSD0_PTF_min","T_soil","PressureActual_hPa","CO2_ref","CO2_inj")], date==x))
#injectionrates der Zeiten die odelliert werden sollen
injection_rates_raw <- data$inj_mol_m2_s[data$date %in% mod_dates]
#um nicht zu viele sweeps rechnen zu müssen werden die injectionrates gerundet
injection_rates <- unique(round(injection_rates_raw,3))

#wertebereiche für DS 1 bis 3
min_DS <- c(2e-6,1e-6,5e-7)
max_DS <- c(1e-5,4e-6,1.1e-6)

#step so das für jeden DS 10 gleich große Schritte sind
step <- (max_DS - min_DS) / 10
#anzahl schichten hier 3
schichten <- length(min_DS)

#alles in matrix und exportieren
DS_1bis8 <- matrix(paste0("DS_",1:schichten," range(",min_DS,",",step,",",max_DS,")"),schichten,1)
pars_Hartheim <- rbind(paste0("injection_rate ",paste(injection_rates,collapse = ", ")),DS_1bis8)
write.table(pars_Hartheim,
            file = paste0(metapfad_comsol,"parameter_Hartheim_pos8.txt"),
            row.names = F,col.names = F,quote=F)

