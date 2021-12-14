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
packages<-c("lubridate","stringr","ggplot2","ggforce","units","egg","dplyr","tictoc")

check.packages(packages)



######################################
#data_agg
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(datapfad_harth,"PPC_DS.RData"))
load(paste0(comsolpfad,"plotdata_Methodenpaper_roll.RData"))

load(paste0(comsolpfad,"extend_wide.RData"))
load(paste0(comsolpfad,"extend_long.RData"))
sweep_wide <- extend_wide
sweep_long <- extend_long
rm(extend_wide)
rm(extend_long)

#load(paste0(comsolpfad,"sweep_wide_list.RData"))
#sweep_wide <- sweep_wide_list[[2]]

#######################
#einheiten anpassen für COMSOl
#Tracersignal in COMSOL Einheit umrechnen

#injektion 1 weg
data$Position[data$date > Pumpzeiten$start[10] & data$date < Pumpzeiten$start[12]] <- NA

#data$date_hour <- round_date(data$date,"3 hours")
data$date_hour <- round_date(data$date,"30 mins")
mod_dates <- sort(unique(data$date[data$Position %in% 7:8 & data$Pumpstufe != 0 & data$date %in% data$date_hour]))
#mod_dates <- sort(unique(data$date[data$Position %in% 8 & data$Pumpstufe != 0 & data$date %in% data$date_hour]))
#mod_dates <- sort(unique(data$date[data$Position %in% 7:8 & data$Pumpstufe != 0 ]))

#######################
#Parameter
n_DS <- 3
pars <- c("injection_rate",paste0("DS_",1:n_DS))
z_soil_cm <- 150

#############
#data subset
minmax_cols <- grep("CO2_tracer_(drift|SWC_T)_.*(min|max)",colnames(data_uncert),value = T)
data[,minmax_cols] <- NA
data[data$date %in% data_uncert$date,minmax_cols] <- data_uncert[,minmax_cols]


data_sub <- subset(data, date %in% mod_dates) %>% select(!matches("(Precip|Wind|Ta_|preds|DSD0|PV|VWC|^R_|offset|T_C|Swin|eps|PTF|Fz|Pumpstufe|daymean)")) 

data_sub$z <- z_soil_cm + data_sub$tiefe






#########################################################

#modell mit obs vergleichen

########################################################
offset_methods <- c(#"SWC_T",
                    # "SWC_T_min",
                    # "SWC_T_mingradient",
                    # "SWC_T_maxgradient",
                    #"SWC_T_max",
                    "drift_min",
                    #"drift_mingradient",
                    #"drift_maxgradient",
                    "drift_max",
                    "drift",
                    "SWC_WS",
                    "SWC_WS_min",
                    "SWC_WS_max"
                    )


DS_long_list <- vector("list",length(offset_methods))
names(DS_long_list) <- offset_methods
F_df_list <- DS_long_list

######################



##################################################
#                                                #
#          Schleife                              #
#                                                #
##################################################



sweep_inj_rates <- as.numeric(unique(sweep_long$injection_rate))

#sweep_sub_id <- grep(paste0("injection_rate=",),colnames(sweep_wide))
#sweep_sub <- sweep_wide[,sweep_sub_id]
sweep_sub_list <- lapply(sweep_inj_rates,function(x) 
  sweep_wide[,grep(paste0("injection_rate=",x),colnames(sweep_wide))])

names(sweep_sub_list) <- sweep_inj_rates


# n.cores <- parallel::detectCores() - 1
# my.cluster <- parallel::makeCluster(
#   n.cores, 
#   type = "PSOCK"
# )
# doParallel::registerDoParallel(cl = my.cluster)

for(offset_method in offset_methods){
tictoc::tic()
  print(offset_method)

data_sub$CO2_mol_per_m3 <- ppm_to_mol(data_sub[,paste0("CO2_tracer_",offset_method)],"ppm",p_kPa = data_sub$PressureActual_hPa/10,T_C = data_sub$T_soil)
data_sub$CO2_mol_per_m3[data_sub$tiefe == 0]<- 0
data_sub$CO2_mol_per_m3[(data_sub$CO2_mol_per_m3) < 0]<- 0

data_sub <- data_sub %>%
  group_by(tiefe) %>%
  mutate(CO2_mol_per_m3 = imputeTS::na_interpolation(CO2_mol_per_m3)) %>%
  as.data.frame()

data_list <- lapply(mod_dates,function(x) subset(data_sub, date == x))

#Dataframe in den Flux und DS Werte reinkommen
F_df <- data.frame(date=mod_dates)
F_df[,paste0("DS_",1:n_DS)]<- NA

pb <-  txtProgressBar(min = 0, max = length(mod_dates), initial = 0,style=3) 
######################################
#Schleife in der obs mit mod für unterschiedliche Zeiten verglichen werden
#foreach::foreach(k = seq_along(mod_dates),.combine = cbind,.packages = c("pkg.WWM","stringr")) %dopar% {
for(k in seq_along(mod_dates)){
  #fortschritt angeben
  setTxtProgressBar(pb,k)

  #k-tes datum
  #kammer_date <- mod_dates[k]
  
  #######################
  #gemesssenes CO2 profil am k-ten datum
  #CO2_obs <- subset(data_agg_mod,date_hour== kammer_date)
  CO2_obs <- data_list[[k]]
  #CO2_obs <- subset(data,date== kammer_date)
  #D0_CO2_m2 <- mean(D0_T_p(T_C = CO2_obs$T_soil,p_kPa = CO2_obs$PressureActual_hPa/10,unit="m2/s")) #20°C m2/s
  
  #umsortieren
  CO2_obs <- CO2_obs[order(-CO2_obs$tiefe),]
  
  #plot des gemessenen Tiefenprofils 
  #ggplot(CO2_obs)+geom_path(aes(CO2_mol_per_m3,tiefe))
  
  ###########
  #mod und obs vergleichen
  if(any(CO2_obs$CO2_mol_per_m3>0,na.rm = T)){
    #Injecitonsrate bei Kammermessungen
    injection_rate_obs <-signif(unique(CO2_obs$inj_mol_m2_s),2)
    
    #injection_rate_obs <-ceiling(unique(CO2_obs$inj_mol_m2_s)*10^3)/10^3
    
    
    # #die modellierte injection rate die am nächsten an der k-ten liegt
    injection_rate_i <- sweep_inj_rates[which.min(abs(injection_rate_obs - sweep_inj_rates))]
    # #subset des Sweeps mit nur der richtigen injektionsrate
     # sweep_sub_id <- grep(paste0("injection_rate=",injection_rate_i),colnames(sweep_wide))
     # sweep_sub <- sweep_wide[,sweep_sub_id]
    sweep_sub <- sweep_sub_list[[as.character(injection_rate_i)]]
    
    #rmse jedes Sweeps berechnen
    rmse <- apply(sweep_sub,2,RMSE,CO2_obs$CO2_mol_per_m3)#,normalize="mean_each")
    #rmse <- apply(sweep_sub,2,function(x) RMSE(x[1:4],CO2_obs$CO2_mol_per_m3[1:4]))
    
    #zu den RMSE werten die jeweiligen DS sets
    DS_mat_ch <- str_extract_all(names(rmse),"(?<=DS_\\d=)\\d(\\.\\d+)?(E|e)-\\d+",simplify = T)
    DS_mat <- as.data.frame(apply(DS_mat_ch,2,as.numeric))
    
    colnames(DS_mat) <- str_subset(pars,"DS")

    #########################################
    #Bester RMSE
    ########################################
    best.fit.id <- which.min(rmse)
    #good.fit.id <- which(rmse <= sort(rmse)[n_best])
    
    #Bester Parametersatz
    best_DS <- as.numeric(DS_mat[best.fit.id,])
    names(best_DS) <- colnames(DS_mat)
    
    #und in Data.fram
    F_df[F_df$date == mod_dates[k],names(best_DS)] <- best_DS
    
  }#ende if
}#ende loop
#########################################
#ende for loop
close(pb)

#DS_long

#Zeitraum bis steady state abschneiden 
 for(i in 1:nrow(Pumpzeiten)){
  F_df[F_df$date > (round_date(Pumpzeiten$start,"hours")[i]-3600) & F_df$date < (round_date(Pumpzeiten$start,"hours")[i]+12*3600),c(grep("Fz|DS",colnames(F_df)))]<-NA
}

F_df$method <- offset_method
# DS_long <-tidyr::pivot_longer(F_df,starts_with("DS"),names_pattern = "(\\w+)_(\\d)",names_to = c(".value","id"))
# 
# DS_long$T_soil <- NA
# for(i in 1:3){
#   DS_long$T_soil[DS_long$id == i] <- data_sub[data_sub$tiefe == c(-3.5,-14,-24.5)[i],"T_soil"]
# }
# DS_long$DSD0 <- DS_long$DS/D0_T_p(DS_long$T_soil,unit="m^2/s")
# #nrow(DS_long)
# #DS_long$T_C <- as.numeric(factor(DS_long$date,levels = data$date, labels = data$T_C)) 
# DS_long_list[[offset_method]] <- DS_long 
F_df_list[[offset_method]] <- F_df 
tictoc::toc()
}
#parallel::stopCluster(cl = my.cluster)

##################################################################
####################################################
for(j in c("drift","SWC_WS")){
#for(j in c("drift","SWC_T")){
minmax_vec <- c("min","max")
for(i in minmax_vec){
#DS_long_list[[j]][,paste0("DSD0_",i)] <-  DS_long_list[[paste0(j,"_",i)]]$DSD0
for(k in 1:3){
F_df_list[[j]][,paste0("DS_",i,"_",k)] <-  F_df_list[[paste0(j,"_",i)]][paste0("DS_",k)]
}
}

#DS_long_list[[j]] <- DS_long_list[[j]] %>%
#  group_by(id) %>%
#  mutate(across(paste0("DSD0",c("","_min","_max")),list(roll=~RcppRoll::roll_mean(.,n=5,fill=NA)))) %>%
#  as.data.frame()

}

for(j in names(F_df_list)){
#DS_long_list[[j]]$method <- j
F_df_list[[j]]$method <- j
}

#F_df_list$SWC_WS[paste0("DS_",rep(minmax_vec,each=3),"_",1:3)] <- NA
#names(DS_long_list)
#DS_long <- do.call(rbind,DS_long_list[c("drift","SWC_T")])
F_df <- do.call(rbind,F_df_list[c("drift","SWC_WS")])
#F_df <- do.call(rbind,F_df_list[c("drift","SWC_T","SWC_WS")])

################################################
#F_df
################################################

data$preds_drift[data$tiefe == 0] <- data$CO2_roll_ref[data$tiefe == 0]

data$CO2_ref_mol_m3 <- ppm_to_mol(data$preds_drift,"ppm",out_class = "units",T_C = data$T_soil,p_kPa = data$PressureActual_hPa/10)

data_wide_CO2 <- tidyr::pivot_wider(data[data$date %in% F_df$date,],date,names_from=tiefenstufe,values_from = CO2_ref_mol_m3,names_prefix = "CO2_ref_")

F_df <- merge(F_df,data_wide_CO2)



##################################################################
#anstatt glm geht es viel schneller jeweils den mittelwert von 3 
################
#flux
dC_list <- vector("list",3)

dC_list[[1]] <- rowMeans(cbind(F_df$CO2_ref_0 - F_df$CO2_ref_1,F_df$CO2_ref_1 - F_df$CO2_ref_2,F_df$CO2_ref_2 - F_df$CO2_ref_3)/-3.5)

dC_list[[2]] <- rowMeans(cbind(F_df$CO2_ref_3 - F_df$CO2_ref_4,F_df$CO2_ref_4 - F_df$CO2_ref_5)/-3.5)
dC_list[[3]] <- rowMeans(cbind(F_df$CO2_ref_5 - F_df$CO2_ref_6,F_df$CO2_ref_6 - F_df$CO2_ref_7)/-3.5)
#mol/m^3/cm
#einheit in mol / m3 /cm

#Ficks Law
#Fz_mumol_per_s_m2 <- F_df$DS_1[k]  * dC_dz_mol * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2
for(i in 1:3){
  for(j in c("","min_","max_")){
    F_df[,paste0("Fz_",j,i)] <- F_df[,paste0("DS_",j,i)]   * dC_list[[i]] * 100 * 10^6#m2/s * mol/m3/m = mumol/s/m2
  }
}


sub7u8 <- subset(data, Position %in% 7:8) 
sub7u8$tiefe_pos <- -sub7u8$tiefe 

data_wide <- tidyr::pivot_wider(sub7u8,date,names_from = tiefe_pos,values_from = c(T_soil,PressureActual_hPa))

for(i in c("T_soil_","PressureActual_hPa_")){
  data_wide[,paste0(i,1)] <- rowMeans(data_wide[,paste0(i,0:2*3.5)])
  data_wide[,paste0(i,2)] <- rowMeans(data_wide[,paste0(i,3:5*3.5)])
  data_wide[,paste0(i,3)] <- rowMeans(data_wide[,paste0(i,6:7*3.5)])
}


F_df<- merge(F_df,data_wide[,c("date",paste0("T_soil_",1:3),paste0("PressureActual_hPa_",1:3))])
for(i in 1:3){
  F_df[,paste0("D0_",i)] <- D0_T_p(T_C=F_df[,paste0("T_soil_",i)],p_kPa = F_df[,paste0("PressureActual_hPa_",i)]/10,unit="m^2/s")
  for(j in c("","min_","max_")){
    F_df[,paste0("DSD0_",j,i)] <- F_df[,paste0("DS_",j,i)]/F_df[,paste0("D0_",i)]
  }
}

#F_df <- F_df[,-2]
DS_long <-tidyr::pivot_longer(F_df[,!grepl("CO2_ref",colnames(F_df))],matches("(DS|Fz|D0|T_soil|Press).*_\\d"),names_pattern = "(\\w+)_(\\d)",names_to = c(".value","id"))

DS_long <- DS_long %>%
    group_by(id,method) %>%
    #mutate(across(c("Fz",paste0("DSD0",c("","_min","_max"))),list(roll=~RcppRoll::roll_mean(.,n=5,fill=NA)))) %>%
    mutate(across(matches("(Fz|DSD0)(_min|_max)?$"),list(roll=~RcppRoll::roll_mean(.,n=10,fill=NA)))) %>%
    as.data.frame()
  

names(F_df)
names(DS_long)
ggplot(F_df)+
  #geom_line(aes(date,DSD0_1,col=method))+
  geom_line(data=DS_long_roll,aes(date,DSD0_roll,linetype=id))+
  geom_ribbon(data=DS_long,aes(date,ymin=DSD0_min_roll*1.26,ymax=DSD0_max_roll*1.26,linetype=id,fill=method),alpha=0.2)+
  geom_line(data=DS_long,aes(date,DSD0_roll*1.26,linetype=id,col=method))
  

  
ggplot(DS_long)+
  geom_ribbon(data=DS_long,aes(date,ymin=Fz_min_roll,ymax=Fz_max_roll,linetype=id,fill=method),alpha=0.2)+
  geom_line(aes(date,Fz_roll,col=method,linetype=id))
  

save(F_df,DS_long,file=paste0(datapfad_harth,"DS_long_list_SWC_WS_drift_minmax.RData"))
#save(F_df,DS_long,file=paste0(datapfad_harth,"DS_long_list_SWC_drift_minmax3.RData"))
#save(F_df,DS_long,file=paste0(datapfad_harth,"DS_long_list_SWC_drift_minmax2.RData"))
#save(F_df,DS_long,file=paste0(datapfad_harth,"DS_long_list_SWC_drift_minmax.RData"))
#save(DS_long_list,DS_long_WS,file=paste0(datapfad_harth,"DS_long_list_SWC_WS.RData"))
#save(DS_long_list,DS_long_mean_each,file=paste0(datapfad_harth,"DS_long_list_drift_mean_each.RData"))
#save(DS_long_list,file=paste0(datapfad_harth,"DS_long_list_ceil.RData"))
#save(DS_long_list,file=paste0(datapfad_harth,"DS_long_list_withPos1.RData"))
#save(DS_long_list,file=paste0(datapfad_harth,"DS_long_list.RData"))
#save(DS_long_list,file=paste0(datapfad_harth,"DS_long_list_poly1.RData"))
#DS_long_list$drift <- DS_long_list$drift[,1:9]
# DS_long_drift$DS_min <- DS_long_drift_min$DS
# DS_long_drift$DS_max <- DS_long_drift_max$DS
# DS_long_drift$DS_maxgradient <- DS_long_drift_maxgradient$DS
# DS_long_drift$DS_mingradient <- DS_long_drift_mingradient$DS


###############################################################################
#plot
##########################################################################


#F_plot
ggplot()+
  #geom_point(data=subset(F_df),aes(date,Fz))+
  #geom_line(data=subset(F_df_drift),aes(date,Fz,col="drift"),alpha=1)+
  geom_line(data=subset(F_df_list$SWC_T),aes(date,Fz,col="SWC_T"),alpha=1)+
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
  

ggplot()+
  #geom_ribbon(aes(x=date,ymin=DSD0_min_roll,ymax=DSD0_max_roll,group=id,fill="sweep"),alpha=0.2)+
  #geom_line(aes(date, DSD0_roll,group=id,col="sweep"))+
  geom_line(data=DS_long_list$SWC_WS,aes(date, DS,group=id,col="SWC_WS"))+
  geom_line(data=DS_long_list$SWC_T,aes(date, DS,group=id,col="SWC_T"),linetype=2)#+
  #geom_line(data=DS_long_roll,aes(date,DSD0_roll,group=id,col="SNOPT"))
  
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

