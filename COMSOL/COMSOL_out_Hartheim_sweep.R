#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_tracer<- paste0(metapfad,"Tracereinspeisung/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","ggforce","units","egg")

check.packages(packages)

######################################
#data_agg
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))

#######################
#einheiten anpassen für COMSOl
#Tracersignal in COMSOL Einheit umrechnen
offset_method <- "gam"
######################

data$CO2_mol_per_m3 <- ppm_to_mol(data[,paste0("CO2_tracer_",offset_method)],"ppm",p_kPa = data$PressureActual_hPa/10,T_C = data$T_soil)
data$CO2_mol_per_m3[data$tiefe == 0]<- 0
#data$CO2_mol_per_m3[data$tiefe == 0]<- NA
data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0]<- 0
#data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0]<- NA

data$date_hour <- round_date(data$date,"hours")
mod_dates <- sort(unique(data$date[day(data$date) %in% 7:14 & month(data$date) == 7 & data$Pumpstufe != 0 & data$date %in% data$date_hour]))
mod_dates <- sort(unique(data$date[day(data$date) %in% 7:14 & month(data$date) == 7 & data$Pumpstufe != 0 ]))
#mod_dates <- sort(unique(data$date[data$Pumpstufe != 0 & data$date %in% data$date_hour]))
#mod_dates <- ymd_hm(c("2020-07-08 11:00","2020.07.08 12:00","2020.07.14 15:00"))
#mod_dates <- ymd_hm(c("2020-06-09 11:00","2020.07.08 11:00","2020.07.08 12:00","2020.07.14 15:00"))
data_sub <- lapply(mod_dates,function(x) subset(data[,c("tiefe","tiefenstufe","date","CO2_mol_per_m3","inj_mol_m2_s","DSD0_PTF_max","DSD0_PTF_min","T_soil","PressureActual_hPa","CO2_ref","CO2_inj")], date==x))
injection_rates_raw <- sapply(data_sub,'[[',"inj_mol_m2_s")[1,]
injection_rates <- unique(round(injection_rates_raw,3))
min_DS <- c(2e-6,5e-7,1e-7)
max_DS <- c(1.5e-5,5e-6,3e-6)
#step <- c(1e-7,1e-7,1e-7)
step <- (max_DS - min_DS) / 10
schichten <- length(min_DS)
DS_1bis8 <- matrix(paste0("DS_",1:schichten," range(",min_DS,",",step,",",max_DS,")"),schichten,1)

pars_Hartheim <- rbind(paste0("injection_rate ",paste(injection_rates,collapse = ", ")),DS_1bis8)

write.table(pars_Hartheim,
            file = paste0(metapfad_comsol,"parameter_Hartheim.txt"),
            row.names = F,col.names = F,quote=F)


# inj_plot <- ggplot(data_agg)+
#   geom_line(aes(date,inj_mol_m2_s))
# T_plot <- ggplot(data_agg)+geom_line(aes(date,Ta_2m))
# p_plot <- ggplot(data_agg)+geom_line(aes(date,PressureActual_hPa))
# egg::ggarrange(inj_plot,T_plot,p_plot,heights = c(3,1,1))

#data_agg$inj_mol_m2_s
#Parameter file lesen
pars_fs <- read.table((paste0(metapfad,"COMSOL/parameter_freeSoil.csv")),sep=";",stringsAsFactors = F)
#Modell tiefe als Character
z_soil_ch <- str_split(pars_fs[pars_fs[,1] == "z_soil",2],"\\s",simplify = T)
#Einheit aus File übernehmen
unit <- str_remove_all(z_soil_ch[,2],"\\[|\\]")
z_soil <- set_units(as.numeric(z_soil_ch[,1]),unit,mode="standard")
z_soil_cm <- as.numeric(set_units(z_soil,"cm"))
#z_soil_cm <- 200

data_mod_range <- subset(data,date > min(mod_dates) & date < max(mod_dates))
data_agg_mod <- data_mod_range %>% group_by(date_hour,tiefe) %>% summarise_all(mean)

#####################################
#Datei mit Parameter sweep
#CO2_mod_sweep <- readLines(paste0(comsolpfad,"CO2_mod_Hartheim.txt"))
n_DS <- 3
CO2_mod_sweep <- readLines(paste0(comsolpfad,"sweep_Hartheim_",n_DS,"DS_mod_dates2.txt"))
#Anzahl von DS schichten im Modell 
schichten <- unique(as.numeric(str_extract_all(CO2_mod_sweep[9],"(?<=DS_)\\d",simplify = T)))
#Parameter die in der Datei gesweept wurden
pars <- c("injection_rate",paste0("DS_",schichten))
#Regular Expression für die unterschiedlichen Werte die die Parameter annehmen
value_regexp <- "\\d+(\\.\\d+)?(E-\\d)?"
#Spaltennahmen der sweep datei ausschneiden
colnames_sweep <- str_extract_all(CO2_mod_sweep[9],paste0("(?<=% )r|z|",paste0(pars,"=",value_regexp,collapse=", ")),simplify = T)
#ab Spalte 10 stehen die Werte in der Datei diese werden bei leerzeichen getrennt 
CO2_sweep_mat <- str_split(CO2_mod_sweep[10:length(CO2_mod_sweep)],"\\s+",simplify = T)
#die matrix als data.frame mit numerischen werden 
CO2_sweep <- as.data.frame(apply(CO2_sweep_mat,2,as.numeric))
#Spaltennamen
colnames(CO2_sweep) <- colnames_sweep
#ins long format

#sweep_long <- tidyr::pivot_longer(CO2_sweep,cols=-(1:2),names_patter= paste0(paste(pars,collapse = "=(.*), "),"=(.*)"),values_to="CO2_mol_per_m3",names_to=pars)

#parameter als extra spalte aus character ausschneiden
# for(i in pars){
#   sweep_long[,i] <- as.numeric(str_extract(sweep_long$par,paste0("(?<=",i,"=)",value_regexp)))
# }

#einheit in ppm
#sweep_long$CO2_mod <- ppm_to_mol(sweep_long$CO2_mol_per_m3,"mol/m^3","units")

#tiefe umrechnen
#sweep_long$tiefe <- set_units(sweep_long$z - z_soil_cm,cm)


##########################################
#modell mit obs vergleichen
############################################

DS_list <- list()
F_list <- list()

n_best <- 10
#subset für datum bei der Kammermessung durchgeführt wurde
F_df <- data.frame(date=mod_dates,Fz=NA)
F_df[,paste0("DS_",rep(c("","min_","max_","sorted_"),each=n_DS),1:n_DS)]<- NA
for(k in seq_along(mod_dates)){
  if(round(k / length(mod_dates)*100,2) %% 10 == 0){
print(paste0(k / length(mod_dates)*100,"% ",mod_dates[k]))}
kammer_date <- mod_dates[k]



#CO2_obs <- subset(data_agg_mod,date_hour== kammer_date)
CO2_obs <- subset(data,date== kammer_date)
D0_CO2_m2 <- mean(D0_T_p(T_C = CO2_obs$T_soil,p_kPa = CO2_obs$PressureActual_hPa/10,unit="m2/s")) #20°C m2/s

#CO2_obs <- subset(data_agg,hour== kammer_date)
CO2_obs$z <- z_soil_cm + CO2_obs$tiefe
#umsortieren
CO2_obs <- CO2_obs[order(-CO2_obs$tiefe),]

#plot des gemessenen Tiefenprofils 
#ggplot(CO2_obs)+geom_path(aes(CO2_mol_per_m3,tiefe))

if(any(CO2_obs$CO2_mol_per_m3>0,na.rm = T)){
#Injecitonsrate bei Kammermessungen
injection_rate_obs <-round(unique(CO2_obs$inj_mol_m2_s),6)

sweep_inj_rates <- as.numeric(unique(sweep_long$injection_rate))
injection_rate_i <- sweep_inj_rates[which.min(abs(injection_rate_obs - sweep_inj_rates))]
#subset des Sweeps mit nur der richtigen injektionsrate
sweep_sub_id <- grep(paste0("injection_rate=",injection_rate_i),colnames(CO2_sweep))
sweep_sub <- CO2_sweep[,sweep_sub_id]

#rmse jedes Sweeps berechnen
rmse <- apply(sweep_sub,2,RMSE,CO2_obs$CO2_mol_per_m3)
#rmse <- apply(sweep_sub,2,RMSE,CO2_obs$CO2_mol_per_m3,normalize="mean_each")
#rmse <- apply(sweep_sub,2,function(x) RMSE(x[1:4],CO2_obs$CO2_mol_per_m3[1:4]))
#rmse <- apply(sweep_sub,2,function(x) sum(sapply(x,RMSE,obs=CO2_obs$CO2_mol_per_m3,normalize="mean")))

#zu den RMSE werten die jeweiligen DS sets
DS_mat_ch <- str_extract_all(names(rmse),"(?<=DS_\\d=)\\d(\\.\\d+)?E-\\d",simplify = T)
DS_mat <- as.data.frame(apply(DS_mat_ch,2,as.numeric))

colnames(DS_mat) <- str_subset(pars,"DS")
#dataframe mit RMSE und DS Sets
DS_wide <- cbind(rmse,DS_mat)
#nur die Paramtersets mit aufsteigendem DS
#DS_sorted <- DS_wide[which(DS_wide[,2] >= DS_wide[,3] & DS_wide[,3] >= DS_wide[,4] &DS_wide[,4] >= DS_wide[,5] ),]
DS_sorted <- DS_wide[which(DS_wide[,2] >= DS_wide[,3] & DS_wide[,3] >= DS_wide[,4]),]
#DS im ,ong format
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
best.fit.id2 <- which.min(DS_sorted$rmse)
#best.rmse <- min(rmse)
#best.rmse_sorted <- min(DS_sorted$rmse)

#Bester Parametersatz
best_DS <- as.numeric(DS_mat[best.fit.id,])

good_DS_chr <- DS_mat[good.fit.id,]
min_DS <- apply(good_DS_chr,2,function(x) min(as.numeric(x)))
names(min_DS) <- paste0("DS_min_",1:length(min_DS))
max_DS <- apply(good_DS_chr,2,function(x) max(as.numeric(x)))
names(max_DS) <- paste0("DS_max_",1:length(max_DS))
names(best_DS) <- colnames(DS_mat)
best_DS_sorted <- as.numeric(DS_sorted[best.fit.id2,-1])
names(best_DS_sorted) <- paste0("DS_sorted_",1:length(best_DS_sorted))
DS_vec <- c(best_DS,max_DS,min_DS,best_DS_sorted)
#DS_list[[as.character(mod_dates)[k]]] <- best_DS
#DS_list[[as.character(mod_dates)[k]]] <- DS_vec

F_df[F_df$date == mod_dates[k],names(DS_vec)] <- DS_vec
################
#flux
slope_0_7cm <- glm(CO2_ref ~ tiefe, data= subset(CO2_obs,tiefe >= -7))#ppm/cm
#plot(obs_j$tiefe,obs_j$CO2_ref)
#abline(slope_0_7cm)
dC_dz <- -slope_0_7cm$coefficients[2]

# slope_0_7cm_inj <- glm(CO2_inj ~ tiefe, data= subset(obs_j,tiefe >= -7))#ppm/cm
# dC_dz_inj <- -slope_0_7cm_inj$coefficients[2]
# dC_dz_mol_inj <- ppm_to_mol(dC_dz_inj,"ppm",out_class = "units")#mol/m^3/cm
# Fz_mumol_per_s_m2_inj <- best_DS$DS_1  * dC_dz_mol_inj * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2
# F_Comsol$Fz_inj[F_Comsol$date == mod_dates_all[[j]]] <- Fz_mumol_per_s_m2_inj

dC_dz#ppm/cm
#DS = -FZ * dz / dC

dC_dz_mol <- ppm_to_mol(dC_dz,"ppm",out_class = "units",T_C = CO2_obs$T_soil[CO2_obs$tiefe == -3.5],p_kPa = unique(CO2_obs$PressureActual_hPa)/10)#mol/m^3/cm

Fz_mumol_per_s_m2 <- best_DS_sorted[1]  * dC_dz_mol * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2
names(Fz_mumol_per_s_m2) <- "Fz"
#Fz_mumol_per_s_m2 <- 3.603326e-06  * dC_dz_mol * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2
#F_list[[as.character(mod_dates)[k]]] <- Fz_mumol_per_s_m2
F_df$Fz[F_df$date == mod_dates[k]] <- Fz_mumol_per_s_m2
}
}
#########################################
#ende for loop
save(F_df,file=paste0(comsolpfad,"F_df_gam_3DS.RData"))
#F_df <- as.data.frame(do.call(rbind,F_list))
F_df$date <- ymd_hms(rownames(F_df))
DS_df <- as.data.frame(do.call(rbind,DS_list))
DS_df$date <- ymd_hms(rownames(DS_df))

DS_long <-tidyr::pivot_longer(F_df,starts_with("DS"),names_pattern = "(\\w+)_(\\d)",names_to = c(".value","id"))

F_df$Fz[F_df$date > (round_date(Pumpzeiten$start,"hours")[13]-3600) & F_df$date < (round_date(Pumpzeiten$start,"hours")[13]+10*3600)]<-NA

  F_df$Fz_roll <- zoo::rollapply(F_df$Fz,width=120,mean,fill=NA)

ggplot(subset(Kammer_flux))+
  geom_errorbar(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max,col=kammer),width=10000)+
  geom_point(aes(date,CO2flux,col=kammer))+
  ggnewscale::new_scale_color()+
  #geom_point(data=subset(F_df),aes(date,Fz))+
  geom_line(data=subset(F_df),aes(date,Fz,col="raw"),alpha=0.3)+
  geom_line(data=subset(F_df),aes(date,Fz_roll,col="roll"))+
  #geom_point(data=F_Comsol_snopt,aes(date,Fz,col="glm"))+
  #geom_point(data=F_Comsol,aes(date,Fz_inj,col=""))+
  scale_color_manual("COMSOL",values=1:2)+
  #xlim(c(min(F_df$date[-1]),max(F_df$date[])+3600*5))+
  xlim(ymd_h(c("2020.07.04 8","2020.07.14 19")))+
  #xlim(ymd_h(c("2020.07.07 0","2020.07.08 19")))+
  labs(title=paste(offset_method,n_DS),y=expression(CO[2]*"flux ["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))
#F_df_gam <- F_df


Kammer_flux$date

ggplot(DS_long)+
  geom_ribbon(aes(x=date,ymin=DS_min,ymax=DS_max,fill=id),alpha=0.2)+
  geom_line(aes(date,DS,col=id))+
  geom_line(aes(date,DS_sorted,col=id))
#mal anschauen
paste(names(best_DS),best_DS)
##############################################################

Kammer_agg <- Kammer_flux %>% group_by(day) %>% summarise(kammer_min = min(CO2flux_min),kammer_max = max(CO2flux_max), kammerflux = mean(CO2flux),date =mean(date))

Kammer_agg <- subset(Kammer_agg,day%in%ymd(c("2020.07.08","2020.07.10","2020.07.14")))
Kammer_agg$Fz_mod <- NA
F_sub <- subset(F_df, !is.na(Fz_roll))
for(i in 1:nrow(Kammer_agg)){
  Kammer_agg$Fz_mod[i] <- F_sub$Fz_roll[which.min(abs(F_sub$date - Kammer_agg$date[i]))]
}
ggplot(Kammer_agg)+geom_errorbar(aes(x=Fz_mod,ymin=kammer_min,ymax=kammer_max))+geom_abline(slope=1,intercept = 0)
Kammer_sub <- subset(CO2_flux,day%in%ymd(c("2020.07.08","2020.07.10","2020.07.14")) & kammer != "D")
Kammer_sub$Fz_mod <- NA
F_sub <- subset(F_df, !is.na(Fz_roll))
for(i in 1:nrow(Kammer_sub)){
  Kammer_sub$Fz_mod[i] <- F_sub$Fz_roll[which.min(abs(F_sub$date - Kammer_sub$date[i]))]
}
f_range <- range(Kammer_sub[,c("Fz_mod","mumol_per_s_m2")])
ggplot(Kammer_sub)+
  #geom_errorbar(aes(x=Fz_mod,ymin=CO2flux_min,ymax=CO2flux_max,col=kammer))+
  geom_point(aes(x=Fz_mod,y=mumol_per_s_m2,col=kammer))+
  geom_abline(slope=1,intercept = 0)+lims(x = f_range, y = f_range)
ggplot(Kammer_agg)+
  geom_point(aes(date,kammerflux,col="obs"))+
  geom_errorbar(aes(x=date,ymin=kammer_min,ymax=kammer_max,col="obs"))+
  geom_point(aes(date,Fz_mod,col="mod"))

F_Comsol$day <- as_date(F_Comsol$date)
F_vergleich <- merge(F_Comsol,Kammer_agg,by="day")
ggplot(F_vergleich)+geom_point(aes(kammerflux,Fz))+geom_abline(intercept = 0,slope=1)+geom_errorbar(aes(xmin=kammer_min,xmax=kammer_max,y=Fz))+xlim(c(1,5.5))+ylim(c(1,5.5))





####################################################
##ALT

###################################################################################
#write.table(paste(names(best_DS),best_DS),file=paste0(comsolpfad,"best_DS_Hartheim.txt"),col.names = F,row.names = F,quote = F)
DS_D0 <- best_DS/D0_CO2_m2 #m2/s

#####################################################
#vorbereitung für den DS plot
##################################################
if(length(best_DS) == 4){
  schichten <- 4
  schicht_grenzen <- seq(0,by=-7,length.out = schichten)
  tiefen <- seq(-3.5,by=-7,length.out = schichten)
}else if(length(best_DS) == 3){
  schichten <- 3
  schicht_grenzen <- c(0,-10.5,-21)
  tiefen <- c(-5.25,-15.75,-24.5)
}else if(length(best_DS) == 2){
  schichten <- 3
  schicht_grenzen <- c(0,-14)
  tiefen <- c(-7,-24.5)
}


schicht_untergrenzen <- c(schicht_grenzen[-1],-z_soil_cm)
DS_profil <- data.frame(DS=unlist(best_DS),tiefe=tiefen,top=schicht_grenzen,bottom=schicht_untergrenzen)
#für die vierte Schicht gibt es nicht genug punkte
#DS_profil$DS[4] <- NA

#CO2_obs$CO2_mod <- ppm_to_mol(sweep_sub[,best.fit.id],"mol/m^3")
CO2_obs$CO2_mod <-sweep_sub[,best.fit.id]
#CO2_obs$CO2_mod_sorted <- ppm_to_mol(sweep_sub[,colnames(sweep_sub) == rownames(DS_sorted[best.fit.id2,])],"mol/m^3")


CO2_obs$DS <- approx(DS_profil$bottom,DS_profil$DS,CO2_obs$tiefe,method="constant",rule=2)$y
CO2_obs$DS_D0 <- approx(DS_profil$bottom,DS_profil$DS_D0,CO2_obs$tiefe,method="constant",rule=2)$y
CO2_obs$DS[CO2_obs$tiefe > 0] <- NA

#DS_profil für plot ins long format
DS_profil_long <- reshape2::melt(DS_profil[,c("DS","tiefe","top","bottom")],id="DS",value.name="tiefe")
#DS_profil_long_sorted <- reshape2::melt(DS_profil[,c("DS_sorted","tiefe","top","bottom")],id="DS_sorted",value.name="tiefe")
DS_profil_long <- DS_profil_long[order(DS_profil_long$tiefe),]
#DS_profil_long_sorted <- DS_profil_long_sorted[order(DS_profil_long_sorted$tiefe),]

#Wertebereich des Sweeps
range_mod <- aggregate(sweep_long$CO2_mod,list(tiefe = sweep_long$tiefe),range)
CO2_obs$max_mod <- rev(range_mod$x[,2])
CO2_obs$min_mod <- rev(range_mod$x[,1])

############################################
#plots
########################################
#
sweep_plot <- ggplot(CO2_obs)+
  #geom_ribbon(aes(xmin=min_mod,xmax=max_mod,y=tiefe,fill="sweep"),alpha=0.3)+
  geom_hline(yintercept = schicht_grenzen)+
  geom_line(aes(CO2_mod,tiefe,col="mod"))+
  geom_point(aes(CO2_mol_per_m3,tiefe,col="obs"))

DS_D0_plot <- ggplot(CO2_obs)+geom_path(aes(DS_D0,tiefe))
DS_plot <- ggplot(DS_profil_long)+geom_path(aes(DS,tiefe))+
  ylim(range(CO2_obs$tiefe))
best_DS
ds_profil_plot <- egg::ggarrange(sweep_plot,DS_plot,ncol=2,widths = c(2,1))

sweep_plot_sorted <- ggplot(CO2_obs)+
  geom_ribbon(aes(xmin=min_mod,xmax=max_mod,y=tiefe,fill="sweep"),alpha=0.3)+
  geom_hline(yintercept = schicht_grenzen)+
  geom_line(aes(CO2_mod_sorted,tiefe,col="mod"))+
  geom_point(aes(CO2_tracer,tiefe,col="obs"))

DS_D0_plot <- ggplot(CO2_obs)+geom_path(aes(DS_D0,tiefe))
DS_plot_sorted <- ggplot(DS_profil_long_sorted)+geom_path(aes(DS_sorted,tiefe))+
  ylim(range(CO2_obs$tiefe))

ds_profil_plot_sorted <- egg::ggarrange(sweep_plot_sorted,DS_plot_sorted,ncol=2,widths = c(2,1))

pdf(paste0(plotpfad,"DS_profil.pdf"),width=8,height=5)
ds_profil_plot
dev.off()
pdf(paste0(plotpfad,"DS_profil_sorted.pdf"),width=8,height=5)
ds_profil_plot_sorted
dev.off()


# ggplot(data_agg)+
#   geom_point(aes(CO2_inj_1.5,tiefe))+
#   geom_point(aes(CO2_ref_1.5,tiefe))

#slope_0_7cm <- glm(CO2_inj ~ tiefe, data= subset(CO2_obs,tiefe >= -7))#ppm/cm
slope_0_7cm <- glm(CO2_ref ~ tiefe, data= subset(CO2_obs,tiefe >= -7))#ppm/cm
#slope_0_20cm <- glm(CO2_inj ~ tiefe, data= subset(CO2_obs,tiefe >= -15))#ppm/cm
#slope_0_20cm <- glm(CO2_ref ~ tiefe, data= subset(CO2_obs,tiefe >= -15))#ppm/cm
ggplot(CO2_obs)+
  geom_path(aes(CO2_ref + offset,tiefe,col="ref + offset"))+
  geom_path(aes(CO2_ref,tiefe,col="ref"))+
  geom_path(aes(CO2_tracer,tiefe,col="tracer"))+
  geom_path(aes(CO2_inj,tiefe,col="inj"))
dC_dz <- -slope_0_7cm$coefficients[2]
#dC_dz <- -slope_0_20cm$coefficients[2]
dC_dz#ppm/cm
#DS = -FZ * dz / dC

dC_dz_mol <- ppm_to_mol(dC_dz,"ppm",out_class = "units")#mol/m^3/cm


Fz_ml_per_min_m2
colnames(CO2_obs)
ggplot(CO2_obs)+geom_path(aes(CO2_tracer,tiefe))

CO2_obs$r <- 0
for(i in 1:7){
  write.table(CO2_obs[CO2_obs$tiefe == (1:7*-3.5)[i],"CO2_mol_per_m3"],paste0(metapfad_comsol,"dom",i,".csv"),col.names = F,row.names = F,sep=",")
}  
CO2_obs$z <- CO2_obs$z -100
write.table(na.omit(CO2_obs[,c("r","z","CO2_mol_per_m3")]),paste0(metapfad_comsol,"CO2_obs.csv"),col.names = F,row.names = F,sep=",")
