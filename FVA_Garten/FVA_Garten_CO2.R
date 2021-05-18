#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_FVAgarten <- paste0(hauptpfad,"Daten/aufbereiteteDaten/FVA_Garten/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","ggforce")
check.packages(packages)

load(file = paste(datapfad_FVAgarten,"injectionrates.RData"))
load(file = paste(datapfad_FVAgarten,"swc_long.RData"))
load(file=paste(datapfad_FVAgarten,"chamber_flux.RData"))

datelim <- ymd_h("2021.03.30 15")

range1 <- ymd_h(c("2021.04.19 01","2021.04.28 00"))
data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")
data_probe3 <-  read_sampler("sampler3",datelim = range1, format = "long")


# data_probe3 <- data_probe3 %>%
#   group_by(tiefe) %>%
#   mutate(
#     CO2 = RcppRoll::roll_mean(CO2,n=50,fill=NA,na_rm=T)
#   ) %>%
#   ungroup() %>%
#   as.data.frame()

colnames(data_probe3) <- str_replace(colnames(data_probe3),"CO2","CO2_smp3")
colnames(data_probe3) <- str_replace(colnames(data_probe3),"temp","T_soil")

data_probe3$date <- round_date(data_probe3$date,"mins") 
data <- merge(data_probe1u2,data_probe3,all.x=T)


range(inj$date)
inj$date <- inj$date - 3600
range(inj$date)

timediff_inj <- which(difftime(inj$date[-1],inj$date[-nrow(inj)],units= "hours") > 12)
inj_periods <- data.frame(
  start=inj$date[c(1,timediff_inj + 1)],
  stop=inj$date[c(timediff_inj, nrow(inj))]
)

data$injection <- 0
for(i in 1:nrow(inj_periods)){
  data$injection[data$date > inj_periods$start[i] & data$date < inj_periods$stop[i]] <- 1
}

#kurven glätten mit rollapply
data <- data %>% 
  group_by(tiefe) %>% 
  mutate(
    CO2_inj = RcppRoll::roll_mean(CO2_smp1,n=50,fill=NA),
    CO2_ref = RcppRoll::roll_mean(CO2_smp2,n=50,fill=NA)
  ) %>% 
  ungroup() %>% 
  as.data.frame()

############################
#injecitonrate mit zeitlichem drift bestimmen
data$inj_ml_min <- 0

data[data$injection==1,"inj_ml_min"] <- approx(x=(inj$date),y=inj$CO2_ml_per_min,xout=(data$date[data$injection==1]),rule=1)$y


A_inj <- set_units(1^2*pi,"mm^2")


inj_mol_min <- ppm_to_mol(data$inj_ml_min,"cm^3/min",out_class = "units",T_C = data$T_C)

inj_mol_mm2_s <- set_units(inj_mol_min,"mol/s")/A_inj
data$inj_mol_m2_s <- set_units(inj_mol_mm2_s,"mol/m^2/s")



ggplot(data)+
  geom_line(aes(date, inj_mol_m2_s))

######################################
#adjustment
data$Versuch <- NA
range1 <- ymd_h(c("2021.04.19 00","2021.04.28 00"))

daterange <- function(dates,range) {
  dates_in_range <- which(dates >= min(range) & dates <= max(range))
  return(dates_in_range)
}

data$hm <- as.numeric(paste(format(data$date,"%H%M")))
data$Versuch[daterange(data$date,range1)] <- 1
data$CO2_ref_drift <- NA
data$CO2_ref_gam <- NA
data$CO2_ref_amp <- NA
data$offset_drift <- NA
data$offset_gam <- NA
data$offset <- data$CO2_inj - data$CO2_ref
data$date_int <- as.numeric(data$date)

adj_periods <- lapply(na.omit(unique(data$Versuch)),function(x) subset(data, inj_ml_min == 0 & Versuch == x))


#for(j in seq_along(adj_periods)){
j <- 1
  for(i in (1:7)*-3.5){
    fm_drift <- glm(offset ~ poly(date_int,2),data=subset(adj_periods[[j]],tiefe==i))
    fm_gam <- mgcv::gam(offset ~ poly(date_int,2) + s(hm) ,data=subset(adj_periods[[j]],tiefe==i))
    
    ID <- which(data$tiefe==i & data$Versuch == 1)
    
    data$offset_drift[ID] <- predict(fm_drift,newdata = data[ID,])
    data$offset_gam[ID] <- predict(fm_gam,newdata = data[ID,])
    
    data$CO2_ref_drift[ID] <- data$CO2_ref[ID] + data$offset_drift[ID]
    data$CO2_ref_gam[ID] <- data$CO2_ref[ID] + data$offset_gam[ID]
    ID2 <- which(data$tiefe==i & !is.na(data$CO2_ref_drift) & data$Versuch == 1)
    fm_amp <- glm(CO2_inj ~ poly(CO2_ref_drift,2),data=subset(data[ID2,],injection== 0))
    data$CO2_ref_amp[ID2] <- predict(fm_amp,newdata = data[ID2,])

    
  }
#}

data$CO2_tracer_drift <- data$CO2_inj - data$CO2_ref_drift
data$CO2_tracer_amp <- data$CO2_inj - data$CO2_ref_amp
data$CO2_tracer_gam <- data$CO2_inj - data$CO2_ref_gam



data_sub <- subset(data,Versuch==1 & injection==1 & !is.na(CO2_tracer_amp))
colnames(data_sub)


###################
#COMSOL

mod_dates <- unique(round_date(data_sub$date,"hours"))[-1]
colnames(data_sub)
comsol_out <- run_comsol(data=data_sub,mod_dates = mod_dates,offset_method = "amp",read_all = F)
colnames(comsol_out)
Deff <- subset(comsol_out,date > min(comsol_out$date) + 10*3600)

ggplot(Deff)+
  geom_line(aes(date,DSD01,col="DSD01"))+
  geom_line(aes(date,DSD02,col="DSD02"))+
  geom_line(aes(date,DSD03,col="DSD03"))

#############################
#flux calculation
########################
colnames(data)
data$CO2_ref_amp[data$tiefe == 0] <- data$CO2_ref[data$tiefe == 0]
data$T_soil[data$tiefe == 0] <- data$T_soil[data$tiefe == -3.5]

data$CO2_ref_mol_m3 <- ppm_to_mol(data$CO2_ref_amp,"ppm",out_class = "units",T_C = data$T_soil)

data_wide_CO2 <- tidyr::pivot_wider(data[data$date %in% Deff$date,],date,names_from=tiefenstufe,values_from = CO2_ref_mol_m3,names_prefix = "CO2_ref_")

F_df <- merge(Deff,data_wide_CO2)

dC_dz_mol_0_10 <- rowMeans(cbind(F_df$CO2_ref_0 - F_df$CO2_ref_1,F_df$CO2_ref_1 - F_df$CO2_ref_2,F_df$CO2_ref_2 - F_df$CO2_ref_3)/-3.5)

dC_dz_mol_10_17 <- rowMeans(cbind(F_df$CO2_ref_3 - F_df$CO2_ref_4,F_df$CO2_ref_4 - F_df$CO2_ref_5)/-3.5)
dC_dz_mol_ab20 <- rowMeans(cbind(F_df$CO2_ref_5 - F_df$CO2_ref_6,F_df$CO2_ref_6 - F_df$CO2_ref_7)/-3.5)
#mol/m^3/cm
#einheit in mol / m3 /cm

#Ficks Law
#Fz_mumol_per_s_m2 <- F_df$DS_1[k]  * dC_dz_mol * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2

F_df$Fz_0_10 <- F_df$DS1   * dC_dz_mol_0_10 * 100 * 10^6#m2/s * mol/m3/m = mumol/s/m2
F_df$Fz_10_17 <- F_df$DS2   * dC_dz_mol_10_17 * 100 * 10^6#m2/s * mol/m3/m = mumol/s/m2
F_df$Fz_ab20 <- F_df$DS3   * dC_dz_mol_ab20 * 100 * 10^6#m2/s * mol/m3/m = mumol/s/m2

flux_agg <- merge(flux_agg,data_probe3,all.x = T)
ggplot(F_df)+
  geom_line(aes(date,Fz_0_10))+
  geom_point(data=flux_agg,aes(date,CO2_mumol_per_s_m2))
flux_agg$CO2_mumol_per_s_m2
######################
#plots
####################


############################
#plot offset function
ggplot(subset(data,inj_ml_min==0 & Versuch==1))+
  geom_line(aes(date,offset,col=as.factor(tiefe)))+
  geom_line(aes(date,offset_drift,col=as.factor(tiefe)))

#################################
#plot adjustment
ggplot(subset(data,Versuch==1))+
  geom_ribbon(aes(date,ymin=CO2_ref_amp,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.3)+
  #geom_ribbon(aes(date,ymin=CO2_ref_gam,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.3)+
  geom_line(aes(date,CO2_ref_drift,col=as.factor(tiefe),linetype="ref drift"))+
  geom_line(aes(date,CO2_ref_amp,col=as.factor(tiefe),linetype="ref amp"))+
  geom_line(aes(date,CO2_ref_gam,col=as.factor(tiefe),linetype="ref gam"))+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe),linetype="inj"))

############################
#tracer plot drift amp
ggplot(subset(data,Versuch==1))+
  geom_line(aes(date,CO2_tracer_drift,col=as.factor(tiefe),linetype="tracer drift"))+
  geom_line(aes(date,CO2_tracer_amp,col=as.factor(tiefe),linetype="tracer amp"))

########################
#tracer plot gam
ggplot(subset(data,Versuch==1))+
  geom_line(aes(date,CO2_tracer_gam,col=as.factor(tiefe)))
range(data$date[data$injection==1 & data $Versuch == 1],na.rm=T)
# range(inj$date)
# which(is.na(data$date))

data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")
data_probe3 <-  read_sampler("sampler3",datelim = datelim, format = "long")
smp1 <- ggplot(data_probe1u2)+
  geom_line(aes(date,CO2_smp1,col=as.factor(tiefe)))+guides(col=F)
smp2 <- ggplot(data_probe1u2)+
  geom_line(aes(date,CO2_smp2,col=as.factor(tiefe)))
smp3 <- ggplot(data_probe3)+
  geom_line(aes(date,CO2,col=as.factor(tiefe)))+guides(col=F)

egg::ggarrange(smp1,smp2,smp3,ncol=1)
