hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Kammermessungen_Arduino"
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)

theme_set(theme_classic())
##
load(file = paste(datapfad_PP_Kammer,"injectionrates_hartheim.RData"))

median(inj$CO2_mumol_per_s)
median(inj$CO2_ml_per_min)
#######################################
#####################################
#read probes
Versuch <- 7
overwrite <-  T
plot <- T
load(file=paste0(datapfad_PP_Kammer,"data_tracer_hartheim.RData"))
data_all <- data

#for(Versuch in 1:8){

data <- data_all[data_all$Versuch == Versuch & !is.na(data_all$Versuch),]
data_uncert_sub <- data_uncert[data_uncert$Versuch == Versuch & !is.na(data_uncert$Versuch),] %>% 
  #filter(tiefe > -21) %>% 
  mutate(inj_meanr6 = RcppRoll::roll_meanr(inj_mol_m2_s,6*6,fill=NA),
         inj_meanr12 = RcppRoll::roll_meanr(inj_mol_m2_s,12*6,fill=NA),
  )


data <- data %>% 
  group_by(tiefe) %>% 
  #filter(tiefe > -21) %>% 
  mutate(CO2_tracer_roll = RcppRoll::roll_mean(CO2_tracer_drift,5,fill=NA),
         inj_meanr6 = RcppRoll::roll_meanr(inj_mol_m2_s,6*6,fill=NA),
         inj_meanr12 = RcppRoll::roll_meanr(inj_mol_m2_s,12*6,fill=NA),
  )

CO2_plot <- 
  ggplot(data)+
  geom_ribbon(aes(date,ymin=CO2_refadj,ymax=CO2_inj,fill=as.factor(-tiefe)),alpha=0.2)+
  geom_line(aes(date,CO2_refadj,col=as.factor(-tiefe),linetype="ref adj"))+
  geom_line(aes(date,CO2_inj,col=as.factor(-tiefe),linetype="inj"))+
  theme(axis.title.x = element_blank())+
  labs(y=expression(CO[2]~(ppm)),linetype="profile",col="depth (cm)",fill="depth (cm)")


#     
##########################################
## COMSOL
##########################################
names(data)
NA_tiefen <- data %>% 
  group_by(tiefe) %>%
  summarise(n_NAS = length(which(!is.na(CO2_tracer_drift))) / length(CO2_tracer_drift))
  
tiefen <- NA_tiefen$tiefe[NA_tiefen$n_NAS > 0.9]
#tiefen <- 4:5*-3.5
#tiefen <- tiefen[-1]
nDS <- 1
file_suffix <- "_40cm"
#file_suffix <- ""
file_i <- paste0(datapfad_PP_Kammer,"DSD0_hartheim_",nDS,"DS_",Versuch,file_suffix,".RData")
if(file.exists(file_i) & overwrite == F){
  load(file_i)
}else{
  data$inj_mol_m2_s <- data$inj_meanr6
  data_uncert_sub$inj_mol_m2_s <- data_uncert_sub$inj_meanr6
  
  comsol <- comsol_sweep(data = subset(data,tiefe %in% tiefen),
                         tracer_colname = "CO2_tracer_drift",
                         intervall = "60 mins",
                         #DS_ratio = eps_ratio,
                         filename = paste0("freeSoil_anisotropy_sweep_",nDS,"DS",file_suffix,".txt"),
                         extend = ifelse(nDS>1,T,F),
                         plot = "CO2",
                         byout= 1e-7)
  comsol_min <- comsol_sweep(data = subset(data_uncert_sub,tiefe %in% tiefen),
                             tracer_colname = "CO2_tracer_min",
                             intervall = "60 mins",
                             #DS_ratio = eps_ratio,
                             filename = paste0("freeSoil_anisotropy_sweep_",nDS,"DS",file_suffix,".txt"),
                             extend = ifelse(nDS>1,T,F),
                             byout= 1e-7)
  comsol_max <- comsol_sweep(data = subset(data_uncert_sub,tiefe %in% tiefen),
                             tracer_colname = "CO2_tracer_max",
                             #DS_ratio = eps_ratio,
                             intervall = "60 mins",
                             filename = paste0("freeSoil_anisotropy_sweep_",nDS,"DS",file_suffix,".txt"),
                             extend = ifelse(nDS>1,T,F),
                             byout= 1e-7)
  
  save(comsol,comsol_min,comsol_max,file=file_i)
  
}



range(subset(comsol,tiefe==1)$DS)

comsol$DSD0_min <- comsol_min$DSD0
comsol$DSD0_max <- comsol_max$DSD0
comsol_sub <- sub_daterange(comsol,range(comsol$date) + 3600 * 10 * c(1,0))
comsol_wide <- tidyr::pivot_wider(comsol_sub,names_from = tiefe,values_from = matches("DS"))
comsol_CO2_long <- tidyr::pivot_longer(comsol_wide,matches("CO2"),names_to = c(".value","tiefe"),names_pattern = "(CO2.*)_(\\d)")
comsol_CO2_long$tiefe <- as.numeric(comsol_CO2_long$tiefe)*3.5
#DSD0plt <- 
  ggplot(comsol_sub)+
  #geom_ribbon(aes(x=date,ymin=DSD0_min,ymax=DSD0_max,fill=as.factor(tiefe)),alpha=0.3)+
  geom_line(aes(date,DS,col=as.factor(tiefe)))+
  guides(col=F,fill=F)+
  labs(y=expression(D[S]/D[0]))

###################################################################
#genauerer Blick auf erste PPC Periode

pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen_hartheim.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)
names(pp_chamber)
pp_chamber_sub <- sub_daterange(pp_chamber[,c("Start","Ende","step_hours","Modus")],range(data$date),"Start")


PPC_daterange_short <-range(ymd_hms(t(pp_chamber_sub[,c("Start","Ende")])))+3600*1*c(-1,1)
PPC_daterange <-range(ymd_hms(t(pp_chamber_sub[,c("Start","Ende")])))+3600*10*c(-1,1)
#PPC_daterange <- ymd_h("2022.05.10 00","2022.05.11 15")




####################
#PP

##################
#Metadata

data_PPC <- read_PP(datelim = range(data$date))
PPC_wide <- read_PP(datelim = PPC_daterange_short,format="wide")
PPC_wide <- PPC_wide %>% 
  mutate(dP_horiz_raw = (abs(P_1 - P_2) + abs(P_2 - P_3))/2,
         dP_horiz = RcppRoll::roll_mean(dP_horiz_raw,10*60,fill=NA))

if(nrow(data_PPC) > 0){
  data_PPC <- subset(data_PPC,id != 5)
  dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T),2)
  
  data_PPC <- data_PPC %>% 
    filter(id != "outside") %>% 
    group_by(id) %>%
    mutate(dt = diff_time(date,"secs"),
           #P_diff = abs(c(NA,diff(P_filter)))/!!dt,
           PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
           PPC_diff = abs(c(NA,diff(PPC))),
           P_roll = RcppRoll::roll_mean(P,3*60/!!dt,fill=NA))
  
  data_PPC$id[data_PPC$id == 6] <- "outside"
  data_PPC[which(data_PPC$dt > 3600),c("PPC","PPC5","P_roll")] <- NA
  
  data_PPC <- sub_daterange(data_PPC,PPC_daterange_short)
  
  step_thr <- 0.05
  PPC_steps <- data_PPC %>%
    filter(id %in% 1:4) %>%
    mutate(date = round_date(date,"10 min")) %>%
    group_by(id,date) %>%
    summarise(across(everything(),mean)) %>%
    mutate(PPC_diff = abs(c(NA,diff(PPC5))),
           step = ifelse(PPC_diff > step_thr,1,0))
  
  
  step_date <- unique(PPC_steps$date[PPC_steps$step == 1])
  step_date <- sort(step_date)
  step_date <- step_date[c(as.numeric(diff(step_date)),100) > 60]
  step_date <- step_date[!is.na(step_date)]
  
  
  
  PP_plot <- 
    ggplot(subset(data_PPC,date %in% round_date(date,"mins") & id == 1))+
    geom_line(aes(date,RcppRoll::roll_mean(PPC5,15,fill = NA),linetype="PPC"))+
    guides(fill=F)+
    scale_linetype("",limits = c("PPC","PPC6h"),labels = expression(PPC,PPC["6h"]))+
    theme(legend.text.align = 0)+
    labs(x="",y="PPC (Pa/s)")
  
  P_roll_plot <- ggplot(subset(data_PPC,id %in% 1:4) )+
    geom_hline(yintercept = 0,col="grey",linetype=2)+
    geom_line(aes(date,P_roll,col=id),alpha=0.5, linetype=2)+
    xlim(PPC_daterange)+
    labs(x="",y=expression(P["moving average"]~"(Pa)"))#+
  dP_horiz_plot <- ggplot(PPC_wide)+
    geom_line(aes(date,dP_horiz))
}

####################################
#merge comsol output with PPC
data_PPC_sub <- data_PPC %>% 
  filter(id == 1) %>% 
  #mutate(date = round_date(date,"10 mins")) %>% 
  select(-id) #%>% 
#group_by(date) %>% 
#summarise(across(everything(),mean))
data_merge <- merge(comsol_sub,data_PPC_sub,all = T)
data_merge <- data_merge %>% 
  mutate(PPC5_int = imputeTS::na_interpolation(PPC5)) %>% 
  filter(!is.na(DSD0)) 
data_merge2 <- data_merge %>% 
  mutate(PPC_meanr6 = RcppRoll::roll_meanr(PPC5_int,6,fill=NA))


data_merge <- merge(data_merge,PPC_wide[,c("date","dP_horiz")],all.x = T)
#data_merge <- merge(comsol,data_PPC[data_PPC$id == 1,],all.x = T)
range(data_merge$date)
PPC_daterange

data_merge$step <- NA
for(i in seq_along(step_date)){
  step_i <- step_date[i]
  id <- daterange_id(data_merge,c(step_i-2*3600,step_i-600))
  data_merge$step[id] <- i
  
}
if(plot){
  
  data_merge2$PPC_meanr6[3] <- data_merge2$PPC5[3]
  PP_plot <- PP_plot+
    geom_line(data=subset(data_merge2,!is.na(PPC_meanr6)),aes(date,PPC_meanr6,linetype="PPC6h"),linetype=2)
  names(comsol)
  tracerplt <- 
    ggplot(subset(comsol_CO2_long,!is.na(CO2_obs)))+
    #geom_rect(data=modes_df,aes(xmin = start,xmax=stop,fill=mode,ymin=-Inf,ymax=Inf),alpha=0.2)+
    geom_line(aes(date,(CO2_obs),col=as.factor(tiefe),linetype="obs"))+#,linetype="PPC"))+
    geom_line(aes(date,(CO2_mod),col=as.factor(tiefe),linetype="mod"))+#,linetype="PPC"))+
    theme(axis.title.x = element_blank())+
    guides(fill=F)+
    labs(y=expression("tracer CO"[2]~(ppm)),col="depth (cm)")
  # tracerplt <- 
  #   ggplot(subset(data,tiefe != 0))+
  #   #geom_rect(data=modes_df,aes(xmin = start,xmax=stop,fill=mode,ymin=-Inf,ymax=Inf),alpha=0.2)+
  #   geom_line(aes(date,(CO2_tracer_roll),col=as.factor(-tiefe)))+#,linetype="PPC"))+
  #   theme(axis.title.x = element_blank())+
  #   guides(fill=F)+
  #   labs(y=expression("tracer CO"[2]~(ppm)),col="depth (cm)")
  
  DSD0_plt <- 
    ggplot(comsol_sub)+
    #geom_rect(data=modes_df,aes(xmin = start,xmax=stop,fill=mode,ymin=-Inf,ymax=Inf),alpha=0.2)+
    geom_ribbon(aes(x=date,ymin=DSD0_min,ymax=DSD0_max,fill=factor(tiefe)),alpha=0.2)+#,fill=factor(tiefe,levels=1:2,labels=c("0-20",">20"))),alpha=0.2)+
    geom_line(aes(date,DSD0,col=factor(tiefe)))+#,col=factor(tiefe,levels=1:2,labels=c("0-20",">20"))))+
    theme(axis.title.x = element_blank())+
    labs(y=expression(D[eff]/D[0]),col="",fill="mode")
  
  #########################
  #final plots
  
  #######################
  #DSD0 PPC timeline
  ggpubr::ggarrange(tracerplt+
                      xlim(range(data_merge$date))+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
                    DSD0_plt+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
                    PP_plot+
                      xlim(range(data_merge$date))+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
                    heights = c(2,2,1),ncol=1,
                    legend = "right",align="v")+
    ggsave(filename = paste0(plotpfad_PPchamber,"DSD0_PPC_hartheim_",nDS,"DS_",Versuch,file_suffix,".png"),width=8,height=7)
  
  
  
}

#save(data_merge,file=paste0(datapfad_PP_Kammer,"data_merge_",Versuch,".RData"))
save(data_merge,step_date,file=paste0(datapfad_PP_Kammer,"data_merge_",nDS,"DS_",Versuch,file_suffix,".RData"))
#}

#}
