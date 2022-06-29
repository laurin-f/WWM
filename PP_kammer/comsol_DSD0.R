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

##
load(file = paste(datapfad_PP_Kammer,"injectionrates.RData"))
#######################################
#####################################
#read probes
i <- 1
#CO2 Werte für i-te injektion inklusive 2 tage vorher und nachher
data <- read_sampler(datelim = dates_ls[[i]] + (3600*24*2 * c(-1,1)))
data$tiefe <- data$tiefe

data$Versuch <- i
inj$date <- round_date(inj$date,"10 mins")
#Spalte in inj hat 1er während der injektion
inj_id <- daterange_id(data,dates_ls[[i]])
data$inj <- ifelse(inj_id,1,0)
#Spalte cal hat 1er vor der injektion und danach mit 20 h Abstand nach der injektion da hier der Tracer noch sichtbar ist
cal_id <- daterange_id(data,dates_ls[[i]] + (3600*20 * c(0,1)))
data$cal <- ifelse(cal_id,0,1)
data <- merge(data,inj[,c("date","CO2_mol_m2_s")],all = T)

data <- data %>% 
  group_by(tiefe) %>% 
  mutate(inj_mol_m2_s = imputeTS::na_interpolation(CO2_mol_m2_s),
         inj_mol_m2_s = ifelse(inj == 1 ,inj_mol_m2_s,0),
         offset = CO2_smp2 - CO2_smp1,
         date_int = as.numeric(date),
         CO2_ref = RcppRoll::roll_mean(CO2_smp1,5,fill=NA),
         CO2_inj = RcppRoll::roll_mean(CO2_smp2,5,fill=NA)
  )

data_cal <- subset(data, cal == 1)


####################################
#adj CO2 tracer
#######################################

data$CO2_refadj <- NA
data$offset_drift <- NA
for(i in 1:7){
  fm_drift <- glm(offset ~ poly(date_int,1),data=subset(data_cal,tiefenstufe==i))
  ID <- which(data$tiefenstufe==i)
  data$offset_drift[ID] <- predict(fm_drift,newdata = data[ID,])
  data$CO2_refadj[ID] <- data$CO2_ref[ID] + data$offset_drift[ID]
}
data$CO2_tracer_drift <- data$CO2_inj - (data$CO2_refadj)
data$T_soil <- data$T_C


data$half_hour <- round_date(data$date,"30 mins")

mod_dates <- sort(unique(data$half_hour[data$inj == 1 & data$half_hour == data$date]))

data_mod <- data %>% 
  mutate(date = round_date(date,"30 mins"))%>% 
  group_by(date,tiefe) %>%
  summarise(across(everything(),mean))
    
#####################
#CO2 inj un refadj plot
ggplot(data_mod)+
  geom_ribbon(aes(date,ymin=CO2_refadj,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.2)+
  geom_line(aes(date,CO2_refadj,col=as.factor(tiefe)))+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))


###################
#tracer plot
ggplot(data)+
  geom_line(aes(date,CO2_tracer_drift,col=as.factor(tiefe)))

######################
#injection rate plot
ggplot(data)+
  geom_line(aes(date,inj_mol_m2_s,col=as.factor(cal),group=1))


##########################################
## COMSOL
##########################################

# comsol<- interp_comsol_inj(data=data_mod,
#                            mod_dates = mod_dates[1:3],
#                            offset_method = "drift",
#                            overwrite = F,
#                            read_all = F,
#                            modelname = "Diffusion_freeSoil_anisotropy_optim_3DS_50runs",
#                            nruns=50,
#                            long=T)
comsol<- run_comsol_nruns(data=data_mod,
                           mod_dates = mod_dates[1:20],
                           offset_method = "drift",
                           overwrite = T,
                           read_all = F,
                           modelname = "Diffusion_freeSoil_anisotropy_optim_3DS_50runs_injection_rate",
                           nruns=50,
                           long=T)


comsol_old<- run_comsol(data=data,
                        mod_dates = mod_dates,
                        offset_method = "drift",
                        overwrite = F,
                        read_all = F,
                        modelname = "Diffusion_freeSoil_anisotropy_optim_3DS",
                        long=T)

save(comsol_old,comsol,file=paste0(datapfad_PP_Kammer,"DSD0_comsol.RData"))



###########
#comsol old and new function vergleich
ggplot()+
  geom_line(data=comsol,aes(date,DSD0,group=as.factor(tiefe),col="inter",linetype="inter"))+
  geom_line(data=comsol_old,aes(date,DSD0,group=as.factor(tiefe),col="old",linetype="old"))#+

ggplot()+
  geom_line(data=comsol,aes(date,mod_inj_rate,group=as.factor(tiefe),col="inter",linetype="inter"))+
  geom_line(data=subset(data_mod,date %in% mod_dates),aes(date,inj_mol_m2_s,group=as.factor(tiefe),col="old",linetype="old"))#+




###################################################################
#genauerer Blick auf erste PPC Periode

PPC_daterange <- ymd_h("2022.05.10 00","2022.05.11 15")

#####################
#CO2 inj un refadj plot
ggplot(sub_daterange(data,PPC_daterange))+
  geom_ribbon(aes(date,ymin=CO2_refadj,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.2)+
  geom_line(aes(date,CO2_refadj,col=as.factor(tiefe)))+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))

  



############
#swc
datelim <- range(mod_dates)

load(file = paste(datapfad_PP_Kammer,"klima_DWD.RData"))
if(as.numeric(difftime(now(),max(klima$date),unit="hours")) > 24){
  source("./PP_kammer/klima_dwd.R")
}
klima_sub <- sub_daterange(klima,datelim)
load(file = paste(datapfad_PP_Kammer,"swc_long.RData"))
swc_sub <- sub_daterange(swc_long,datelim)
if(nrow(swc_sub) > 0){
  sec_ax_fac <- 0.7
  swc_min <- min(swc_sub$swc,na.rm = T)/1.1
  swc_plot <- ggplot(swc_sub)+
    geom_ribbon(data=klima_sub,aes(x=date,ymin=swc_min,ymax=P24tot/sec_ax_fac + swc_min),fill="blue",alpha=0.8)+
    geom_line(aes(date,swc,col=as.factor(tiefe)))+
    xlim(PPC_daterange)+
    scale_y_continuous(sec.axis = sec_axis(~(. - swc_min)*sec_ax_fac,name=expression(italic(P)["24h"]*" (mm)")))+
    theme(
      axis.title.y.right = element_text(color = "blue"),
      axis.text.y.right = element_text(color = "blue"),
      #axis.text.x = element_blank()
    )+
    labs(x="",y="SWC (Vol. %)",col="tiefe (cm)")
}

####################
#PP

##################
#Metadata
pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

data_PPC <- read_PP(datelim = range(data$date))

if(nrow(data_PPC) > 0){
  data_PPC <- subset(data_PPC,id != 5)
  dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T),2)
  
  data_PPC <- data_PPC %>% 
    filter(id != "outside") %>% 
    group_by(id) %>%
    mutate(dt = diff_time(date,"secs"),
           P_diff = abs(c(NA,diff(P_filter)))/!!dt,
           PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
           PPC_diff = abs(c(NA,diff(PPC))),
           P_roll = RcppRoll::roll_mean(P,3*60/!!dt,fill=NA))
  
  data_PPC$id[data_PPC$id == 6] <- "outside"
  data_PPC[which(data_PPC$dt > 3600),c("PPC","PPC5","P_roll")] <- NA
  
  
  
  PP_plot <- 
    ggplot(subset(data_PPC,id != "outside"))+
    #geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    #geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_line(aes(date,PPC5,col=id))+
    xlim(PPC_daterange)+
    scale_fill_grey()+
    guides(fill=F)+
    labs(x="",y="PPC (Pa/s)")
  
  P_roll_plot <- ggplot(subset(data_PPC,id %in% 1:4) )+
    geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
    geom_line(aes(date,P_roll,col=id),alpha=0.5, linetype=2)+
    coord_cartesian(xlim=datelim)+
    scale_fill_grey()+
    guides(fill=F,col=F)+
    labs(x="",y=expression(P["moving average"]))
}
data_PPC <- sub_daterange(data_PPC,PPC_daterange)
PPC_steps <- data_PPC %>% 
  filter(id != "outside") %>% 
  mutate(date = round_date(date,"10 min")) %>% 
  group_by(id,date) %>%
  summarise(across(everything(),mean)) %>% 
  mutate(PPC_diff = abs(c(NA,diff(PPC5))),
         step = ifelse(PPC_diff > 0.1,1,0))

step_date <- unique(PPC_steps$date[PPC_steps$step == 1])
step_date <- step_date[c(as.numeric(diff(step_date)),100) > 60]
step_date <- step_date[!is.na(step_date)]
#PPC_stable <- lapply(step_date,function(x) sub_daterange(data_merge,c(x-2*3600,x)))




########
#plot ppc steps
ggplot(PPC_steps)+
  geom_line(aes(date,PPC_diff,col=as.factor(id)))+
  geom_point(aes(date,PPC_diff,col=as.factor(step)))


####################################
#merge comsol output with PPC
data_merge <- merge(comsol_old,data_PPC[data_PPC$id == 1,],all.x = T)


data_merge$step <- NA
for(i in seq_along(step_date)){
  step_i <- step_date[i]
  id <- daterange_id(data_merge,c(step_i-2*3600,step_i-600))
  data_merge$step[id] <- i
  
}


DSD0_plt <- ggplot(sub_daterange(comsol_old,PPC_daterange))+
  geom_line(aes(date,DSD0,col=factor(tiefe,levels=1:3,labels=c("0-10","10-20",">20"))))+
  xlim(PPC_daterange)+
  geom_point(data=subset(data_merge,!is.na(step)),aes(date,DSD0,col=factor(tiefe,levels=1:3,labels=c("0-10","10-20",">20"))))+
  labs(y=expression(D[S]/D[0]),col="tiefenstufe")
#########################
#final plots

#######################
#DSD0 PPC timeline
ggpubr::ggarrange(DSD0_plt+
                 geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
               PP_plot+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
               swc_plot,heights = c(2,1,1),ncol=1,align = "v")+
  ggsave(filename = paste0(plotpfad_PPchamber,"DSD0_PPC_Sand_timeline",i,".png"),width=8,height=7)


####################
#DSD0 PPC scatterplot
ggplot(subset(data_merge,step %in% 1:6))+
  geom_point(aes(PPC,DSD0,col=as.factor(tiefe)))+
  ggsave(filename = paste0(plotpfad_PPchamber,"DSD0_PPC_scatterplot",i,".png"),width=6,height=5)


########################
