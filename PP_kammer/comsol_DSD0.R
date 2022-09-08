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
load(file = paste(datapfad_PP_Kammer,"injectionrates.RData"))
#######################################
#####################################
#read probes
Versuch <- 5
overwrite <-  F
plot <- T
load(file=paste0(datapfad_PP_Kammer,"data_tracer.RData"))
data_all <- data

for(Versuch in 1:8){

data <- data_all[data_all$Versuch == Versuch & !is.na(data_all$Versuch),]
data_uncert_sub <- data_uncert[data_uncert$Versuch == Versuch & !is.na(data_uncert$Versuch),] %>% 
  #filter(tiefe > -21) %>% 
  mutate(inj_meanr6 = RcppRoll::roll_meanr(inj_mol_m2_s,6*6,fill=NA),
         inj_meanr12 = RcppRoll::roll_meanr(inj_mol_m2_s,12*6,fill=NA),
         )
  

data <- data %>% 
  group_by(tiefe) %>% 
  #filter(tiefe > -21) %>% 
  mutate(CO2_tracer_roll = RcppRoll::roll_mean(CO2_tracer_drift,10,fill=NA),
         inj_meanr6 = RcppRoll::roll_meanr(inj_mol_m2_s,6*6,fill=NA),
         inj_meanr12 = RcppRoll::roll_meanr(inj_mol_m2_s,12*6,fill=NA),
         )

# ggplot(data)+
#   geom_line(aes(date,inj_mol_m2_s,col=""))+
#   geom_line(aes(date,inj_meanr12,col="meanr12"))+
#   geom_line(aes(date,inj_meanr6,col="meanr6"))

tracerplt <- ggplot(data)+
  #  geom_line(aes(date,CO2_tracer_roll,col=as.factor(tiefe)))+
  geom_line(aes(date,CO2_tracer_drift,col=as.factor(tiefe)))
 # ggplot(data_uncert_sub)+
 #   geom_ribbon(aes(date,ymax=CO2_tracer_max,ymin=ifelse(CO2_tracer_min > 0,CO2_tracer_min,0),fill=as.factor(tiefe)),alpha=0.2)+
 #  geom_line(aes(date,CO2_tracer_drift,col=as.factor(tiefe)))
#   
# 



#     
##########################################
## COMSOL
##########################################

nDS <- 2
file_suffix <- "_40cm"
#file_suffix <- ""
file_i <- paste0(datapfad_PP_Kammer,"DSD0_comsol_",nDS,"DS_",Versuch,file_suffix,".RData")
if(file.exists(file_i) & overwrite == F){
  load(file_i)
}else{
   data$inj_mol_m2_s <- data$inj_meanr6
   data_uncert_sub$inj_mol_m2_s <- data_uncert_sub$inj_meanr6

  comsol <- comsol_sweep(data = data,
                         tracer_colname = "CO2_tracer_drift",
                         intervall = "60 mins",
                         filename = paste0("freeSoil_anisotropy_sweep_",nDS,"DS.txt"),
                         extend = ifelse(nDS>1,T,F),
                         byout= 1e-7)
  comsol_min <- comsol_sweep(data = data_uncert_sub,
                             tracer_colname = "CO2_tracer_min",
                             intervall = "60 mins",
                             filename = paste0("freeSoil_anisotropy_sweep_",nDS,"DS.txt"),
                             extend = ifelse(nDS>1,T,F),
                             byout= 1e-7)
  comsol_max <- comsol_sweep(data = data_uncert_sub,
                             tracer_colname = "CO2_tracer_max",
                             intervall = "60 mins",
                             filename = paste0("freeSoil_anisotropy_sweep_",nDS,"DS.txt"),
                             extend = ifelse(nDS>1,T,F),
                             byout= 1e-7)

  save(comsol,comsol_min,comsol_max,file=file_i)
  
}

# comsol <- run_comsol_nruns(data=data_mod,
#                            mod_dates = mod_dates,
#                            offset_method = "drift",
#                            overwrite = F,
#                            read_all = F,
#                            modelname = "Diffusion_freeSoil_anisotropy_optim_3DS_50runs_injection_rate",
#                            nruns=50,
#                            long=T)

###########
#comsol old and new function vergleich
#wrong <- comsol$date != comsol$mod_date
range(subset(comsol,tiefe==1)$DS)

comsol$DSD0_min <- comsol_min$DSD0
comsol$DSD0_max <- comsol_max$DSD0
DSD0plt <- ggplot(comsol)+
  geom_ribbon(aes(x=date,ymin=DSD0_min,ymax=DSD0_max,fill=as.factor(tiefe)),alpha=0.3)+
  geom_line(aes(date,DSD0,col=as.factor(tiefe)))
  #  geom_line(aes(date,DSD0_min,col=as.factor(tiefe)),linetype=2)+
  #  geom_line(aes(date,DSD0_max,col=as.factor(tiefe)),linetype=2)+
#  geom_vline(xintercept = step_date,linetype=2,alpha=0.2)
#x_datelim <- ymd_h("2022-05-09 23","2022-05-11 15")
#egg::ggarrange(tracerplt+xlim(range(comsol$date)),DSD0plt)
#DSD0plt+ylim(c(0.1,0.3))



###################################################################
#genauerer Blick auf erste PPC Periode

pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

pp_chamber_sub <- sub_daterange(pp_chamber[,c("Start","Ende","step_hours","start_offset","Modus")],range(data$date),"Start")

step_date_ls <- list()
for(i in 1:nrow(pp_chamber_sub)){
  if(is.na(pp_chamber_sub$step_hours[i])){
    step_date_ls[[i]] <- c(pp_chamber_sub$Start[i],pp_chamber_sub$Ende[i])
  }else{
    step_date_ls[[i]] <- c(seq(pp_chamber_sub$Start[i],pp_chamber_sub$Ende[i],by=pp_chamber_sub$step_hours[i]*3600),pp_chamber_sub$Ende[i])
  }
  id_i <- c(1,length(step_date_ls[[i]]))
  step_date_ls[[i]][-id_i] <- step_date_ls[[i]][-id_i] - pp_chamber_sub$start_offset[i]*60
}
step_date <- do.call(c,step_date_ls)

#PP_plot+geom_vline(xintercept = step_date,linetype=2,alpha=0.2)

PPC_daterange_short <-range(ymd_hms(t(pp_chamber_sub[,c("Start","Ende")])))+3600*1*c(-1,1)
PPC_daterange <-range(ymd_hms(t(pp_chamber_sub[,c("Start","Ende")])))+3600*10*c(-1,1)
#PPC_daterange <- ymd_h("2022.05.10 00","2022.05.11 15")


###########################
#comsol optim old
##########################
# data$half_hour <- round_date(data$date,"30 mins")
# 
# 
# data_mod <- data %>%
#   mutate(date = round_date(date,"30 mins"))%>%
#   filter(date >= min(PPC_daterange) &
#            date <= max(PPC_daterange) &
#            inj == 1 &
#            !is.na(CO2_tracer_drift)) %>%
#   group_by(date,tiefe) %>%
#   summarise(across(everything(),mean))
# 
# mod_dates <- sort(unique(data_mod$date))
# 
# comsol_old<- run_comsol(data=data_mod,
#                         mod_dates = mod_dates,
#                         offset_method = "drift",
#                         overwrite = F,
#                         read_all = F,
#                         modelname = paste0("Diffusion_freeSoil_anisotropy_optim_",nDS,"DS"),
#                         n_DS=2,
#                         long=T)
# 
# 
# 
# comsol$date
# 
# ggplot(comsol_old)+
#   geom_line(aes(date,DSD0,col=factor(tiefe)),linetype=2)+
#   geom_line(data=comsol,aes(date,DSD0,col=factor(tiefe)))
# range_optim <- comsol_old %>%
#   group_by(tiefe) %>% 
#   summarise(min=min(DS),
#             max=max(DS)) %>% 
#   as.data.frame()
# range_sweep <- comsol %>%
#   group_by(tiefe) %>% 
#   summarise(min=min(DS),
#             max=max(DS)) %>% 
#   as.data.frame()
# range_sweep
# range_optim

#####################
#CO2 inj un refadj plot
ggplot(sub_daterange(data,PPC_daterange))+
  geom_ribbon(aes(date,ymin=CO2_refadj,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.2)+
  geom_line(aes(date,CO2_refadj,col=as.factor(tiefe)))+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))






####################
#PP

##################
#Metadata

data_PPC <- read_PP(datelim = range(data$date))
PPC_wide <- read_PP(datelim = PPC_daterange_short,format="wide")
PPC_wide <- PPC_wide %>% 
  mutate(dP_horiz_raw = (abs(P_1 - P_2) + abs(P_2 - P_3))/2,
         dP_horiz = RcppRoll::roll_mean(dP_horiz_raw,10*60,fill=NA))

if(Versuch == 2){
  data_PPC <- subset(data_PPC, id %in% c(1,4))
}
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
  
  data_PPC <- sub_daterange(data_PPC,PPC_daterange_short)
  
  
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
    geom_hline(yintercept = 0,col="grey",linetype=2)+
    geom_line(aes(date,P_roll,col=id),alpha=0.5, linetype=2)+
    xlim(PPC_daterange)+
    labs(x="",y=expression(P["moving average"]~"(Pa)"))#+
  #ylim(c(-10,10))
dP_horiz_plot <- ggplot(PPC_wide)+
    xlim(PPC_daterange)+
  geom_line(aes(date,dP_horiz))
}
#data_PPC <- sub_daterange(data_PPC,PPC_daterange)
# step_thr <- 0.05
# PPC_steps <- data_PPC %>% 
#   filter(id != "outside") %>% 
#   mutate(date = round_date(date,"10 min")) %>% 
#   group_by(id,date) %>%
#   summarise(across(everything(),mean)) %>% 
#   mutate(PPC_diff = abs(c(NA,diff(PPC5))),
#          step = ifelse(PPC_diff > step_thr,1,0))
# 

# step_date <- unique(PPC_steps$date[PPC_steps$step == 1])
# step_date <- step_date[c(as.numeric(diff(step_date)),100) > 60]
# step_date <- step_date[!is.na(step_date)]

#apply(pp_chamber_sub,1,function(x) seq(x[1],x[2],by=x[3]))

#PPC_stable <- lapply(step_date,function(x) sub_daterange(data_merge,c(x-2*3600,x)))

# ggpubr::ggarrange(
#   PP_plot+geom_vline(xintercept = step_date,linetype=2,alpha=0.2)
#   ,
#   P_roll_plot+geom_vline(xintercept = step_date,linetype=2,alpha=0.2)
#   ,ncol=1)



########
#plot ppc steps
# ggplot(PPC_steps)+
#   geom_line(aes(date,PPC_diff,col=as.factor(id)))+
#   geom_point(aes(date,PPC_diff,col=as.factor(step)))+
#   geom_hline(yintercept = step_thr)
# 

####################################
#merge comsol output with PPC
data_PPC_sub <- data_PPC %>% 
  filter(id == 1) %>% 
  #mutate(date = round_date(date,"10 mins")) %>% 
  select(-id) #%>% 
  #group_by(date) %>% 
  #summarise(across(everything(),mean))
data_merge <- merge(comsol,data_PPC_sub,all = T)
data_merge <- data_merge %>% 
  mutate(PPC5_int = imputeTS::na_interpolation(PPC5)) %>% 
  filter(!is.na(DSD0))
  

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
titles <- c("2D PP - Über-Unterdruck",
            "2D PP",
            "1D PP",
            "2D PP and P mov.mean",
            "2D PP",
            "2D PP - 2D PP",
            "1D PP",
            "1D PP - 2D PP")

DSD0_plt <- ggplot(sub_daterange(comsol,PPC_daterange))+
  #DSD0_plt <- ggplot(sub_daterange(comsol,PPC_daterange))+
  geom_ribbon(aes(x=date,ymin=DSD0_min,ymax=DSD0_max,fill=factor(tiefe,levels=1:2,labels=c("0-20",">20"))),alpha=0.2)+
  geom_line(aes(date,DSD0,col=factor(tiefe,levels=1:2,labels=c("0-20",">20"))))+
  #geom_line(aes(date,DSD0,col=factor(tiefe,levels=1:3,labels=c("0-10","10-20",">20"))))+
  xlim(PPC_daterange)+
  geom_point(data=subset(data_merge,!is.na(step)),aes(date,DSD0,col=factor(tiefe,levels=1:2,labels=c("0-20",">20"))))+
  #geom_point(data=subset(data_merge,!is.na(step)),aes(date,DSD0,col=factor(tiefe,levels=1:3,labels=c("0-10","10-20",">20"))))+
  labs(y=expression(D[S]/D[0]),col="tiefenstufe",fill="tiefenstufe")

# tracer_plt <- ggplot(sub_daterange(data,PPC_daterange))+
# #DSD0_plt <- ggplot(sub_daterange(comsol,PPC_daterange))+
#   geom_line(aes(date,CO2_tracer_drift,col=factor(-tiefe)))+
#   xlim(PPC_daterange)+
#   labs(y=expression(CO[2]~"(ppm)"),col="tiefe")
#########################
#final plots

#######################
#DSD0 PPC timeline
# ggpubr::ggarrange(tracer_plt+
#                     geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
#                   PP_plot+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
#                   P_roll_plot+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
#                   #swc_plot,
#                   heights = c(2,1,1,1),ncol=1,align = "v")

ggpubr::ggarrange(tracerplt+labs(title=titles[Versuch])+
                    xlim(PPC_daterange)+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
                  DSD0_plt+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
                  PP_plot+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
                  P_roll_plot+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
                  #swc_plot,
                  heights = c(2,2,1,1),ncol=1,align = "v")+
  ggsave(filename = paste0(plotpfad_PPchamber,"DSD0_PPC_Sand_timeline_",nDS,"DS_",Versuch,file_suffix,".png"),width=8,height=7)


####################
#DSD0 PPC scatterplot
# ggplot(subset(data_merge,!is.na(step)))+
#   geom_point(aes(PPC,DSD0,col=as.factor(tiefe)))+
#   ggsave(filename = paste0(plotpfad_PPchamber,"DSD0_PPC_scatterplot",Versuch,".png"),width=6,height=5)

}
#save(data_merge,file=paste0(datapfad_PP_Kammer,"data_merge_",Versuch,".RData"))
save(data_merge,step_date,file=paste0(datapfad_PP_Kammer,"data_merge_",nDS,"DS_",Versuch,file_suffix,".RData"))
}

#}
