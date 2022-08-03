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
Versuch <- 7
overwrite <-  F
load(file=paste0(datapfad_PP_Kammer,"data_tracer.RData"))
data_all <- data
#for(Versuch in na.omit(unique(data_all$Versuch))){

data <- data_all[data_all$Versuch == Versuch & !is.na(data_all$Versuch),]
data <- data %>% 
  group_by(tiefe) %>% 
  mutate(CO2_tracer_roll = RcppRoll::roll_mean(CO2_tracer_drift,5,fill=NA))

ggplot(data)+
  geom_line(aes(date,CO2_tracer_roll,col=as.factor(tiefe)))+
  geom_line(aes(date,CO2_tracer_drift,col=as.factor(tiefe)),alpha=0.3)
# 
# data$half_hour <- round_date(data$date,"30 mins")
# 
# mod_dates <- sort(unique(data$half_hour[data$inj == 1 & data$half_hour == data$date & !is.na(data$CO2_tracer_drift)]))
# 
# data_mod <- data %>%
#   mutate(date = round_date(date,"30 mins"))%>%
#   group_by(date,tiefe) %>%
#   summarise(across(everything(),mean))


#     
##########################################
## COMSOL
##########################################

file_i <- paste0(datapfad_PP_Kammer,"DSD0_comsol_,",Versuch,".RData")
if(file.exists(file_i) & overwrite == F){
load(file_i)
  }else{

comsol <- comsol_sweep(data = data,
                       tracer_colname = "CO2_tracer_roll",
                       intervall = "30 mins",
                       filename = "freeSoil_anisotropy_sweep_2DS.txt",
                       extend = T,
                       byout= 1e-7)

save(comsol,file=file_i)

}
#comsol_sweep <- comsol_sweep(data = data,intervall = "30 mins",filename = "freeSoil_anisotropy_sweep_2DS.txt",extend = T,byout= 1e-7)



# comsol <- run_comsol_nruns(data=data_mod,
#                            mod_dates = mod_dates,
#                            offset_method = "drift",
#                            overwrite = F,
#                            read_all = F,
#                            modelname = "Diffusion_freeSoil_anisotropy_optim_3DS_50runs_injection_rate",
#                            nruns=50,
#                            long=T)

# comsol_old<- run_comsol(data=data_mod,
#                         mod_dates = mod_dates,
#                         offset_method = "drift",
#                         overwrite = F,
#                         read_all = F,
#                         modelname = "Diffusion_freeSoil_anisotropy_optim_3DS",
#                         long=T)





###########
#comsol old and new function vergleich
#wrong <- comsol$date != comsol$mod_date



#  ggplot()+
#    geom_line(data=comsol,aes(date,DSD0,group=as.factor(tiefe),col=as.factor(tiefe)))
# # geom_line(data=comsol_old,aes(date,DSD0,group=as.factor(tiefe),col="old",linetype="old"))
  # geom_point(data=comsol[wrong,],aes(date,DSD0,group=as.factor(tiefe),col="wrong"))+
  # geom_point(data=comsol[!wrong,],aes(date,DSD0,group=as.factor(tiefe),col="right"))+
  #geom_line(data=comsol,aes(mod_date,DSD0,group=as.factor(tiefe),col=as.factor(tiefe),linetype="inter"))#+

# ggplot()+
#   geom_line(data=comsol,aes(mod_date,mod_inj_rate,group=as.factor(tiefe),col="inter",linetype="inter"))+
#   geom_line(data=subset(data_mod,date %in% mod_dates),aes(date,inj_mol_m2_s,group=as.factor(tiefe),col="old",linetype="old"))#+




###################################################################
#genauerer Blick auf erste PPC Periode

pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

pp_chamber_sub <- sub_daterange(pp_chamber[,c("Start","Ende","step_hours","start_offset","Modus")],range(comsol$date),"Start")

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

  PPC_daterange <-range(ymd_hms(t(pp_chamber_sub[,c("Start","Ende")])))+3600*10*c(-1,1)
#PPC_daterange <- ymd_h("2022.05.10 00","2022.05.11 15")

#####################
#CO2 inj un refadj plot
ggplot(sub_daterange(data,PPC_daterange))+
  geom_ribbon(aes(date,ymin=CO2_refadj,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.2)+
  geom_line(aes(date,CO2_refadj,col=as.factor(tiefe)))+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe)))

  



# ############
# #swc
# datelim <- range(mod_dates)
# 
# load(file = paste(datapfad_PP_Kammer,"klima_DWD.RData"))
# if(as.numeric(difftime(now(),max(klima$date),unit="hours")) > 24){
#   source("./PP_kammer/klima_dwd.R")
# }
# klima_sub <- sub_daterange(klima,datelim)
# load(file = paste(datapfad_PP_Kammer,"swc_long.RData"))
# swc_sub <- sub_daterange(swc_long,datelim)
# if(nrow(swc_sub) > 0){
#   sec_ax_fac <- 0.7
#   swc_min <- min(swc_sub$swc,na.rm = T)/1.1
#   swc_plot <- ggplot(swc_sub)+
#     geom_ribbon(data=klima_sub,aes(x=date,ymin=swc_min,ymax=P24tot/sec_ax_fac + swc_min),fill="blue",alpha=0.8)+
#     geom_line(aes(date,swc,col=as.factor(tiefe)))+
#     xlim(PPC_daterange)+
#     scale_y_continuous(sec.axis = sec_axis(~(. - swc_min)*sec_ax_fac,name=expression(italic(P)["24h"]*" (mm)")))+
#     theme(
#       axis.title.y.right = element_text(color = "blue"),
#       axis.text.y.right = element_text(color = "blue"),
#       #axis.text.x = element_blank()
#     )+
#     labs(x="",y="SWC (Vol. %)",col="tiefe (cm)")
# }

####################
#PP

##################
#Metadata

data_PPC <- read_PP(datelim = range(data$date))

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
    geom_line(aes(date,P_roll,col=id),alpha=0.5, linetype=2)+
    xlim(PPC_daterange)+
    labs(x="",y=expression(P["moving average"]))#+
    #ylim(c(-10,10))
}
data_PPC <- sub_daterange(data_PPC,PPC_daterange)
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
data_merge <- merge(comsol,data_PPC[data_PPC$id == 1,],all.x = T)
#data_merge <- merge(comsol,data_PPC[data_PPC$id == 1,],all.x = T)


data_merge$step <- NA
for(i in seq_along(step_date)){
  step_i <- step_date[i]
  id <- daterange_id(data_merge,c(step_i-2*3600,step_i-600))
  data_merge$step[id] <- i
  
}

titles <- c("2D PP - Über-Unterdruck",
            "2D PP",
            "1D PP",
            "2D PP and P mov.mean",
            "2D PP",
            "2d PP - 2D PP",
            "1D PP")

DSD0_plt <- ggplot(sub_daterange(comsol,PPC_daterange))+
#DSD0_plt <- ggplot(sub_daterange(comsol,PPC_daterange))+
  geom_line(aes(date,DSD0,col=factor(tiefe,levels=1:2,labels=c("0-20",">20"))))+
  #geom_line(aes(date,DSD0,col=factor(tiefe,levels=1:3,labels=c("0-10","10-20",">20"))))+
  xlim(PPC_daterange)+
  geom_point(data=subset(data_merge,!is.na(step)),aes(date,DSD0,col=factor(tiefe,levels=1:2,labels=c("0-20",">20"))))+
  #geom_point(data=subset(data_merge,!is.na(step)),aes(date,DSD0,col=factor(tiefe,levels=1:3,labels=c("0-10","10-20",">20"))))+
  labs(title=titles[Versuch],y=expression(D[S]/D[0]),col="tiefenstufe")

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

ggpubr::ggarrange(DSD0_plt+
                 geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
               PP_plot+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
               P_roll_plot+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
               #swc_plot,
               heights = c(2,1,1,1),ncol=1,align = "v")+
  ggsave(filename = paste0(plotpfad_PPchamber,"DSD0_PPC_Sand_timeline",Versuch,".png"),width=8,height=7)


####################
#DSD0 PPC scatterplot
ggplot(subset(data_merge,!is.na(step)))+
  geom_point(aes(PPC,DSD0,col=as.factor(tiefe)))+
  ggsave(filename = paste0(plotpfad_PPchamber,"DSD0_PPC_scatterplot",Versuch,".png"),width=6,height=5)


save(data_merge,file=paste0(datapfad_PP_Kammer,"data_merge_",Versuch,".RData"))

#}
