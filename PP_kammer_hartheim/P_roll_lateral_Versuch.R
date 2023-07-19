hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 


klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
klimapfad_CR1000<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/Hartheim CR1000/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

chamber_arduino_pfad <- paste0(hauptpfad,"/Daten/Urdaten/Kammermessungen_Arduino/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)
theme_set(theme_classic())

plot <- F

##################
#Metadata
pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen_hartheim.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

injections <- read_ods(paste0(metapfad_PP,"injektionen_hartheim.ods"))
injections$Start <- dmy_hm(injections$Start)
injections$Ende <- dmy_hm(injections$Ende)


Versuch <- 20
for(Versuch in c(10,20,21,22)){
if(Versuch == 20){
  datelim <- c(pp_chamber$Start[Versuch],pp_chamber$Ende[Versuch]+3600*24*0.5)
}else{
  datelim <- c(pp_chamber$Start[Versuch]-3600 * 6,pp_chamber$Ende[Versuch]+3600*24*0.5)
}

if(Versuch == 21){
  datelim[2] <- ymd_h("23.01.16 08")
}
###################
#PPC data

data_PPC <- read_PP(datelim = datelim,table="PP_1min",corfac = "P_corfac_date")

# datelim <- ymd_hms("2022-12-07 00:00:00 UTC", "2022-12-13 12:00:00 UTC")
# data_PPC <- read_PP(datelim = datelim,corfac = "P_corfac_date")
# 
# load(paste0(klimapfad_CR1000,"klima_data_PP_kammer.RData"))
# ggplot(sub_daterange(data_PPC,ymd_hm("2022-12-07 00:00","2022-12-09 15:00")))+geom_line(aes(date,PPC,col=factor(id)))
# 
# ggplot(sub_daterange(data_PPC,ymd_hm("2022-12-09 09:00","2022-12-09 09:02")))+geom_line(aes(date,P,col=factor(id)))
# ggplot(sub_daterange(data_PPC,ymd_hm("2022-12-09 11:00","2022-12-09 11:02")))+geom_line(aes(date,P,col=factor(id)))

#data_PPC <- subset(data_PPC,id %in% c(1:6))
dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T),2)

data_PPC <- data_PPC %>% 
  group_by(id) %>%
  mutate(dt = diff_time(date,"secs"),
         PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
         P_roll = RcppRoll::roll_mean(P,20,fill=NA))


P_ref <- subset(data_PPC,id==5)
P_ref$P_ref <- P_ref$P_roll
data_PPC <- subset(data_PPC,id!=5)
data_PPC <- merge(data_PPC,P_ref[,c("date","P_ref")])
data_PPC$P_roll <- data_PPC$P_roll - data_PPC$P_ref

data_PPC[which(data_PPC$dt > 3600),c("PPC","PPC5","P_roll")] <- NA


step_thr <- ifelse(Versuch == 20,0.27,0.4)

data_PPC <- data_PPC[order(data_PPC$date),]
PPC_steps <- data_PPC %>%
  filter(id %in% 1:4) %>%
  mutate(date = round_date(date,"10 min")) %>%
  group_by(id,date) %>%
  summarise(across(everything(),mean)) %>%
  mutate(P_diff = abs(c(NA,diff(P))),
         step = ifelse(P_diff > step_thr,1,0),
  )

step_date <- sort(unique(PPC_steps$date[PPC_steps$step == 1]))
step_date <- step_date[c(T,c(as.numeric(diff(step_date))) > 60*3)]
step_date <- step_date[!is.na(step_date)]

n_versuche <- 3
n_steps <- 4
step_h <- 8 * 3600
break_h <- 10 * 3600
hours_vec <- cumsum(c(rep(c(rep(step_h,n_steps),break_h),n_versuche-1),rep(step_h,n_steps)))
if(Versuch == 21){
  step_date <- c(step_date[1],step_date[1]+hours_vec+3600)
}
if(Versuch == 22){
  step_date <- c(step_date[1],step_date[1]+hours_vec)
}
#step_date - step_date_2
ggplot(PPC_steps)+
  geom_vline(xintercept = step_date,col="grey",linetype=2)+
  geom_hline(yintercept = step_thr,col="grey",linetype=2)+
  geom_line(aes(date,P,col=factor(id)))+
  #  geom_point(aes(date,P,col=factor(id)))+
  geom_line(aes(date,P_diff))#+
#  xlim(ymd_h("22.12.09 03","22.12.09 23"))
#  facet_wrap(~id)

PPC_steps_wide <- tidyr::pivot_wider(PPC_steps,id_cols = date,names_from = id,values_from = P_roll,names_prefix = "P_roll_")

step_df <- PPC_steps_wide %>% 
  mutate(step = ifelse(date %in% !!step_date,1,0),
         step_id=cumsum(step)) %>% 
  group_by(step_id) %>% 
  summarise(across(matches("P_roll"),mean,na.rm=T),
            Start = min(date),
            End = max(date))



if(Versuch %in% 20:21){
  modes <- c(rep(c("0,0","1,1","1,-1","-1,1","-1,-1"),3),"0,0")
}
if(Versuch %in% 22){
  modes <- c(rep(c("0,0","1,1","1,-1","-1,-1","-1,1"),3),"0,0")
}
if(Versuch %in% 24){
  modes <- c(rep(c("0,0","100","70","1","-1","-70","-100"),2),"0,0")
}
if(Versuch %in% 25){
  modes <- c(rep(c("0,0","70,70","70,1","1,70","-40,1","1,-40","-40,-40"),2),"0,0")
}
if(Versuch %in% 26){
  modes <- c("0,0",c(t(cbind(c("-100","-70","-1","30","70"),"0"))),"100","0,0")
}
if(Versuch %in% 27){
  modes <- c("0,0",c(t(cbind(c("-100","100","-70","70","-1"),"0"))),"30","0,0")
}
if(Versuch == 10){
  modes <- c("0,0","40","20","-20","-40","0,0")
}
step_df$mode <- modes
step_df$step <- factor(step_df$mode,
                       levels = unique(step_df$mode),
                       labels = seq_along(unique(step_df$mode))-1)

data_PPC$step_id <- NA
data_PPC$mode <- NA
for(i in 1:nrow(step_df)){
  id <- which(daterange_id(data_PPC,c(step_df$Start[i]+3600*3,step_df$End[i]-1800)))
  data_PPC$mode[id] <- step_df$mode[i]
  data_PPC$step_id[id] <- step_df$step_id[i]
}




cal_period <- data_PPC %>% 
  filter(mode == "0,0") %>% 
  filter(!is.na(P_roll)) %>% 
  group_by(id) %>% 
  mutate(zeit = as.numeric(difftime(date,min(date))),
         sensor_drift = predict(glm(P_roll ~ zeit)),
         P_roll_cal = P_roll - sensor_drift)

data_PPC <- merge(data_PPC,cal_period[,c("date","sensor_drift","id")],all = T)
data_PPC <- data_PPC %>% 
  group_by(id) %>% 
  mutate(sensor_drift = imputeTS::na_interpolation(sensor_drift),
         P_roll_cal = P_roll - sensor_drift,
         P_tot = RcppRoll::roll_sumr(P_roll_cal,24*60))

ggplot(subset(data_PPC,id != 6))+
  geom_line(aes(date,P_roll,col=id))+
  geom_line(aes(date,sensor_drift,col=id))
ggplot(subset(data_PPC,id != 6))+
  geom_line(aes(date,P_roll_cal,col=id))

# ggplot(subset(data_PPC,id != 6))+
#   geom_line(aes(date,P_roll_cal,col=id))+
#   geom_line(aes(date,P_tot,col=id))
# 

############
#probe 1 u 2
data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")

data_probe1u2 <- data_probe1u2 %>% 
  group_by(tiefe) %>% 
  mutate(CO2_smp1_roll = RcppRoll::roll_mean(CO2_smp1,5,fill=NA),
         CO2_smp2_roll = RcppRoll::roll_mean(CO2_smp2,5,fill=NA)
  )

###########
#CO2atm

chamber_data <- read_arduino_chamber(datelim=datelim)


chamber_data$atm <- ifelse(minute(chamber_data$date) %% 30 > 5 & minute(chamber_data$date) %% 30 < 29,1,0)
chamber_data$date
CO2_atm <- chamber_data %>% 
  filter(atm == 1) %>% 
  select(date,CO2) %>% 
  mutate(date = round_date(date,"10 mins")) %>% 
  group_by(date) %>% 
  summarise(CO2_atm = mean(CO2,na.rm=T)) %>% 
  mutate(CO2_atm = imputeTS::na_interpolation(CO2_atm))

#############
#CO2 flux
if(Versuch %in% c(25,27)){
  load(paste0(datapfad_PP_Kammer,"flux_ls.RData"))
  flux <- sub_daterange(flux,datelim)
  
  flux$CO2_flux_raw <- flux$CO2_GGA_mumol_per_s_m2
  flux$CO2_flux <- RcppRoll::roll_mean(flux$CO2_GGA_mumol_per_s_m2,5,fill=NA)
  
  flux$CH4_flux_raw <- flux$CH4_mumol_per_s_m2 * 10^3
  flux$CH4_flux <- RcppRoll::roll_mean(flux$CH4_mumol_per_s_m2 * 10^3,5,fill=NA)
}

#########
#swc
source("./PP_kammer_hartheim/SWC_hartheim.R")

swc_sub <- sub_daterange(swc_wide,datelim)
########################


# data_PPC_wide <- tidyr::pivot_wider(data_PPC[,c("date","id","P_roll_cal","mode","step_id")],names_from = id,values_from =P_roll_cal,names_prefix = "P_")
# 
data_PPC_wide <- tidyr::pivot_wider(data_PPC[,c("date","id","P_roll_cal","PPC5","mode","step_id")],names_from = id,values_from =c(P_roll_cal,PPC5))
names(data_PPC_wide) <- str_replace_all(names(data_PPC_wide),c("P_roll_cal"="P","PPC5"="PPC"))


data_probes_wide <- tidyr::pivot_wider(data_probe1u2,id_cols=c(date,T_C),names_from = tiefenstufe,values_from = matches("CO2_smp"))

range(data_probes_wide$date)

data <- merge(data_PPC_wide,data_probes_wide)
data <- merge(data,swc_sub,all.x = T)
data <- merge(data,CO2_atm)
data <- data %>% mutate(zeit = as.numeric(difftime(date,min(date))))
if(Versuch %in% c(20,22,24,25,26)){
  data[,paste0("CO2_smp1_roll_",1:2)] <- NA
}
data_long <- tidyr::pivot_longer(data,matches("CO2_smp\\d_roll_\\d"),names_pattern = "CO2_smp(\\d)_roll_(\\d)",values_to = "CO2",names_to = c("probe","tiefe"))
data_long$smp_depth <- paste(data_long$probe,data_long$tiefe,sep="_")
data_long$CO2_preds <- NA 

for(i in unique(data_long$smp_depth)){
  cal_df <- subset(data_long, smp_depth == i & mode == "0,0" & !is.na(CO2))
  cal_datelim <- range(cal_df$date)
  if(any(!is.na(cal_df$CO2))){
    if(Versuch == 10){
      fm <- glm(CO2 ~poly(zeit,2),data = cal_df)
    }else{
      fm <- mgcv::gam(CO2 ~ s(zeit),data = cal_df)
    }
    tiefenID <- which(data_long$smp_depth == i & daterange_id(data_long,cal_datelim))
    data_long$CO2_preds[tiefenID] <- predict(fm,newdata = data_long[tiefenID,])
  }
}
data_long$CO2_offset <- data_long$CO2 - data_long$CO2_preds

data_long$P_horiz <- data_long$P_2 - data_long$P_3

names(data_long)

step_df_cal <- data_long %>% 
  filter(!is.na(step_id)) %>% 
  group_by(step_id,mode) %>% 
  summarise(across(matches("^P_"),mean,na.rm=T)) %>%
  ungroup() %>% 
  mutate(Start = step_df$Start,
         End = step_df$End)
data_long <- data_long %>% 
  group_by(step_id,tiefe,probe) %>% 
  mutate(dummy = 1,
         mode_zeit = cumsum(dummy))

# ggplot(data_long)+
#   geom_line(aes(date,P_1,col=factor(step_id),group=smp_depth))
# ggplot(data_long)+
#   geom_line(aes(date,mode_zeit,col=factor(step_id)))+
#   geom_hline(yintercept = 9)

data_long$CO2_cal <- ifelse(data_long$mode == "0,0",NA,data_long$CO2)
data_long$CO2_shift <-  data_long$CO2_offset / data_long$CO2_preds * 100
data_long$profile <- factor(data_long$probe,levels = 2:1,labels = 1:2)
data_long$subspace <- factor(data_long$probe,levels = 2:1,labels = 2:3)
data_long$depth <- factor(as.numeric(data_long$tiefe) * 3.5)

if(Versuch %in% 20:22){
  step_df_cal$P_alpha <- step_df_cal$P_horiz
}else{
  step_df_cal$P_alpha <- step_df_cal$P_3
  
}
# ggplot(data_long)+
#   geom_vline(xintercept = step_date,col="grey",linetype=2)+
#   geom_line(aes(date,CO2_cal,col=tiefe))+
#   geom_line(aes(date,CO2,col=tiefe),linetype = 3)+
#   geom_line(aes(date,CO2_preds,col=tiefe),linetype=2)+
#   guides(alpha=F)+
#   facet_wrap(~probe,ncol=1)+
#   ggsave(paste0(plotpfad_PPchamber,"CO2_offset_fm_",Versuch,".png"),width = 7,height = 6)
# step_df
# if(Versuch == 20){
#   step_df$step <- factor(step_df$step,labels = c("0","high P","P-"))
# }

if(plot){
  cols <- RColorBrewer::brewer.pal(4,"PuOr")
  PPC_col <- "brown"
  names(data_long)
  CO2_offset_plot <- 
    ggplot(data_long)+
    geom_rect(data=subset(step_df,step != 0),aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=step))+
    scale_alpha_discrete(range = c(0.1,0.4),guide = guide_legend(order = 1))+
    geom_line(data = subset(data_long, tiefe %in% 1:4),aes(date,CO2_cal,col=factor(tiefe,labels = c(1:4))))+
    scale_color_manual("subspace",values = c(cols),guide = guide_legend(order = 3))+
    ggnewscale::new_scale_color()+
    # geom_line(data = subset(data_long, tiefe %in% 1),aes(date,CO2_cal,col=factor(tiefe,labels = "")))+
    # scale_color_manual("ambient PPC",values = PPC_col,guide = guide_legend(order = 4))+
    # ggnewscale::new_scale_color()+
    geom_line(data = subset(data_long, tiefe %in% 1),aes(date,CO2_cal,col=factor(tiefe,labels = "")))+
    scale_color_manual(expression(P[lateral]),values = 1,guide = guide_legend(order = 5))+
    ggnewscale::new_scale_color()+
    #guides(alpha = F)+
    geom_vline(xintercept = step_date,col="grey",linetype=2)+
    geom_line(aes(date,CO2_cal,col=depth))+
    geom_point(data = subset(data_long,mode == "0,0"),aes(date,CO2,col=depth),pch=20)+
    geom_line(aes(date,CO2,col=depth),alpha = 0.5)+
    geom_line(aes(date,CO2_preds,col=depth),linetype=2)+
    scale_color_discrete(guide = guide_legend(order = 2))+
    labs(y = expression(CO[2]~"(ppm)"),col = "depth (cm)")+
    facet_wrap(~paste0("profile ",profile," (subspace ",subspace,")"),ncol=1)
  
  CO2_plot <- 
    ggplot(data_long)+
    #geom_rect(data=step_df_cal,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=abs(P_alpha)))+
    geom_rect(data=subset(step_df,step != 0),aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=step))+
    scale_alpha_discrete(range = c(0.1,0.4))+
    #scale_alpha(range = c(0,0.4))+
    geom_hline(yintercept = 0,col="grey",linetype=2)+
    geom_line(aes(date,CO2_shift,col=tiefe))+
    geom_vline(xintercept = step_date,col="grey",linetype=2)+
    facet_wrap(~paste0("profile ",profile," (subspace ",subspace,")"),ncol=1)+
    guides(col= F)+
    labs(y = expression(CO[2]*"-shift"~("%")), x ="",col="depth")
  #labs(y = expression(CO[2~offset]~(ppm)), x ="")
  
  
  
  #P_tot_plt <- 
  ggplot(data_long)+
    geom_rect(data=subset(step_df,step != 0),aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=step))+
    scale_alpha_discrete(range = c(0.1,0.4))+
    guides(alpha=F)+
    geom_vline(xintercept = step_date,col="grey",linetype=2)+
    geom_line(aes(date,-(cumsum(P_1)/4 + P_1 *50)+3200,col="1"))+
    #geom_line(aes(date,-(cumsum(P_1)+P_1*500),col="2"))+
    geom_line(data = subset(data_long,tiefe==7& probe == 1),aes(date,CO2,col="3"))+
    scale_color_manual(values = c(cols,1))+
    labs(col="subspace",y = expression(P[roll]~"(Pa)"))
  
  #ggpubr::ggarrange(CO2_offset_plot,P_tot_plt,common.legend = T,legend="right",ncol=1,align="v")
  
  P_plt <- ggplot(data_long)+
    geom_rect(data=subset(step_df,step != 0),aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=step))+
    scale_alpha_discrete(range = c(0.1,0.4))+
    guides(alpha=F)+
    geom_line(aes(date,P_horiz,col="lateral"))+
    geom_hline(yintercept = 0,col="grey",linetype=2)+
    geom_vline(xintercept = step_date,col="grey",linetype=2)+
    geom_line(aes(date,P_1,col="1"))+
    geom_line(aes(date,P_2,col="2"))+
    geom_line(aes(date,P_3,col="3"))+
    geom_line(aes(date,P_4,col="4"))+
    scale_color_manual(values = c(cols,1))+
    labs(col="subspace",y = expression(P[mean]~"(Pa)"))
  names(data_long)
  PPC_plt <- 
    ggplot(data_long)+
    geom_rect(data=subset(step_df,step != 0),aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=step))+
    scale_alpha_discrete(range = c(0,0.4))+
    guides(alpha=F)+
    geom_line(aes(date,PPC_1,col="1"))+
    geom_line(aes(date,PPC_2,col="2"))+
    geom_vline(xintercept = step_date,col="grey",linetype=2)+
    geom_line(aes(date,PPC_3,col="3"))+
    geom_line(aes(date,PPC_4,col="4"))+
    #geom_line(aes(date,PPC_6,col="ambient PPC"))+
    scale_color_manual(values = c(cols,PPC_col))+
    theme(legend.text.align = 0.5)+
    labs(col="subspace",y = "PPC (Pa/s)", x ="")
  
  if(exists("flux")){
    FCO2_plt <- ggplot(flux)+
      geom_rect(data=subset(step_df,step != 0),aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=step))+
      scale_alpha_discrete(range = c(0.1,0.4))+
      guides(alpha=F)+
      geom_vline(xintercept = step_date,col="grey",linetype=2)+
      geom_line(aes(date,CO2_flux_raw),alpha=.5)+
      geom_line(aes(date,CO2_flux))+
      xlim(range(data_long$date))+
      labs(y = expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"))
    
    FCH4_plt <- ggplot(flux)+
      geom_rect(data=subset(step_df,step != 0),aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=step))+
      scale_alpha_discrete(range = c(0.1,0.4))+
      guides(alpha=F)+
      geom_vline(xintercept = step_date,col="grey",linetype=2)+
      geom_line(aes(date,CH4_flux_raw),alpha=.5)+
      geom_line(aes(date,CH4_flux))+
      xlim(range(data_long$date))+
      labs(y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"))
    
    T_plt <- ggplot(flux)+
      geom_rect(data=subset(step_df,step != 0),aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=step))+
      scale_alpha_discrete(range = c(0.1,0.4))+
      guides(alpha=F)+
      geom_vline(xintercept = step_date,col="grey",linetype=2)+
      geom_line(aes(date,T_C))+
      xlim(range(data_long$date))
    
    ggpubr::ggarrange(CO2_offset_plot+
                        labs(title = paste("Versuch",paste0(Versuch,":"),pp_chamber$Modus[Versuch]))+
                        theme(axis.title.x = element_blank(),axis.text.x = element_blank()),
                      #CO2_plot+theme(axis.title.x = element_blank(),
                      #               axis.text.x = element_blank()),
                      FCO2_plt+theme(axis.title.x = element_blank(),
                                     axis.text.x = element_blank()),
                      FCH4_plt+theme(axis.title.x = element_blank(),
                                     axis.text.x = element_blank()),
                      P_plt+theme(axis.title.x = element_blank(),
                                  axis.text.x = element_blank()),
                      T_plt,ncol=1,align = "v",heights = c(3,1,1,1,1),common.legend = T,legend = "right")+
      ggsave(paste0(plotpfad_PPchamber,"CO2_offset_flux_",Versuch,".png"),width = 7,height = 10)
    
  }
  ggpubr::ggarrange(CO2_offset_plot+
                      labs(title = paste("Versuch",paste0(Versuch,":"),pp_chamber$Modus[Versuch]))+
                      theme(axis.title.x = element_blank(),axis.text.x = element_blank()),
                    CO2_plot+theme(axis.title.x = element_blank(),
                                   axis.text.x = element_blank()),
                    PPC_plt+theme(axis.title.x = element_blank(),
                                  axis.text.x = element_blank()),
                    P_plt,ncol=1,align = "v",heights = c(3,2,.8,1),common.legend = T,legend = "right")+
    ggsave(paste0(plotpfad_PPchamber,"CO2_offset_",Versuch,".png"),width = 7,height = 7)
  
  if(Versuch == 20){
    Sys.setlocale("LC_ALL", "English")
    ggpubr::ggarrange(CO2_offset_plot+
                        scale_alpha_discrete("mode",range = c(0.1,0.4),labels = 
                                               expression("1: high P",
                                                          "2: P"[lateral],
                                                          "3: P"[lateral],
                                                          "4: low P"),guide = guide_legend(order = 1))+
                        theme(legend.text.align = 0,axis.title.x = element_blank(),axis.text.x = element_blank()),
                      CO2_plot+theme(axis.title.x = element_blank(),
                                     axis.text.x = element_blank()),
                      #PPC_plt+theme(axis.title.x = element_blank(),
                      #              axis.text.x = element_blank()),
                      P_plt,ncol=1,align = "v",heights = c(3,2,1),common.legend = T,legend = "right")+
      ggsave(paste0(plotpfad_PPchamber,"Figure_4.png"),width = 7,height = 7)
    Sys.setlocale("LC_ALL", "")
    
  }
  ggpubr::ggarrange(CO2_plot+labs(title = paste("Versuch",paste0(Versuch,":"),pp_chamber$Modus[Versuch])),P_plt,ncol=1,align = "v",heights = c(2,1))+
    ggsave(paste0(plotpfad_PPchamber,"CO2_offset_PPC_",Versuch,".png"),width = 7,height = 6)
  
  
}
data_long$Versuch <- Versuch

save(data_long,file = paste0(datapfad_PP_Kammer,"CO2_offset_",Versuch,".RData"))
}





ggpubr::ggarrange(CO2_offset_plot+
                    theme(axis.title.x = element_blank(),axis.text.x = element_blank()),
                  P_plt,ncol=1,align = "v",heights = c(3,1),common.legend = T,legend = "right")+
  ggsave(paste0(plotpfad_PPchamber,"CO2_offset_flux_Werkst1.png"),width = 7,height = 6)
ggpubr::ggarrange(CO2_plot+theme(axis.title.x = element_blank(),
                                 axis.text.x = element_blank()),
                  P_plt,ncol=1,align = "v",heights = c(3,1),common.legend = T,legend = "right")+
  ggsave(paste0(plotpfad_PPchamber,"CO2_offset_flux_Werkst2.png"),width = 7,height = 6)
########################################
########################################
########################################
# ggplot(subset(data_long,mode_zeit > 9))+
#   geom_point(aes(P_3,CO2_offset,col=factor(tiefe)))+
#   geom_smooth(aes(P_3,CO2_offset,col=factor(tiefe)),method = "glm")+
#   facet_grid(~probe)
# ggplot(subset(data_long,mode_zeit > 9))+
#   geom_point(aes(P_horiz,CO2_offset,col=factor(tiefe)))+
#   geom_smooth(aes(P_horiz,CO2_offset,col=factor(tiefe)),method = "glm")+
#   facet_grid(~probe)
# 
# 
# ggplot(data)+
#   geom_point(aes(P_3,CO2_smp2_6,col=P_2))
# ggplot(data)+
#   geom_point(aes(P_3,CO2_smp1_6,col=P_2))
# probe1_plot <- 
#   ggplot(subset(data_probe1u2,tiefenstufe %in% c(3:7)))+
#   geom_vline(xintercept = step_date,col="grey",linetype=2)+
#   geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,fill=mode),alpha=0.4)+
#   geom_line(aes(date, CO2_smp1_roll,col=factor(abs(tiefe))))+
#   labs(col="tiefe (cm)",y = "CO2 profile 1 (ppm)",x="")+
#   scale_color_manual(values = scales::hue_pal()(8),limits = factor(0:7 * 3.5))+
#   theme(axis.title.y = element_text(colour = "blue"))+
#   scale_fill_manual(values = c(0,scales::hue_pal()(4)),limits = unique(modes))
# 
# probe2_plot <- ggplot(subset(data_probe1u2,tiefenstufe %in% c(1:7)))+
#   geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,fill=mode),alpha=0.4)+
#   geom_vline(xintercept = step_date,col="grey",linetype=2)+
#   geom_line(aes(date, CO2_smp2_roll,col=factor(abs(tiefe))))+
#   scale_color_manual(values = scales::hue_pal()(8),limits = factor(0:7 * 3.5))+
#   geom_line(data = data,aes(date, CO2_atm,col=factor(0)))+
#   
#   labs(y = "CO2 profile 2 (ppm)",x="")+
#   scale_fill_manual(values = c(0,scales::hue_pal()(4)),limits = unique(modes))
# 
# P_roll_plot <- ggplot(subset(data_PPC,id%in%1:4))+
#   geom_vline(xintercept = step_date,col="grey",linetype=2)+
#   geom_hline(yintercept = 0,col="grey",linetype=2)+
#   geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,fill=mode),alpha=0.4)+
#   geom_line(aes(date, P_roll_cal,col=factor(id),group=id),alpha=0.8)+
#   labs(y = expression(P[rollmean]~"(Pa)"))+
#   scale_color_manual(values = c("black","black","blue","blue"))+
#   scale_fill_manual(values = c(0,scales::hue_pal()(4)),limits = unique(modes))
# 
# ggpubr::ggarrange(probe1_plot,probe2_plot,P_roll_plot,ncol=1,align = "v",common.legend = T,legend = "right")+
#   ggsave(paste0(plotpfad_PPchamber,"P_roll_lateral_Versuch.png"),width = 7,height = 6)

