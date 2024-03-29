hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 


kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

chamber_arduino_pfad <- paste0(hauptpfad,"/Daten/Urdaten/Kammermessungen_Arduino/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)
theme_set(theme_classic())


##################
#Metadata
pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

injections <- read_ods(paste0(metapfad_PP,"injektionen.ods"))
injections$Start <- dmy_hm(injections$Start)
injections$Ende <- dmy_hm(injections$Ende)

PP_Versuche <- c(16,18,19,20,23,26,29,30,31)

Versuch <- 20
#for(Versuch in PP_Versuche){
  datelim <- c(pp_chamber$Start[Versuch]-3600 * 12,pp_chamber$Ende[Versuch]+3600*12)
  
  if(Versuch == 26){
    #datelim[1] <- ymd_h("2022-07-26 06")
    datelim[1] <- ymd_h("2022-07-26 00")
    datelim[2] <- ymd_hm("2022-07-27 1200")
  }
  
  if(Versuch == 31){
    datelim[1] <- ymd_hm("2022-08-11 0910")
    datelim[2] <- ymd_hm("2022-08-12 1400")
  }
  # if(Versuch == 30){
  #   datelim[1] <- ymd_h("2022-08-10 14")
  #   datelim[2] <- ymd_hm("2022-08-12 1400")
  # }
  # ###################
  #PPC data
  
  data_PPC <- read_PP(datelim = datelim,table="PP_1min",corfac = T)
  if(Versuch == 29){
    data_PPC <- subset(data_PPC, date < ymd_h("22.08.03 11"))
  }
  range(data_PPC$date)
  if (Versuch == 23){
    datelim <- range(data_PPC$date)
  }
  data_PPC <- subset(data_PPC,id %in% c(1:5))
  
  data_PPC <- data_PPC %>% 
    group_by(id) %>%
    mutate(dt = diff_time(date,"secs"),
           PPC5 = RcppRoll::roll_mean(P_diff,10,fill=NA),
           P_roll = RcppRoll::roll_mean(P,3,fill=NA))
  
  ggplot(data_PPC)+
    geom_line(aes(date, PPC5, col=factor(id)))+
    geom_line(aes(date, P_roll, col=factor(id)))
  
  data_PPC[which(data_PPC$dt > 3600),c("PPC","PPC5","P_roll")] <- NA
  
  
  # data_Proll_wide <- tidyr::pivot_wider(data_PPC,date,values_from = P_roll,names_from = id,names_prefix = "id_")
  # data_Proll_wide[paste0("id_",1:4)] <- data_Proll_wide[paste0("id_",1:4)] - data_Proll_wide$id_5
  # data_Proll <- tidyr::pivot_longer(data_Proll_wide[,1:6],paste0("id_",1:5),names_prefix = "id_",names_to = "id",values_to="P_roll")
  
  step_thr <- 0.05
  data_PPC <- data_PPC[order(data_PPC$date),]
  PPC_steps <- data_PPC %>%
    filter(id %in% 1:4) %>%
    mutate(date = round_date(date,"10 min")) %>%
    group_by(id,date) %>%
    summarise(across(everything(),mean)) %>%
    mutate(PPC_diff = abs(c(NA,diff(PPC5))),
           step = ifelse(PPC_diff > step_thr,1,0)
    )
  
  step_date <- sort(unique(PPC_steps$date[PPC_steps$step == 1]))
  step_date <- step_date[c(as.numeric(diff(step_date)),100) > 60]
  step_date <- step_date[!is.na(step_date)]

  cal_thr <- 0.1
  
  PPC_steps_wide <- tidyr::pivot_wider(PPC_steps,id_cols = date,names_from = id,values_from = P_roll,names_prefix = "P_roll_")
  names(data_PPC)
  step_df <- data_PPC %>%
    group_by(id) %>% 
    mutate(step = ifelse(date %in% !!step_date,1,0),
           step_id=cumsum(step)) %>% 
    group_by(step_id) %>% 
    summarise(across(PPC,mean,na.rm=T),
              Start = min(date),
              End = max(date)-1200) %>% 
    mutate(cal = ifelse(PPC > cal_thr,0,1))
  
  ggplot(PPC_steps)+
    geom_vline(xintercept = step_date,col="grey",linetype=2)+
    geom_hline(yintercept = step_thr,col="grey",linetype=2)+
    geom_hline(yintercept = cal_thr,col="green",linetype=2)+
    geom_line(aes(date,PPC_diff))+
    geom_line(aes(date,PPC5/2,col=factor(id)))

  
  data_PPC$step_id <- NA
  
    id <- which(daterange_id(data_PPC,c(step_df$Start[1],step_df$End[1])))
    data_PPC$step_id[id] <- step_df$step_id[i]
  
  for(i in 2:nrow(step_df)){
    id <- which(daterange_id(data_PPC,c(step_df$Start[i]+3600,step_df$End[i])))
    data_PPC$step_id[id] <- step_df$step_id[i]
  }
  # ggplot(data_PPC)+
  #   geom_line(aes(date,PPC,col=factor(step_id),group = id))
  
  data_PPC$cal <- factor(data_PPC$step_id,levels = step_df$step_id,labels = step_df$cal)
  
  
  ggplot(data_PPC)+
    geom_line(aes(date,PPC5,col=cal,group=id))+
    geom_vline(xintercept = step_df$End)
  
  cal_period <- data_PPC %>% 
    filter(cal == 1) %>% 
    filter(!is.na(P_roll)) %>% 
    group_by(id) %>% 
    mutate(zeit = as.numeric(difftime(date,min(date))),
           sensor_drift = predict(glm(P_roll ~ zeit)),
           P_roll_cal = P_roll - sensor_drift)
  
  data_PPC <- merge(data_PPC,cal_period[,c("date","sensor_drift","id")],all = T)
  data_PPC <- data_PPC %>% 
    group_by(id) %>% 
    mutate(sensor_drift = imputeTS::na_interpolation(sensor_drift),
           P_roll_cal = P_roll - sensor_drift)

  
  ggplot(data_PPC)+
    geom_point(data = subset(data_PPC,cal == 1),aes(date,P_roll,col=id))+
    geom_line(aes(date,P_roll,col=id))+
    geom_line(aes(date,sensor_drift,col=id))
    

  ggplot(data_PPC)+
    geom_line(aes(date,P_roll_cal,col=id))

  ############
  #probe 1 u 2
  
  
  data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")
  injections
  data_probe1u2$inj <- 0
  for(i in 1:nrow(injections)){
    inj_id <- daterange_id(data_probe1u2,c(injections$Start[i],injections$Ende[i]))
    data_probe1u2$inj[inj_id] <- 1
  }
  
  data_probe1u2 <- data_probe1u2 %>% 
    filter(inj == 1) %>% 
    group_by(tiefe) %>% 
    mutate(CO2_smp1_roll = RcppRoll::roll_mean(CO2_smp1,5,fill=NA),
           CO2_smp2_roll = RcppRoll::roll_mean(CO2_smp2,5,fill=NA)
    )
  
  ggplot(data_probe1u2)+
    geom_line(aes(date,CO2_smp1_roll,col=factor(tiefe)))+
    geom_line(aes(date,CO2_smp2_roll,col=factor(tiefe)))
  
  ###########
  #CO2atm
  
  chamber_data <- read_arduino_chamber(datelim=datelim)
  
  
  chamber_data$atm <- ifelse(minute(chamber_data$date) %% 30 > 5 & minute(chamber_data$date) %% 30 < 29,1,0)
  
  CO2_atm <- chamber_data %>% 
    filter(atm == 1) %>% 
    select(date,CO2) %>% 
    mutate(date = round_date(date,"10 mins")) %>% 
    group_by(date) %>% 
    summarise(CO2_atm = mean(CO2,na.rm=T)) %>% 
    mutate(CO2_atm = imputeTS::na_interpolation(CO2_atm))
  
  
  #########
  #swc
  # source("./PP_kammer/Bodenfeuchte_PP_Kammer.R")
  # 
  # swc_sub <- sub_daterange(swc_wide,datelim)
  # ########################
  
  
  data_PPC_wide <- tidyr::pivot_wider(data_PPC[,c("date","id","P_roll_cal","PPC5","cal","step_id")],names_from = id,values_from =c(P_roll_cal,PPC5))
  names(data_PPC_wide) <- str_replace_all(names(data_PPC_wide),c("P_roll_cal"="P","PPC5"="PPC"))
  
  data_probes_wide <- tidyr::pivot_wider(data_probe1u2,id_cols=c(date,T_C),names_from = tiefenstufe,values_from = matches("CO2_smp"))
  
  
  data <- merge(data_probes_wide,data_PPC_wide,all.x = T)
#  data <- merge(data,swc_sub)
  data <- merge(data,CO2_atm,all.x = T)
  data <- data %>% mutate(zeit = as.numeric(difftime(date,min(date))))

  data_long <- tidyr::pivot_longer(data,matches("CO2_smp\\d_roll_\\d"),names_pattern = "CO2_smp(\\d)_roll_(\\d)",values_to = "CO2",names_to = c("probe","tiefe")) %>% 
    select(!matches("CO2_smp\\d_\\d"))
  data_long$smp_depth <- paste(data_long$probe,data_long$tiefe,sep="_")
  data_long <- data_long %>% 
    group_by(tiefe,probe) %>% 
    mutate(diff_CO2 = abs(c(NA,diff(CO2))))
           #CO2 = ifelse(diff_CO2 > 400,NA,CO2))
  range(data_long$date)
  data_long$cal[!daterange_id(data_long,range(data_PPC$date)+3600 * c(-1,1))] <- 1
  
  ggplot(data_long)+
    geom_vline(xintercept = step_date)+
  #  geom_line(aes(date,CO2,col=factor(cal),group = tiefe))+
    geom_line(aes(date,T_C,col=factor(cal),group = tiefe))+
    facet_wrap(~probe,ncol = 1)
  
  data_long <- data_long %>% 
    group_by(step_id,tiefe,probe) %>% 
    mutate(dummy = 1,
           mode_zeit = cumsum(dummy))#,
  #cal_trim = ifelse(mode_zeit > 4,cal,0))
  data_long$CO2_preds <- NA 
  
  for(i in unique(data_long$smp_depth)){
    cal_df <- subset(data_long, smp_depth == i & cal == "1" & !is.na(CO2))
    cal_datelim <- range(cal_df$date)
    if(length(!is.na(cal_df$CO2)) > 10){
      #fm <- mgcv::gam(CO2 ~ s(zeit),data = cal_df)
      fm <- glm(CO2 ~ poly(zeit,2),data = cal_df)
      tiefenID <- which(data_long$smp_depth == i & daterange_id(data_long,cal_datelim))
      data_long$CO2_preds[tiefenID] <- predict(fm,newdata = data_long[tiefenID,])
    }
  }
  data_long$CO2_offset <- data_long$CO2 - data_long$CO2_preds
  
  
  data_long$P_horiz <- data_long$P_1 - data_long$P_3
  
  # ggplot(data_long)+
  #   geom_line(aes(date,mode_zeit))+
  #   geom_hline(yintercept = 9)
  data_long$CO2_cal <- ifelse(data_long$cal == 1,NA,data_long$CO2)
  
  
  step_df$Start[1] <- min(data_long$date)
  step_df$End[nrow(step_df)] <- max(data_long$date)
  names(data_long)
  if(Versuch == 26){
    data_long <- subset(data_long,!(probe == 2 & tiefe ==7) )
  }
  
  CO2_offset_plot <- ggplot(data_long)+
    geom_vline(xintercept = step_date,col="grey",linetype=2)+
    geom_line(aes(date,CO2_cal,col=tiefe))+
    geom_point(data = subset(data_long,cal == 1),aes(date,CO2,col=tiefe),pch=1)+
    geom_line(aes(date,CO2,col=tiefe),alpha = 0.5)+
    geom_line(aes(date,CO2_preds,col=tiefe),linetype=2)+
    guides(alpha=F)+
    labs(y = expression(CO[2]~"(ppm)"))+
    facet_wrap(~paste("probe",probe),ncol=1,scales = "free_y")
  
  
  
  
  CO2_plot <- 
    ggplot(data_long)+
    geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
    scale_alpha(range = c(0,0.4))+
    guides(alpha=F)+
    geom_hline(yintercept = 0,col="grey",linetype=2)+
    geom_line(aes(date,CO2_offset,col=tiefe))+
    geom_vline(xintercept = step_date,col="grey",linetype=2)+
    facet_wrap(~paste("probe",probe),ncol=1)+
    labs(y = expression(CO[2~offset]~(ppm)), x ="")
  
  P_plt <- ggplot(data_long)+
    geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
    scale_alpha(range = c(0,0.4))+
    guides(alpha=F)+
    geom_line(aes(date,P_horiz,col="lateral"))+
    geom_hline(yintercept = 0,col="grey",linetype=2)+
    geom_vline(xintercept = step_date,col="grey",linetype=2)+
    geom_line(aes(date,P_1,col="1"))+
    geom_line(aes(date,P_2,col="2"))+
    geom_line(aes(date,P_3,col="3"))+
    geom_line(aes(date,P_4,col="4"))+
    scale_color_manual(values = c(scales::hue_pal()(4),1))+
    labs(col="P",y = expression(P[roll]~"(Pa)"))
  
  PPC_plt <- ggplot(data_long)+
    geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
    scale_alpha(range = c(0,0.4))+
    guides(alpha=F)+
    geom_line(aes(date,PPC_1,col="1"))+
    geom_line(aes(date,PPC_2,col="2"))+
    geom_vline(xintercept = step_date,col="grey",linetype=2)+
    geom_line(aes(date,PPC_3,col="3"))+
    geom_line(aes(date,PPC_4,col="4"))+
    labs(col="PPC",y = "PPC (Pa/s)", x ="")
  
  ggpubr::ggarrange(CO2_offset_plot+labs(title = paste("Versuch",paste0(Versuch,":"),pp_chamber$Modus[Versuch])),PPC_plt,ncol=1,align = "v",heights = c(4,1))+
    ggsave(paste0(plotpfad_PPchamber,"CO2_offset_fm_Sand",Versuch,".png"),width = 7,height = 6)
  ggpubr::ggarrange(CO2_plot+labs(title = paste("Versuch",paste0(Versuch,":"),pp_chamber$Modus[Versuch])),PPC_plt,P_plt,ncol=1,align = "v",heights = c(2,1,1))+
    ggsave(paste0(plotpfad_PPchamber,"CO2_offset_PPC_Sand",Versuch,".png"),width = 7,height = 6)
  
  data_long$Versuch <- paste("Sand",Versuch)
  save(data_long,file = paste0(datapfad_PP_Kammer,"CO2_offset_Sand",Versuch,".RData"))
}
#data_long$CO2_offset[data_long$CO2_offset < -300] <- NA

P_scatter <- ggplot(subset(data_long,mode_zeit > 9))+
  geom_point(aes(P_horiz,CO2_offset,col=factor(tiefe)))+
  geom_smooth(aes(P_horiz,CO2_offset,col=factor(tiefe)),method = "glm")+
  facet_grid(~probe)
PPC_scatter <- ggplot(subset(data_long,mode_zeit > 9))+
  geom_point(aes(PPC_2,CO2_offset,col=factor(tiefe)))+
  geom_smooth(aes(PPC_2,CO2_offset,col=factor(tiefe)),method = "glm")+
  facet_grid(~probe)

ggpubr::ggarrange(PPC_scatter,P_scatter,ncol=1,align = "v",common.legend = T,legend = "right")
ggplot(data)+
  geom_point(aes(CO2_smp2_7,T_C,col=mode))

