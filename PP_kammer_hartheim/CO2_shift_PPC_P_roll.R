hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 


klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
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

PP_Versuche <- grep("PP|\\dD",pp_chamber$Modus)
Versuch <- 3
for(Versuch in PP_Versuche){
  datelim <- c(pp_chamber$Start[Versuch]-3600 * 12,pp_chamber$Ende[Versuch]+3600*12)
  
  if(Versuch == 1){
    datelim[1] <- ymd_h("2022-09-19 19")
  }
  if(Versuch == 2){
    datelim[1] <- ymd_h("2022-09-26 19")
  }
  if(Versuch == 17){
    datelim[1] <- ymd_h("2022-11-17 10")
  }
  ###################
  #PPC data
  
  data_PPC <- read_PP(datelim = datelim,table="PP_1min")
  
  names(data_PPC)
  #data_PPC <- subset(data_PPC,id %in% c(1:5))
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
  
  ggplot(data_PPC)+
    geom_line(aes(date, P_roll, col=factor(id)))
  data_PPC[which(data_PPC$dt > 3600),c("PPC","PPC5","P_roll")] <- NA
  
  # data_Proll_wide <- tidyr::pivot_wider(data_PPC,date,values_from = P_roll,names_from = id,names_prefix = "id_")
  # data_Proll_wide[paste0("id_",1:4)] <- data_Proll_wide[paste0("id_",1:4)] - data_Proll_wide$id_5
  # data_Proll <- tidyr::pivot_longer(data_Proll_wide[,1:6],paste0("id_",1:5),names_prefix = "id_",names_to = "id",values_to="P_roll")
  
  step_thr <- 0.04
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
  
  PPC_steps_wide <- tidyr::pivot_wider(PPC_steps,id_cols = date,names_from = id,values_from = P_roll,names_prefix = "P_roll_")
  names(data_PPC)
  step_df <- data_PPC %>%
    group_by(id) %>% 
    mutate(step = ifelse(date %in% !!step_date,1,0),
           step_id=cumsum(step)) %>% 
    group_by(step_id) %>% 
    summarise(across(PPC,mean,na.rm=T),
              Start = min(date),
              End = max(date)) %>% 
    mutate(cal = ifelse(PPC > 0.1,0,1))
  
  ggplot(PPC_steps)+
    geom_vline(xintercept = step_date,col="grey",linetype=2)+
    geom_hline(yintercept = step_thr,col="grey",linetype=2)+
    geom_line(aes(date,PPC_diff))+
    geom_line(aes(date,PPC/2,col=factor(id)))
  
  
  data_PPC$step_id <- NA
  
  
  for(i in 1:nrow(step_df)){
    id <- which(daterange_id(data_PPC,c(step_df$Start[i]+3600*3,step_df$End[i]-1800)))
    data_PPC$step_id[id] <- step_df$step_id[i]
  }
  ggplot(data_PPC)+
    geom_line(aes(date,PPC,col=factor(step_id),group = id))
  
  data_PPC$cal <- factor(data_PPC$step_id,levels = step_df$step_id,labels = step_df$cal)
  
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
    geom_line(aes(date,P_roll,col=id))+
    geom_line(aes(date,sensor_drift,col=id))
  ggplot(data_PPC)+
    geom_line(aes(date,P_roll_cal,col=id))
  
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
  
  CO2_atm <- chamber_data %>% 
    filter(atm == 1) %>% 
    select(date,CO2) %>% 
    mutate(date = round_date(date,"10 mins")) %>% 
    group_by(date) %>% 
    summarise(CO2_atm = mean(CO2,na.rm=T)) %>% 
    mutate(CO2_atm = imputeTS::na_interpolation(CO2_atm))
  
  
  #########
  #swc
  source("./PP_kammer_hartheim/SWC_hartheim.R")
  
  swc_sub <- sub_daterange(swc_wide,datelim)
  ########################
  
  
  data_PPC_wide <- tidyr::pivot_wider(data_PPC[,c("date","id","P_roll_cal","PPC5","cal","step_id")],names_from = id,values_from =c(P_roll_cal,PPC5))
  names(data_PPC_wide) <- str_replace_all(names(data_PPC_wide),c("P_roll_cal"="P","PPC5"="PPC"))
  
  data_probes_wide <- tidyr::pivot_wider(data_probe1u2,id_cols=c(date,T_C),names_from = tiefenstufe,values_from = matches("CO2_smp"))
  
  
  data <- merge(data_PPC_wide,data_probes_wide)
  data <- merge(data,swc_sub,all.x = T)
  data <- merge(data,CO2_atm)
  data <- data %>% mutate(zeit = as.numeric(difftime(date,min(date))))
  if(Versuch %in% c(23)){
    data[,paste0("CO2_smp1_roll_",1:2)] <- NA
  }
  
  
  data_long <- tidyr::pivot_longer(data,matches("CO2_smp\\d_roll_\\d"),names_pattern = "CO2_smp(\\d)_roll_(\\d)",values_to = "CO2",names_to = c("probe","tiefe"))
  data_long$smp_depth <- paste(data_long$probe,data_long$tiefe,sep="_")
  data_long <- data_long %>% 
    group_by(tiefe,probe) %>% 
    mutate(diff_CO2 = abs(c(NA,diff(CO2))),
           CO2 = ifelse(diff_CO2 > 400,NA,CO2))
  
  data_long <- data_long %>% 
    group_by(step_id,tiefe,probe) %>% 
    mutate(dummy = 1,
           mode_zeit = cumsum(dummy))#,
  #cal_trim = ifelse(mode_zeit > 4,cal,0))
  data_long$CO2_preds <- NA 
  if(Versuch == 18) data_long <- subset(data_long,date > ymd_h("2022.11.23 00"))
  
  for(i in unique(data_long$smp_depth)){
    cal_df <- subset(data_long, smp_depth == i & cal == "1" & !is.na(CO2))
    cal_datelim <- range(cal_df$date)
    if(length(!is.na(cal_df$CO2)) > 10){
      fm <- mgcv::gam(CO2 ~ s(zeit),data = cal_df)
      tiefenID <- which(data_long$smp_depth == i & daterange_id(data_long,cal_datelim))
      data_long$CO2_preds[tiefenID] <- predict(fm,newdata = data_long[tiefenID,])
    }
  }
  data_long$CO2_offset <- data_long$CO2 - data_long$CO2_preds
  
  
  data_long$P_horiz <- data_long$P_1 - data_long$P_3
  #data_long$P_horiz <- (data_long$P_1 + data_long$P_2)/2 - (data_long$P_3 + data_long$P_4)/2
  
  # ggplot(data_long)+
  #   geom_line(aes(date,mode_zeit))+
  #   geom_hline(yintercept = 9)
  data_long$CO2_cal <- ifelse(data_long$cal == 1,NA,data_long$CO2)
  
  data_long$CO2_shift <-  data_long$CO2_offset / data_long$CO2_preds * 100
  data_long$profile <- factor(data_long$probe,levels = 2:1,labels = 1:2)
  data_long$subspace <- factor(data_long$probe,levels = 2:1,labels = 2:3)
  data_long$depth <- factor(as.numeric(data_long$tiefe) * 3.5)
  
  step_df$Start[1] <- min(data_long$date)
  step_df$End[nrow(step_df)] <- max(data_long$date)
  
  if(plot){
    
    cols <- RColorBrewer::brewer.pal(4,"PuOr")
    
    PPC_col <- "brown"
    
    
    step_df <- step_df %>% 
      ungroup() %>% 
      mutate(period = cumsum(cal)) %>% 
      group_by(period) %>% 
      mutate(step = factor(cumsum(abs(cal-1))))
    
    CO2_offset_plot <- 
      ggplot(data_long)+
      geom_rect(data=subset(step_df,step != 0),aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=step))+
      scale_alpha_discrete("PPC level",range = c(0.4,0.1),guide = guide_legend(order = 1))+
      geom_line(data = subset(data_long, tiefe %in% 1:4),aes(date,CO2_cal,col=factor(tiefe,labels = c(1:4))))+
      scale_color_manual("subspace",values = c(cols),guide = guide_legend(order = 3))+
      # ggnewscale::new_scale_color()+
      # geom_line(data = subset(data_long, tiefe %in% 1),aes(date,CO2_cal,col=factor(tiefe,labels = "")))+
      # scale_color_manual("ambient PPC",values = PPC_col,guide = guide_legend(order = 4))+
      ggnewscale::new_scale_color()+
      geom_line(data = subset(data_long, tiefe %in% 1),aes(date,CO2_cal,col=factor(tiefe,labels = "")))+
      scale_color_manual(expression(P[lateral]),values = 1,guide = guide_legend(order = 5))+
      ggnewscale::new_scale_color()+
      #guides(alpha = F)+
      geom_vline(xintercept = step_date,col="grey",linetype=2)+
      geom_line(aes(date,CO2_cal,col=depth))+
      geom_point(data = subset(data_long,cal == 1),aes(date,CO2,col=depth),pch=20)+
      geom_line(aes(date,CO2,col=depth),alpha = 0.5)+
      geom_line(aes(date,CO2_preds,col=depth),linetype=2)+
      scale_color_discrete(guide = guide_legend(order = 2))+
      labs(y = expression(CO[2]~"(ppm)"),col = "depth (cm)")+
      facet_wrap(~paste0("profile ",profile," (subspace ",subspace,")"),ncol=1)
    # CO2_offset_plot <- 
    #   ggplot(data_long)+
    #   geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
    #   scale_alpha(range = c(0,0.4))+
    #   geom_line(data = subset(data_long, tiefe %in% 1:5),aes(date,CO2_cal,col=factor(tiefe,labels = c(1:4,"lateral"))))+
    #   scale_color_manual("subspace",values = c(cols,1))+
    #   ggnewscale::new_scale_color()+
    #   geom_vline(xintercept = step_date,col="grey",linetype=2)+
    #   geom_line(aes(date,CO2_cal,col=tiefe))+
    #   geom_point(data = subset(data_long,cal == 1),aes(date,CO2,col=tiefe),pch=20)+
    #   geom_line(aes(date,CO2,col=tiefe),alpha = 0.5)+
    #   geom_line(aes(date,CO2_preds,col=tiefe),linetype=2)+
    #   #guides(alpha=F)+
    #   labs(y = expression(CO[2]~"(ppm)"),col="depth")+
    #   #theme(legend.justification = "right") +
    #   facet_wrap(~paste0("profile ",profile," (subspace ",subspace,")"),ncol=1)
    # 
    
    
    
    CO2_plot <- 
      ggplot(data_long)+
      geom_rect(data=subset(step_df,step != 0),aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=step))+
      scale_alpha_discrete(range = c(0.4,0.1),guide = guide_legend(order = 1))+
      guides(alpha=F)+
      geom_hline(yintercept = 0,col="grey",linetype=2)+
      geom_line(aes(date,CO2_shift,col=tiefe))+
      geom_vline(xintercept = step_date,col="grey",linetype=2)+
      facet_wrap(~paste0("profile ",profile," (subspace ",subspace,")"),ncol=1)+
      labs(y = expression(CO[2]*"-shift"~("%")), x ="")
    #labs(y = expression(CO[2~offset]~(ppm)), x ="")
    
    
    P_plt <- 
      ggplot(data_long)+
      geom_rect(data=subset(step_df,step != 0),aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=step))+
      scale_alpha_discrete(range = c(0.4,0.1),guide = guide_legend(order = 1))+
      guides(alpha=F)+
      geom_vline(xintercept = step_date,col="grey",linetype=2)+
      geom_hline(yintercept = 0,col="grey",linetype=2)+
      geom_line(aes(date,P_horiz,col="lateral",linetype=""))+
      geom_line(aes(date,P_1,col="1"))+
      geom_line(aes(date,P_2,col="2"))+
      geom_line(aes(date,P_3,col="3"))+
      geom_line(aes(date,P_4,col="4"))+
      scale_color_manual(values = c(cols,1))+
      guides(col=F)+
      labs(linetype="lateral gradient",y = expression(P[mean]~"(Pa)"))
    
    PPC_plt <- 
      ggplot(data_long)+
      geom_rect(data=subset(step_df,step != 0),aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=step))+
      scale_alpha_discrete(range = c(0.4,0.1),guide = guide_legend(order = 1))+
      guides(alpha=F)+
      geom_line(aes(date,PPC_1,col="1"))+
      geom_line(aes(date,PPC_2,col="2"))+
      geom_vline(xintercept = step_date,col="grey",linetype=2)+
      geom_line(aes(date,PPC_3,col="3"))+
      geom_line(aes(date,PPC_4,col="4"))+
      #    geom_line(aes(date,PPC_6,col="ambient PPC"))+
      scale_color_manual(values = c(cols,PPC_col))+
      theme(legend.text.align = 0.5)+
      labs(col="subspace",y = "PPC (Pa/s)", x ="")
    
    ggpubr::ggarrange(CO2_offset_plot+labs(title = paste("Versuch",paste0(Versuch,":"),pp_chamber$Modus[Versuch]))+theme(axis.title.x = element_blank(),                                                                                       axis.text.x = element_blank()),
                      CO2_plot+theme(axis.title.x = element_blank(),
                                     axis.text.x = element_blank()),
                      PPC_plt+theme(axis.title.x = element_blank(),
                                    axis.text.x = element_blank()),
                      P_plt,ncol=1,align = "v",heights = c(3,2,0.8,1),common.legend = T,legend = "right")+
      ggsave(paste0(plotpfad_PPchamber,"CO2_offset_",Versuch,".png"),width = 7,height = 8)
    #ggpubr::ggarrange(CO2_plot+labs(title = paste("Versuch",paste0(Versuch,":"),pp_chamber$Modus[Versuch])),PPC_plt,P_plt,ncol=1,align = "v",heights = c(2,1,1))+
    #  ggsave(paste0(plotpfad_PPchamber,"CO2_offset_PPC_",Versuch,".png"),width = 7,height = 6)
    
  }
  data_long$Versuch <- Versuch
  save(data_long,file = paste0(datapfad_PP_Kammer,"CO2_offset_",Versuch,".RData"))
}
#data_long$CO2_offset[data_long$CO2_offset < -300] <- NA


####################################
#plot für MS
######################################

if(Versuch == 3){
  Sys.setlocale("LC_ALL", "English")
  ggpubr::ggarrange(CO2_offset_plot+theme(axis.title.x = element_blank(),                                                                                       axis.text.x = element_blank()),
                    CO2_plot+theme(axis.title.x = element_blank(),
                                   axis.text.x = element_blank()),
                    PPC_plt+theme(axis.title.x = element_blank(),
                                  axis.text.x = element_blank()),
                    P_plt,ncol=1,align = "v",heights = c(3,2,0.8,1),common.legend = T,legend = "right")+
    ggsave(paste0(plotpfad_PPchamber,"Figure_5.png"),width = 7,height = 7)
  # ggpubr::ggarrange(CO2_offset_plot,CO2_plot,PPC_plt,P_plt,ncol=1,align = "v",heights = c(3,2,1,1))+
  #   ggsave(paste0(plotpfad_PPchamber,"CO2_shift_PPC_.png"),width = 7,height = 8)
  # 
  Sys.setlocale("LC_ALL", "")
}
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

