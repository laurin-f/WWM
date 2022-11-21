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
inj <- inj %>% 
  mutate(date = round_date(date,"10 mins")) %>% 
  group_by(date) %>% 
  summarise(across(everything(),mean)) %>% 
  as.data.frame()
#######################################
#####################################
#read probes

data_ls <- vector("list",length = length(dates_ls))
Versuch <- 7
for(Versuch in seq_along(dates_ls)){
  
  #CO2 Werte für i-te injektion inklusive 2 tage vorher und nachher
  data <- read_sampler(datelim = dates_ls[[Versuch]] + (3600*24*2 * c(-1,1.5)))
  data_PP <- read_PP(datelim = dates_ls[[Versuch]] + (3600*24*2 * c(-1,1.5)),format = "wide")
  data_PP$P_roll <- RcppRoll::roll_mean(data_PP$P_2,30,fill = NA)
  
  data <- merge(data,data_PP,all.x = T)
  #data <- read_sampler(datelim = dates_ls[[Versuch]] + (3600*24*2 * c(-1,0)))
  data$tiefe <- data$tiefe
  
  data$Versuch <- Versuch
  
  
  #Spalte in inj hat 1er während der injektion
  inj_id <- daterange_id(data,dates_ls[[Versuch]])
  data$inj <- ifelse(inj_id,1,0)
  #Spalte cal hat 1er vor der injektion und danach mit 20 h Abstand nach der injektion da hier der Tracer noch sichtbar ist
  cal_id <- daterange_id(data,dates_ls[[Versuch]] + (3600*20 * c(0,1)))
  data$cal <- ifelse(cal_id,0,1)
  data$cal[data$PPC_2 > 0.1] <- 0
  data$cal[data$P_roll > 1 | data$P_roll < -1] <- 0
  data <- merge(data,inj[inj$Versuch == Versuch,c("date","CO2_mol_m2_s")],all = T)
  data <- data %>% 
    group_by(tiefe) %>% 
    mutate(offset = CO2_smp1 - CO2_smp2,
           date_int = as.numeric(date),
           CO2_ref = RcppRoll::roll_mean(CO2_smp2,5,fill=NA),
           CO2_inj = RcppRoll::roll_mean(CO2_smp1,5,fill=NA)
    )
  inj_vals <- length(which(!is.na(data$CO2_mol_m2_s)))
  if(inj_vals > 1){
    data <- data %>% 
      mutate(inj_mol_m2_s = imputeTS::na_interpolation(CO2_mol_m2_s),
             inj_mol_m2_s = ifelse(inj == 1 ,inj_mol_m2_s,0))
  }
  data_cal <- subset(data, cal == 1)
  
  # ggplot(data)+
  #   geom_line(aes(date,CO2_inj,col=factor(cal),group=tiefe))
  # 
  # ggplot(data)+
  #   geom_line(aes(date,PPC_2,col=factor(cal),group=tiefe))
  # ####################################
  #adj CO2 tracer
  #######################################
  
  data$CO2_refadj <- NA
  data$offset_drift <- NA
  for(i in 1:7){
    fm_data <- subset(data_cal,tiefenstufe==i)
    if(any(!is.na(fm_data$offset))){
      fm_drift <- glm(offset ~ poly(date_int,1),data=fm_data)
      ID <- which(data$tiefenstufe==i)
      data$offset_drift[ID] <- predict(fm_drift,newdata = data[ID,])
      data$CO2_refadj[ID] <- data$CO2_ref[ID] + data$offset_drift[ID]
    }
  }
  data$CO2_tracer_drift <- data$CO2_inj - (data$CO2_refadj)
  data$T_soil <- data$T_C
  
  data_ls[[Versuch]] <- data
}

data <- do.call(rbind,data_ls)


data$flag <- 0
# flagid <- daterange_id(data,ymd_hm("2022.07.02 21:00","2022.07.03 10:00"))
# flagid2 <- daterange_id(data, ymd_h("2022.06.26 10","2022.06.27 17"))
# flagid3 <- daterange_id(data, ymd_h("2022.07.01 20","2022.07.02 10"))
# flagid4 <- daterange_id(data, ymd_h("2022.07.02 22","2022.07.03 10"))
# 
# data$flag[flagid & data$tiefe == -3.5] <- 1
# data$flag[flagid2] <- 1
# data$flag[flagid3 & data$tiefe == -3.5] <- 1
# data$flag[flagid4 & data$tiefe == -3.5] <- 1

names(data)
data_uncert <- data %>% 
  #select(matches("(date|tiefe|CO2_(ref|inj)|cal|CO2_tracer_|Versuch|flag)")) %>% 
  filter(!is.na(tiefe) & flag == 0) %>% 
  group_by(tiefe,Versuch) %>% 
  mutate(across(c("CO2_refadj"),
                list(
                  min = ~. + max(CO2_inj[cal==1] - .[cal==1],na.rm=T),
                  #q25 = ~. + quantile(CO2_inj[cal==1] - .[cal==1],probs=0.75,na.rm=T),
                  #q75 = ~. + quantile(CO2_inj[cal==1] - .[cal==1],probs=0.25,na.rm=T),
                  max = ~. + min(CO2_inj[cal==1] - .[cal==1],na.rm=T)
                )
  )
  ) %>% 
  mutate(across(matches("CO2_refadj_(max|min|q\\d+)"),
                list(
                  CO2_tracer= ~CO2_inj - .
                ),
                .names="{.fn}_{.col}"
  )
  ) %>% 
  rename_with(~str_remove(.,"CO2_refadj_"),matches("^CO2_tracer_CO2_refadj")) %>% 
  #select(-matches("preds_")) %>% 
  ungroup() %>%
  as.data.frame()
# 
# data_uncert <- data_uncert %>% 
#   mutate(
#     CO2_tracer_mingradient = case_when(
#       tiefe >= -7 ~ CO2_tracer_max,
#       tiefe == -10.5 ~ CO2_tracer_q75,
#       tiefe == -14 ~ CO2_tracer_drift,
#       tiefe == -17.5 ~ CO2_tracer_q25,
#       tiefe <= -21 ~ CO2_tracer_min
#     ),
#     CO2_tracer_maxgradient = case_when(
#       tiefe >= -7 ~ CO2_tracer_min,
#       tiefe == -10.5 ~ CO2_tracer_q25,
#       tiefe == -14 ~ CO2_tracer_drift,
#       tiefe == -17.5 ~ CO2_tracer_q75,
#       tiefe <= -21 ~ CO2_tracer_max
#     )
#   )

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

save(data,data_uncert,file=paste0(datapfad_PP_Kammer,"data_tracer_hartheim.RData"))
###################################################

########################
#PLOT
####################
#PP

##################
#Metadata
Versuch_x <- 2
for(Versuch_x in c(2,3,5,6,7)){
  data_x <- subset(data,Versuch == Versuch_x)
  
  data_PPC <- read_PP(datelim = range(data_x$date),table.name = "PP_1min")
  PPC_wide <- read_PP(datelim = range(data_x$date),format="wide",table.name = "PP_1min")
  
  
  data_PPC <- subset(data_PPC,id != 5)
  dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T),2)
  
  PPC_wide <- PPC_wide %>% 
    mutate(dP_horiz_raw = (abs(P_1 - P_2) + abs(P_2 - P_3))/2,
           dP_horiz = RcppRoll::roll_mean(dP_horiz_raw,10*60/!!dt,fill=NA))
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
  
  #data_PPC <- sub_daterange(data_PPC,PPC_daterange_short)
  
  step_thr <- 0.03
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
  
  step_df <- PPC_steps %>% 
    mutate(step = ifelse(date %in% !!step_date,1,0),
           step_id=cumsum(step)) %>% 
    group_by(step_id) %>% 
    summarise(PPC = mean(PPC,na.rm=T),
              Start = min(date),
              End = max(date))
  
  PP_plot <- 
    ggplot(subset(data_PPC,date %in% round_date(date,"mins") & id == 1))+
    geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
    geom_line(aes(date,RcppRoll::roll_mean(PPC5,15,fill = NA)))+
    guides(fill=F)+
    theme(legend.text.align = 0)+
    scale_alpha(range = c(0,0.3))+
    guides(alpha = F)+
    labs(x="",y="PPC (Pa/s)")
  
  P_roll_plot <- ggplot(subset(data_PPC,id %in% 1:4) )+
    geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
    geom_hline(yintercept = 0,col="grey",linetype=2)+
    geom_line(aes(date,P_roll,col=id),alpha=0.5, linetype=2)+
    scale_alpha(range = c(0,0.3))+
    guides(alpha = F)+
    scale_color_brewer(type="qual",palette="Dark2")+
    labs(x="",y=expression(P["moving average"]~"(Pa)"))
  dP_horiz_plot <- ggplot(PPC_wide)+
    geom_line(aes(date,dP_horiz))
  
  CO2_plot <- 
    ggplot(data_x)+
    geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
    scale_alpha(range = c(0,0.3))+
    guides(alpha = F)+
    geom_ribbon(aes(date,ymin=CO2_refadj,ymax=CO2_inj,fill=as.factor(-tiefe)),alpha=0.2)+
    geom_line(aes(date,CO2_refadj,col=as.factor(-tiefe),linetype="ref adj"))+
    geom_line(aes(date,CO2_inj,col=as.factor(-tiefe),linetype="inj"))+
    theme(axis.title.x = element_blank())+
    labs(y=expression(CO[2]~(ppm)),linetype="profile",col="depth (cm)",fill="depth (cm)")
  
  tracerplt <-
    ggplot(subset(data_x,tiefe != 0))+
    geom_rect(data=step_df,aes(xmin = Start, xmax=End,ymin=-Inf,ymax = Inf,alpha=PPC))+
    scale_alpha(range = c(0,0.3))+
    guides(alpha = F)+
    geom_line(aes(date,(CO2_tracer_roll),col=as.factor(-tiefe)))+#,linetype="PPC"))+
    theme(axis.title.x = element_blank())+
    guides(fill=F)+
    labs(y=expression("tracer CO"[2]~(ppm)),col="depth (cm)")
  
  
  ggpubr::ggarrange(CO2_plot+
                      xlim(range(data_x$date))+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
                    tracerplt+
                      xlim(range(data_x$date))+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
                    PP_plot+
                      
                      xlim(range(data_x$date))+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
                    P_roll_plot+
                      xlim(range(data_x$date))+geom_vline(xintercept = step_date,linetype=2,alpha=0.2),
                    heights = c(2,2,1,1),ncol=1,
                    legend = "right",align="v",common.legend = T)+
    ggsave(filename = paste0(plotpfad_PPchamber,"injection_hartheim_",Versuch_x,".png"),width=8,height=7)
}
#########################
#plots
unique(data$Versuch)
Versuch_x <- 7
#####################
#CO2 inj un refadj plot
PPC_plot <- ggplot(subset(data,!is.na(Versuch) & Versuch==Versuch_x))+
  geom_line(aes(date,PPC_2,col=factor(cal),group=1))
CO2_adj <- ggplot(subset(data,!is.na(Versuch) & Versuch==Versuch_x))+
  geom_ribbon(aes(date,ymin=CO2_refadj,ymax=CO2_inj,fill=as.factor(abs(tiefe))),alpha=0.2)+
  
  geom_line(aes(date,CO2_refadj,col=as.factor(abs(tiefe)),linetype="ref adj"))+
  geom_line(aes(date,CO2_inj,col=as.factor(abs(tiefe)),linetype="inj"))+
  labs(col="depth (cm)",fill="depth (cm)",y=expression(CO[2]~"(ppm)"))

egg::ggarrange(CO2_adj+theme(axis.title.x = element_blank(),
                             axis.text.x = element_blank()),PPC_plot,heights = c(3,1))
#  geom_vline(xintercept = ymd_h("2022.06.26 10","2022.06.27 17","2022.07.01 20","2022.07.02 10","2022.07.02 22","2022.07.03 10"))
#  facet_wrap(~Versuch,scales = "free_x",ncol=1)

#####################
#uncert
ggplot(subset(data_uncert,!is.na(Versuch) & Versuch==Versuch_x))+
  #geom_ribbon(aes(date,ymin=CO2_refadj,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.2)+
  geom_ribbon(aes(date,ymin=CO2_refadj_min,ymax=CO2_refadj_max,fill=as.factor(tiefe)),alpha=0.2)+
  geom_line(aes(date,CO2_refadj,col=as.factor(tiefe),linetype="ref adj"))+
  geom_line(aes(date,CO2_inj,col=as.factor(tiefe),linetype="inj"))#+
#  geom_vline(xintercept = step_date,linetype=2,alpha=0.2)

tracer_plot <- ggplot(subset(data_uncert,!is.na(Versuch) & Versuch==Versuch_x))+
  #geom_ribbon(aes(date,ymin=CO2_refadj,ymax=CO2_inj,fill=as.factor(tiefe)),alpha=0.2)+
  geom_ribbon(aes(date,ymin=CO2_tracer_min,ymax=CO2_tracer_max,fill=as.factor(abs(tiefe))),alpha=0.2)+
  geom_line(aes(date,CO2_tracer_drift,col=as.factor(abs(tiefe))))#+
# 
egg::ggarrange(tracer_plot+
                 labs(title=paste("Versuch",Versuch_x))+
                 theme(axis.title.x = element_blank(),
                       axis.text.x = element_blank()),PPC_plot,heights = c(3,1))
# ggplot(subset(data_uncert,!is.na(Versuch) & Versuch==Versuch_x & date %in% round_date(date,"3 hours") & inj==1)[1:63,])+
#   geom_line(aes(CO2_tracer_drift,tiefe,col=as.factor(date),linetype="a"),orientation = "y")+
#   geom_line(aes(CO2_tracer_min,tiefe,col=as.factor(date),linetype="min"),orientation = "y")+
#   geom_line(aes(CO2_tracer_max,tiefe,col=as.factor(date),linetype="max"),orientation = "y")+
#   facet_wrap(~date_int)+
#   guides(col=F)
###################
#tracer plot

ggplot(subset(data,!is.na(Versuch)))+
  geom_line(aes(date,CO2_tracer_drift,col=as.factor(tiefe)))+
  facet_wrap(~Versuch,scales = "free_x",ncol=1)

#  geom_vline(xintercept = ymd_hm("2022.07.06 15:00"))

######################
#injection rate plot
ggplot(subset(data,!is.na(Versuch)))+
  geom_line(aes(date,inj_mol_m2_s,col=as.factor(cal),group=1))+
  facet_wrap(~Versuch,scales = "free_x",ncol=1)
