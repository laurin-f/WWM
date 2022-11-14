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
#######################################
#####################################
#read probes
data_ls <- vector("list",length = length(dates_ls))
Versuch <- 3
for(Versuch in seq_along(dates_ls)){
  
  #CO2 Werte für i-te injektion inklusive 2 tage vorher und nachher
  data <- read_sampler(datelim = dates_ls[[Versuch]] + (3600*24*2 * c(-1,1)))
  data_PP <- read_PP(datelim = dates_ls[[Versuch]] + (3600*24*2 * c(-1,1)),format = "wide")
  data_PP$P_roll <- RcppRoll::roll_mean(data_PP$P_2,30,fill = NA)
  
  data <- merge(data,data_PP,all.x = T)
  #data <- read_sampler(datelim = dates_ls[[Versuch]] + (3600*24*2 * c(-1,0)))
  data$tiefe <- data$tiefe
  
  data$Versuch <- Versuch
  inj$date <- round_date(inj$date,"10 mins")
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
    mutate(inj_mol_m2_s = imputeTS::na_interpolation(CO2_mol_m2_s),
           inj_mol_m2_s = ifelse(inj == 1 ,inj_mol_m2_s,0),
           offset = CO2_smp1 - CO2_smp2,
           date_int = as.numeric(date),
           CO2_ref = RcppRoll::roll_mean(CO2_smp2,5,fill=NA),
           CO2_inj = RcppRoll::roll_mean(CO2_smp1,5,fill=NA)
    )
  
  data_cal <- subset(data, cal == 1)
  
  ggplot(data)+
    geom_line(aes(date,CO2_inj,col=factor(cal),group=tiefe))
  
  ggplot(data)+
    geom_line(aes(date,PPC_2,col=factor(cal),group=tiefe))
  ####################################
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


save(data,data_uncert,file=paste0(datapfad_PP_Kammer,"data_tracer_hartheim.RData"))
#########################
#plots
unique(data$Versuch)
Versuch_x <- 6
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
