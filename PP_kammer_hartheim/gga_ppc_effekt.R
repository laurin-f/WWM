hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 


klimapfad_CR1000<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/Hartheim CR1000/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

chamber_arduino_pfad <- paste0(hauptpfad,"/Daten/Urdaten/Kammermessungen_Arduino/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS","data.table","glmnet")
check.packages(packages)
theme_set(theme_classic())


##################
#Metadata
pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen_hartheim.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

now <- now()
tz(now) <- "UTC"
datelim <- c(ymd_h("22.09.27 10","22.12.10 10"))
datelim <- c(ymd_h("22.09.27 10","23.02.22 10"))
plot <-  T
load <- T

#datelim <- c(ymd_h("2022-04-13 18"),ymd_h("2022-04-25 18"))
#datelim <- ymd_hm("2022.05.02 00:00","2022.05.02 01:20")
#datelim <- ymd_hm("2022.05.08 18:00","2022.05.10 13:20")
#datelim <- ymd_hm("2022.05.12 10:00","2022.05.16 16:00")


#############
#klima_data

load(paste0(klimapfad_CR1000,"klima_data_PP_kammer.RData"))
names(klima)
###############
#load PPC

data_PPC <- read_PP(datelim = datelim,table.name = "PP_1min")



dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T))

data_PPC <- data_PPC %>% 
  group_by(id) %>%
  mutate(dt = diff_time(date,"secs"),
         PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
         PPC_meanr3 = RcppRoll::roll_meanr(P_diff,3*60*60/!!dt,fill=NA),
         PPC_meanr6 = RcppRoll::roll_meanr(P_diff,6*60*60/!!dt,fill=NA),
         P_roll = RcppRoll::roll_mean(P,3*60/!!dt,fill=NA),
         PPC_diff = abs(c(NA,diff(PPC5)))/!!dt,
         PPC_diff_meanr = RcppRoll::roll_meanr(PPC_diff,2*60,fill=NA))




data_PPC$id[data_PPC$id == 6] <- "outside"
data_PPC[which(data_PPC$dt > 3600),c("PPC","PPC5","P_roll")] <- NA

data_PPC_wide <- tidyr::pivot_wider(subset(data_PPC,id %in% c(1:3,"outside")),date,names_from = id,values_from = matches("^P"))  

data_PPC_wide$PPC_sum <- data_PPC_wide$PPC_2 + data_PPC_wide$PPC_outside
data_PPC_wide$P_horiz_1 <- RcppRoll::roll_mean(data_PPC_wide$P_horiz_1,10,fill=NA)
data_PPC_wide$P_horiz_2 <- RcppRoll::roll_mean(data_PPC_wide$P_horiz_2,10,fill=NA)
#data_PPC_wide$P_horiz2 <- RcppRoll::roll_mean(abs(data_PPC_wide$P_2 - data_PPC_wide$P_3),10,fill=NA)


# ggplot(subset(data_PPC_wide[1:5000,]))+
#   geom_line(aes(date,P_roll_2,col="P_roll"))+
#   geom_line(aes(date,PPC_2,col="PPC"))+
#   geom_line(aes(date,P_horiz_2,col="P_horiz"))
# 
# ggplot(subset(data_PPC_wide[1:5000,]))+
#   geom_line(aes(date,PPC_2,col="PPC"))+
#   geom_line(aes(date,PPC_sum,col="PPC_sum"))+
#   geom_line(aes(date,PPC_meanr6_2,col="PPC_meanr6"))+
#   geom_line(aes(date,PPC_diff_meanr_2*1000,col="meanr"))

if(load){
  load(paste0(datapfad_PP_Kammer,"flux_ls.RData"))
  datelim_old <- range(flux_data$date)
  
  if(datelim[2] > datelim_old[2]){
    flux_ls <- chamber_arduino(datelim=c(max(datelim_old),max(datelim)),
                               gga_data = T,
                               return_ls = T,
                               t_init=1,
                               t_min=2,
                               t_max=2,
                               plot = "timeline",
                               gas = c("CO2_GGA","CH4"))
    flux_new <- flux_ls[[1]]
    flux_data_new <- flux_ls[[2]]
    
    flux <- rbind(flux,flux_new)
    flux_data <- rbind(flux_data,flux_data_new)
    
    save(flux,flux_data,file=paste0(datapfad_PP_Kammer,"flux_ls.RData"))
  }
}else{
  flux_ls <- chamber_arduino(datelim=datelim,
                             gga_data = T,
                             return_ls = T,
                             t_init=1,
                             plot="",
                             gas = c("CO2_GGA","CH4"),
                             t_min=2,
                             t_max=2)
  flux <- flux_ls[[1]]
  flux_data <- flux_ls[[2]]
}
# flux_data <- flux_data[!duplicated(flux_data$date),]
# flux <- flux[!duplicated(flux$date),]
rm(flux_ls)
rm(flux_data)
#ggplot(flux_data)+
#  geom_line(aes(date,CO2,col=factor(messid),group=1))+
#  geom_line(aes(date,CO2_GGA+50,col=factor(messid),group=1))
flux_sub <- subset(flux,!is.na(CH4_R2))
data_merge <- merge(flux_sub,data_PPC_wide)

range(data_merge$date)
rm(data_PPC_wide)

##################################
#swc
source("./PP_kammer_hartheim/SWC_hartheim.R")
data_merge <- data_merge[order(data_merge$date),]

swc_sub <- sub_daterange(swc_wide,datelim)
swc_sub <- swc_sub[order(swc_sub$date),]

setDT(swc_sub)
swc_sub[,date:= round_date(date,"30 mins")]
swc_agg <- swc_sub[,lapply(.SD,mean),by=date] %>% as.data.frame()

###########################
#komischen bereich interpolieren
swc_agg$swc_7_roll <- RcppRoll::roll_mean(swc_agg$swc_7,3,fill=NA)
swc_agg$swc_7_diff <- c(NA,diff(swc_agg$swc_7_roll))

swc_ids <- which(abs(swc_agg$swc_7_diff) > 0.25)
timeperiod <- (swc_ids[1]-30):(swc_ids[2]+30)
period_vals <- swc_agg[timeperiod,"swc_7"]

swc_agg$swc_7_cal <- RcppRoll::roll_mean(swc_agg$swc_7,60,fill=NA)
swc_agg$swc_7_cal[timeperiod] <- NA
swc_agg$swc_7_cal <- imputeTS::na_interpolation(swc_agg$swc_7_cal)

data_merge <- merge(data_merge,swc_agg,by="date",all.x = T)
data_merge <- merge(data_merge,klima,all.x = T)

####################
#Co2 flux roll
data_merge$CO2_flux <- RcppRoll::roll_mean(data_merge$CO2_GGA_mumol_per_s_m2,5,fill=NA)
data_merge$CO2_flux2 <- RcppRoll::roll_mean(data_merge$CO2_GGA_mumol_per_s_m2,10,fill=NA)
data_merge$CH4_flux <- RcppRoll::roll_mean(data_merge$CH4_mumol_per_s_m2 * 10^3,5,fill=NA)
data_merge$CH4_flux2 <- RcppRoll::roll_mean(data_merge$CH4_mumol_per_s_m2 * 10^3,10,fill=NA)



####################################################
#fit modells
#####################################################
data_merge$dummy <- rnorm(nrow(data_merge))


############################
#modelle mit swc und T
#CO2
fm_CO2_swc_T <- mgcv::gam(CO2_flux ~ (swc_14) + (T_C),data = data_merge)
fm_CO2_T <- mgcv::gam(CO2_flux ~ (T_C),data = data_merge)
#CH4
fm_CH4_swc_T <- mgcv::gam(CH4_flux ~ (swc_14) + (T_C),data = data_merge)
######################
#modelle mit allen variablen
#CO2

##################
#lasso test
# mm_all <- model.matrix(~CO2_flux + swc_14 + T_C + PPC_2 + PPC_meanr6_2 + P_roll_2 + P_horiz_2 + PPC_outside + PPC_diff_meanr_2 + dummy,data_merge)[,-1]
# 
# lasso <- cv.glmnet(mm_all[,-1],mm_all[,1])
# 
# plot(lasso)
# coef(lasso, s = "lambda.min")
# 
# 
# fm_CO2_all <- step(glm(CO2_flux ~ swc_14 + T_C + PPC_2 + PPC_meanr6_2 + P_roll_2 + P_horiz_2 + PPC_outside + PPC_diff_meanr_2 + dummy,data = data_merge))
# fm_CO2_swc_T_PPC_meanr <- step(glm(CO2_flux ~ swc_14 + T_C + PPC_meanr6_2 + P_roll_2 + PPC_outside + dummy,data = data_merge))
# fm_CO2_swc_T_PPC_meanr2 <- step(glm(CO2_flux ~ swc_14 + T_C + PPC_meanr6_2 + dummy,data = data_merge))
# fm_CO2_swc_T_PPC <- step(glm(CO2_flux ~ swc_14 + T_C + PPC_2 + P_roll_2 + PPC_outside + dummy,data = data_merge))
# fm_CO2_swc_T_P_horiz <- step(glm(CO2_flux ~ swc_14 + T_C + P_horiz_2 + P_roll_2 + PPC_outside + dummy,data = data_merge))
# 
# summary((fm_CO2_all))
# summary((fm_CO2_swc_T_PPC_meanr))
# summary((fm_CO2_swc_T_PPC))
# summary((fm_CO2_swc_T_P_horiz))
# calc_flux()
# comsol_sweep()
# R2_fm(fm_CO2_swc_T)
# R2_fm(fm_CO2_all)
# R2_fm(fm_CO2_swc_T_PPC)
# R2_fm(fm_CO2_swc_T_PPC_meanr)
# R2_fm(fm_CO2_swc_T_PPC_meanr2)
# #CH4
# fm_CH4_all <- step(glm(CH4_flux ~ swc_14 + T_C + PPC_2 + PPC_meanr6_2 + P_roll_2 + P_horiz_2 + PPC_outside + PPC_diff_meanr_2 + dummy,data = data_merge))
# fm_CH4_swc_T_PPC_meanr <- step(glm(CH4_flux ~ swc_14 + T_C + PPC_meanr6_2 + P_roll_2 + PPC_outside + dummy ,data = data_merge))
# fm_CH4_swc_T_PPC <- step(glm(CH4_flux ~ swc_14 + T_C + PPC_2  + P_roll_2 + PPC_outside + dummy,data = data_merge))
# fm_CH4_swc_T_PPC_atm <- step(glm(CH4_flux ~ swc_14 + T_C + PPC_outside + dummy,data = data_merge))
# fm_CH4_swc_T_P_horiz <- step(glm(CH4_flux ~ swc_14 + T_C + P_horiz_2 + P_roll_2 + PPC_outside + dummy,data = data_merge))
# 
# summary((fm_CH4_all))
# summary((fm_CH4_swc_T_PPC_meanr))
# summary((fm_CH4_swc_T_PPC))
# summary((fm_CH4_swc_T_PPC_atm))
# summary((fm_CH4_swc_T_P_horiz))
# 
# R2_fm(fm_CH4_swc_T)
# R2_fm(fm_CH4_all)
# R2_fm(fm_CH4_swc_T_PPC)
# R2_fm(fm_CH4_swc_T_PPC_atm)
# R2_fm(fm_CH4_swc_T_PPC_meanr)
##########################
#predict modells
#CO2
data_merge$CO2_swc_T <- predict(fm_CO2_swc_T,newdata = data_merge)
#data_merge$CO2_T <- predict(fm_CO2_T,newdata = data_merge)
#data_merge$CO2_swc_T_PPC <- predict(fm_CO2_swc_T_PPC,newdata = data_merge)
#data_merge$CO2_swc_T_PPC_meanr <- predict(fm_CO2_swc_T_PPC_meanr,newdata = data_merge)
#data_merge$CO2_all <- predict(fm_CO2_all,newdata = data_merge)
#CH4
data_merge$CH4_swc_T <- predict(fm_CH4_swc_T,newdata = data_merge)
#data_merge$CH4_swc_T_PPC <- predict(fm_CH4_swc_T_PPC,newdata = data_merge)
#data_merge$CH4_all <- predict(fm_CH4_all,newdata = data_merge)
##########
#CO2 und CH4 adj sind die flüsse ohne SWC und T einfluss
data_merge$CO2_adj <- data_merge$CO2_flux - data_merge$CO2_swc_T + mean(data_merge$CO2_flux,na.rm = T)
#data_merge$CO2_adj <- data_merge$CO2_flux - data_merge$CO2_T + mean(data_merge$CO2_flux,na.rm = T)
data_merge$CH4_adj <- data_merge$CH4_flux - data_merge$CH4_swc_T + mean(data_merge$CH4_flux,na.rm = T)



############################
#Modellselektion
CO2_step_fm <- step(fm_CO2_swc_T_PPC)
summary(step_fm)
CH4_step_fm <- step(fm_CH4_swc_T_PPC)
summary(step_fm)
summary(fm_CO2_swc_T_PPC)
summary(fm_CH4_swc_T_PPC)
summary(lm(CO2_adj ~ PPC_sum , data=data_merge))


##################################################
#Versuch spalte und modelle für einzelne Versuch
data_merge$P_lateral <- data_merge$P_roll_1 - data_merge$P_roll_3
data_merge$Versuch <- NA
data_merge$CH4_group_fm <- NA
data_merge$CH4_group_PPC <- NA
data_merge$CO2_group_fm <- NA
data_merge$CO2_group_PPC <- NA

#SChleife durch die Versuche

for(i in 2:nrow(pp_chamber)){
  id_i <- which(daterange_id(data_merge,c(pp_chamber$Start[i] - 3600 * 24,pp_chamber$Ende[i] + 3600 * 24)))
  if(length(id_i)){
    data_merge$Versuch[id_i] <- i
    data_merge$CH4_group_fm[id_i] <- predict(glm(CH4_flux ~ swc_7 + T_C, data = data_merge[id_i,]))
    data_merge$CH4_group_PPC[id_i] <- predict(glm(CH4_flux ~ swc_7 + T_C + PPC_sum, data = data_merge[id_i,]))
    data_merge$CO2_group_fm[id_i] <- predict(glm(CO2_flux ~ swc_7 + T_C, data = data_merge[id_i,]))
    data_merge$CO2_group_PPC[id_i] <- predict(glm(CO2_flux ~ swc_7 + T_C + PPC_sum, data = data_merge[id_i,]))
  }
}#ende schleife
###################
#Modus 1D oder 2D oder andere
data_merge$modus <- factor(data_merge$Versuch,levels = 1:nrow(pp_chamber),labels = pp_chamber$Modus)


################################
# correlation plots
#PerformanceAnalytics::chart.Correlation(data_merge[,c("CO2_flux","T_C","swc_7_cal","swc_7","swc_14","swc_21")])
#PerformanceAnalytics::chart.Correlation(data_merge[,c("CH4_flux","T_C","swc_7_cal","swc_7","swc_14","swc_21")])
names(data_merge)
names(klima)

data_merge <- data_merge %>% 
  mutate(T_max = RcppRoll::roll_maxr(T_C,2 * 24,fill=NA),
         T_min = RcppRoll::roll_minr(T_C,2 * 24,fill=NA),
         dT_C = T_max - T_min
  )

PerformanceAnalytics::chart.Correlation(data_merge[,c("T_C","swc_7_cal","P_hPa","Wind_ms","Precip_1hr_mm","Precip_24Tot","CO2_flux")])

PerformanceAnalytics::chart.Correlation(data_merge[,c("T_C","swc_7_cal","P_roll_2","P_lateral","PPC_outside","PPC_2","PPC_meanr3_2","CO2_flux")])
PerformanceAnalytics::chart.Correlation(data_merge[,c("T_C","swc_7_cal","P_roll_2","P_lateral","PPC_outside","PPC_2","PPC_meanr3_2","CH4_flux")])
##################
#plot Spielwiese
Versuch_x <-  c(25,10)

ggplot(subset(data_merge, Versuch %in% Versuch_x))+
  geom_line(aes(date, CO2_flux))+
  geom_line(aes(date, CO2_adj,col="adj"))
  
ggplot(subset(data_merge, Versuch %in% Versuch_x),aes(P_roll_2, CH4_flux))+
  geom_smooth(method = "glm")+
  geom_point()+
  ggpubr::stat_regline_equation(label.x.npc = 0.5,label.y.npc =0.2,aes(label = paste(..eq.label..,..rr.label..,sep = "~")))+
  #  facet_wrap(~cut(T_C,4))+
  scale_color_viridis_d()

ggplot(subset(data_merge, Versuch %in% Versuch_x),aes(P_roll_2, CO2_flux, col=cut(T_C,4)))+
  geom_smooth(method = "glm")+
  geom_point()+
  ggpubr::stat_regline_equation(label.x.npc = 0.5,label.y.npc =0.2,aes(label = paste(..eq.label..,..rr.label..,sep = "~")))+
  #  facet_wrap(~cut(T_C,4))+
  scale_color_viridis_d()
unique(data_merge$modus)
data_sub_PPC <- subset(data_merge,modus %in% c("2D PP","1D PP","2D, 1D, 2D, 1D"))
data_sub_P <- data_merge[grepl("Unterdruck",data_merge$modus),]
ggplot(subset(data_sub_PPC),aes(PPC_2, CO2_flux, col=swc_7_cal))+
  geom_smooth(method = "glm")+
  geom_point()+
  facet_wrap(~cut(T_C,4))+
  ggpubr::stat_regline_equation(label.x.npc = 0.5,label.y.npc =0.2,aes(label = paste(..eq.label..,..rr.label..,sep = "~")))#+
ggplot(subset(data_sub_P),aes(P_roll_2, CO2_flux, col=factor(Versuch)))+
  geom_smooth(method = "glm")+
  geom_point()+
  #facet_wrap(~cut(T_C,4))+
  ggpubr::stat_regline_equation(label.x.npc = 0.5,label.y.npc =0.2,aes(label = paste(..eq.label..,..rr.label..,sep = "~")))#+
  #scale_color_brewer(palette = "RdYlBu",direction = -1)

ggplot(subset(data_merge,PPC_2 > 0.00 & !is.na(swc_7_cal)),aes(P_roll_2, CH4_flux, col=cut(swc_7_cal,4)))+
  geom_smooth(method = "glm")+
  geom_point()+
  ggpubr::stat_regline_equation(label.x.npc = 0.45,label.y.npc =0.2,aes(label = paste(..eq.label..,..rr.label..,sep = "~")))+
  #  facet_wrap(~cut(T_C,4))+
  scale_color_brewer(palette = "RdYlBu")


ggplot(subset(data_merge, Versuch %in% Versuch_x))+
  geom_point(aes(P_roll_2, CO2_flux, col=T_C))+
  facet_wrap(~cut(T_C,4))+
  scale_color_viridis_c()

ggplot(subset(data_merge, Versuch %in% Versuch_x))+
  geom_point(aes(P_roll_2, CH4_flux, col=T_C))+
  scale_color_viridis_c()
#  facet_wrap(~Versuch)
# ggplot(subset(data_merge,Versuch %in% c(2:12)))+
#   geom_line(aes(date,PPC_2*4,col="PPC"))+
#   geom_line(aes(date,P_horiz_2,col="P_horiz"))+
#   facet_wrap(~paste(Versuch,modus),scales="free_x")

ggplot(subset(data_merge,Versuch %in% c(2:18)))+
  geom_line(aes(date,CO2_flux2,col="chamber_flux"),lwd=1)+
  #geom_line(aes(date,CO2_group_fm,col="swc_T"))+
  #geom_line(aes(date,CO2_group_PPC,col="swc_T_PPC"))+
  geom_line(aes(date,CO2_swc_T_PPC,col="swc_T_PPC"))+
  geom_line(aes(date,CO2_swc_T,col="swc_T"))+
  geom_line(aes(date,PPC_2+1,col="PPC_2"))+
  geom_line(aes(date,PPC_outside+1,col="PPC_outside"))+
  # geom_line(aes(date,PPC_sum+1,col="PPC_sum"))+
  geom_line(aes(date,T_C/10,col="T_C"))+
  facet_wrap(~Versuch,scales="free_x")
ggplot(subset(data_merge,Versuch %in% c(2:15)))+
  geom_line(aes(date,CH4_flux2,col="chamber_flux"),lwd=1)+
  #geom_line(aes(date,CH4_group_fm,col="swc_T"))+
  #geom_line(aes(date,CH4_group_PPC,col="swc_T_PPC"))+
  #geom_line(aes(date,CH4_swc_T_PPC,col="swc_T_PPC"))+
  #geom_line(aes(date,CH4_swc_T,col="swc_T"))+
  geom_line(aes(date,PPC_2-1,col="PPC_2"))+
  geom_line(aes(date,PPC_outside-1,col="PPC_outside"))+
  #  geom_line(aes(date,PPC_sum-1,col="PPC_sum"))+
  geom_line(aes(date,T_C/20-2,col="T_C"))+
  facet_wrap(~Versuch,scales="free_x")

ggplot(data_merge)+
  geom_point(aes(T_C,CH4_flux,col=swc_14))+
  facet_wrap(~cut(swc_14,4))+
  scale_color_viridis_c()
# ggplot(data_merge)+
#   geom_line(aes(date, PPC_diff_meanr_2*-10000 -1,col="diff"))+
#   geom_line(aes(date, CH4_adj,col="CH4_flux"))+
#   facet_wrap(~Versuch,scales = "free_x")

ggplot(data_merge,aes(PPC_meanr6_2,CH4_adj,col=T_C))+
  geom_point()+
  geom_smooth(method = "glm")+
  scale_color_viridis_c()+
  facet_wrap(~cut(swc_14,5))

#  facet_wrap(~Versuch,scales = "free")
ggplot(data_merge)+
  geom_point(aes(PPC_diff_meanr_2,CH4_flux,col=T_C))
ggplot(data_merge)+
  geom_point(aes(PPC_outside,CH4_flux,col=PPC_2))+
  scale_color_viridis_c()#+
#facet_wrap(~cut(T_C,6))
#geom_point(aes(PPC_outside,CH4_adj,col="adj"),alpha=0.4)
ggplot(data_merge)+
  geom_point(aes(PPC_2,CH4_flux,col="raw"))+
  geom_point(aes(PPC_2,CH4_adj,col="adj"),alpha=0.4)
ggplot(data_merge)+
  geom_point(aes(PPC_sum,CH4_flux,col="raw"))+
  geom_point(aes(PPC_sum,CH4_adj,col="adj"),alpha=0.4)

ggplot(data_merge)+
  geom_line(aes(date,CH4_flux,col = "CH4_flux"))+
  geom_line(aes(date,PPC_2-1))+
  facet_wrap(~Versuch,scales="free_x")
ggplot(data_merge)+
  geom_point(aes(PPC_outside,CO2_flux,col="raw"))+
  geom_point(aes(PPC_outside,CO2_adj,col="adj"),alpha=0.4)
ggplot(data_merge)+
  geom_point(aes(CH4_flux,PPC_2))
names(data_merge)
Versuch <- 4

names(data_merge)


ggplot(subset(data_merge,Versuch %in% c(2:7)))+
  geom_line(aes(date,CH4_flux,col="chamber_flux"))+
  geom_line(aes(date,CH4_swc_T_PPC,col="swc_T_PPC"))+
  geom_line(aes(date,CH4_swc_T,col="swc_T"))+
  geom_line(aes(date,T_C/-20,col="T_C"))+
  geom_line(aes(date,PPC_2-1,col="PPC_2"))+
  geom_line(aes(date,PPC_sum-1,col="PPC_sum"))+
  facet_wrap(~Versuch,scales="free_x")



ggplot(data_merge)+
  geom_point(aes(PPC_sum, CO2_flux,col="raw"))+
  geom_point(aes(PPC_sum, CO2_adj,col="adj"))




ggplot(data_merge)+
  geom_line(aes(date,CH4_flux))+
  geom_line(aes(date,CH4_swc_T_PPC,col="swc_T_PPC"))+
  geom_line(aes(date,CH4_swc_T,col="swc_T"))+
  geom_line(aes(date,PPC_sum -1,col="PPC"))+
  xlim(ymd_h("22.10.07 10","22.10.12 10"))

ggplot(data_merge)+
  #geom_point(aes(PPC_sum, CH4_flux))
  geom_point(aes(PPC_sum, CH4_flux,col="raw"))+
  geom_point(aes(PPC_sum, CH4_adj,alpha=0.2,col="adj"))

##############################################################
#          TIMELINE PLOTS
##############################################################
Versuch_sel <- c(14:18)
CO2_GGA_flux <- ggplot(subset(data_merge, Versuch %in% Versuch_sel))+
  geom_point(aes(date,CO2_GGA_mumol_per_s_m2,col="CO2"))+
  geom_line(aes(date,CO2_flux,col="CO2"))+
  guides(col=F)+
  labs(x="",y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="")

CH4_GGA_flux <- ggplot(subset(data_merge, Versuch %in% Versuch_sel))+
  geom_point(aes(date,CH4_mumol_per_s_m2*10^3,col="CH4"))+
  geom_line(aes(date,(CH4_flux),col="CH4"))+
  labs(x="",y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),col="")+
  guides(col=F)+
  scale_color_manual(values = scales::hue_pal()(2)[2])

PPC_sum_plot <- ggplot(subset(data_merge, Versuch %in% Versuch_sel))+
  geom_line(aes(date,PPC_2,col="artificial"))+
  geom_line(aes(date,PPC_outside,col="natural"))+
  geom_line(aes(date,PPC_sum,col="sum"))+
  scale_color_manual(values=c(2,4,1))+
  theme(legend.position = "top")+
  labs(x = "",y="PPC (Pa/s)",col="")
P_plot <- ggplot(subset(data_merge, Versuch %in% Versuch_sel))+
  geom_line(aes(date,P_roll_2,col="2"))+
  geom_line(aes(date,P_roll_3,col="3"))+
  geom_line(aes(date,P_lateral,col="lateral"))+
  scale_color_manual(values=c(2,4,1))+
  theme(legend.position = "top")+
  labs(x = "",y="PPC (Pa/s)",col="")

T_plot <- 
  ggplot(subset(data_merge, Versuch %in% Versuch_sel & !is.na(T_C)))+
  geom_line(aes(date,RcppRoll::roll_mean(T_C,10,fill=NA)))+
  labs(y=expression(T["atm"]~"(°C)"))+
  xlim(range(subset(data_merge, Versuch %in% Versuch_sel)$date))

swc_plot <- ggplot(subset(data_merge, Versuch %in% Versuch_sel))+
  geom_line(aes(date,swc_7,col="7"))+
  geom_line(aes(date,swc_14,col="14"))+
  geom_line(aes(date,swc_21,col="21"))+
  labs(x="",y="SWC (Vol. %)",col="tiefe (cm)")

Precip_plot <- ggplot(subset(data_merge, Versuch %in% Versuch_sel))+
  geom_line(aes(date,Precip_24Tot))
PhPa_plot <- ggplot(subset(data_merge, Versuch %in% Versuch_sel))+
  geom_line(aes(date,P_hPa))

timelines <- ggpubr::ggarrange(CO2_GGA_flux,
                               CH4_GGA_flux,
                               PPC_sum_plot+guides(col=F),
                               P_plot+guides(col=F),
                               T_plot,
                               #Precip_plot,
                               #PhPa_plot,
                               #swc_plot+guides(col=F),
                               ncol=1,align = "v")
timelines
summary(glm(CO2_flux ~ PPC_sum + T_C,data=data_merge))
summary(glm(CH4_flux ~ PPC_sum + T_C,data=data_merge))



##########################################
#      SCATTER PLOTS
######################################
names(data_merge)
names(data_merge)

ggplot(subset(data_merge),aes(PPC_2,CO2_flux,col=T_C))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point()+
  ggpubr::stat_regline_equation(aes(label= paste(..eq.label..,..rr.label..,sep="~~")))+
  #facet_wrap(~cut(T_C,breaks = 4))+
  scale_color_viridis_c()
#CO2_scatter <- 
ggplot(subset(data_merge,PPC_2 > 0.1),aes(PPC_2,CO2_flux,col=factor(Versuch)))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point()+
  #ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  #scale_color_viridis_d()+
  facet_wrap(~cut(T_C,breaks = 6))
#  labs(y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),x= expression(PPC[sum]~"(Pa/s)"))

#CH4_scatter <- 
ggplot(subset(data_merge,PPC_2 > 0.1 & !is.na(swc_7_cal)),aes(P_roll_2,CH4_adj,col=(T_C)))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point()+
  ggpubr::stat_regline_equation(label.y=-0.8,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y=-0.85,aes(label= ..rr.label..))+
  scale_color_viridis_c()
#  facet_wrap(~cut(swc_7_cal,breaks = 6))
ggplot(subset(data_merge),aes(P_roll_2,CH4_flux,col=factor(Versuch)))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point()+
  ggpubr::stat_regline_equation(label.y=-0.8,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y=-0.85,aes(label= ..rr.label..))+
  #scale_color_viridis_c()+
  facet_wrap(~cut(swc_7_cal,breaks = 6))

ggplot(subset(data_merge),aes(P_roll_2,CO2_flux,col=factor(Versuch)))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point()+
  ggpubr::stat_regline_equation(aes(label= ..rr.label..))+
  #scale_color_viridis_c()+
  facet_wrap(~cut(swc_7_cal,breaks = 6))
#  labs(y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),x= expression(PPC[sum]~"(Pa/s)"))

ggplot(data_merge,aes(PPC_2,CH4_flux,col=T_C))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point()+
  ggpubr::stat_regline_equation(label.y=-0.8,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y=-0.85,aes(label= ..rr.label..))+
  scale_color_viridis_c()+
  facet_wrap(~cut(swc_14,breaks = 6))+
  labs(y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),x= expression(PPC[sum]~"(Pa/s)"))

CO2_T <- ggplot(data_merge,aes(T_C,CO2_flux))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point(col=scales::hue_pal()(2)[1])+
  ggpubr::stat_regline_equation(label.y = 3.49,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y = 3.4,aes(label= ..rr.label..))+
  labs(y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),x= "T (°C)")


CH4_T <- 
  ggplot(data_merge,aes(T_C,CH4_flux))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point(col=scales::hue_pal()(2)[2])+
  ggpubr::stat_regline_equation(label.y=-0.8,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y=-0.85,aes(label= ..rr.label..))+
  labs(y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),x= "T (°C)")

CO2_swc_7 <- ggplot(data_merge,aes(swc_7,CO2_flux))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point(col=scales::hue_pal()(2)[1])+
  ggpubr::stat_regline_equation(label.y = 3.49,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y = 3.4,aes(label= ..rr.label..))+
  labs(y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),x= "swc 7 cm (Vol. %)")
CO2_swc_14 <- ggplot(data_merge,aes(swc_14,CO2_flux))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point(col=scales::hue_pal()(2)[1])+
  ggpubr::stat_regline_equation(label.y = 3.49,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y = 3.4,aes(label= ..rr.label..))+
  labs(y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),x= "swc 14 cm (Vol. %)")

CH4_swc_7 <- ggplot(data_merge,aes(swc_7_cal,CH4_flux))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point(col=scales::hue_pal()(2)[2])+
  ggpubr::stat_regline_equation(label.y = -0.8,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y = -0.85,aes(label= ..rr.label..))+
  labs(y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),x= "swc 7 cm (Vol. %)")
CH4_swc_14 <- ggplot(data_merge,aes(swc_14,CH4_flux,col=as.numeric(date)))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point()+
  #geom_point(col=scales::hue_pal()(2)[2])+
  ggpubr::stat_regline_equation(label.y = -0.8,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y = -0.85,aes(label= ..rr.label..))+
  labs(y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),x= "swc 14 cm (Vol. %)")
CH4_swc_21 <- ggplot(data_merge,aes(swc_21,CH4_flux))+
  geom_smooth(method="glm",linetype=2,col=1,lwd=0.7)+
  geom_point(col=scales::hue_pal()(2)[2])+
  ggpubr::stat_regline_equation(label.y = -0.8,aes(label= ..eq.label..))+
  ggpubr::stat_regline_equation(label.y = -0.85,aes(label= ..rr.label..))+
  labs(y=expression(italic(F[CH4])~"("*n * mol ~ m^{-2} ~ s^{-1}*")"),x= "swc 14 cm (Vol. %)")
#ggpubr::ggarrange(CO2_scatter,CH4_scatter)

ggplot(data_merge)+
  geom_line(aes(date,swc_7_cal))+
  geom_line(aes(date,CH4_flux + 20))
ggplot(data_merge,aes(date,swc_14))+
  geom_point()

##########################
#export Plots
ggpubr::ggarrange(CO2_scatter,CO2_T,CO2_swc_7,CH4_scatter,CH4_T,CH4_swc_7)+
  ggsave(paste0(plotpfad_PPchamber,"flux_scatterplots.png"),width = 9,height = 8)

png(paste0(plotpfad_PPchamber,"GGA_PPC_effect.png"),width=7,height = 7,unit="in",res=300)
grid.arrange(timelines,
             CO2_scatter+theme(axis.title = element_blank()),
             CH4_scatter+theme(axis.title.y = element_blank()),
             layout_matrix = rbind(c(1,1,4),
                                   c(1,1,5),
                                   c(1,1,NA)))
dev.off()
