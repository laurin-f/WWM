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
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS","data.table","glmnet")
check.packages(packages)
theme_set(theme_classic())


##################
#Metadata
pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen_hartheim.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

Versuch <- 3
datelim <- c(pp_chamber$Start[Versuch],pp_chamber$Ende[Versuch])

plot <-  T
load <- T
#datelim <- c(ymd_h("2022-04-13 18"),ymd_h("2022-04-25 18"))
#datelim <- ymd_hm("2022.05.02 00:00","2022.05.02 01:20")
#datelim <- ymd_hm("2022.05.08 18:00","2022.05.10 13:20")
#datelim <- ymd_hm("2022.05.12 10:00","2022.05.16 16:00")



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
#ggplot(flux_data)+
#  geom_line(aes(date,CO2,col=factor(messid),group=1))+
#  geom_line(aes(date,CO2_GGA+50,col=factor(messid),group=1))
flux_sub <- subset(flux,!is.na(CH4_R2))
data_merge <- merge(flux_sub,data_PPC_wide)

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


data_merge <- merge(data_merge,swc_agg,by="date")
####################
#Co2 flux roll
data_merge$CO2_flux <- RcppRoll::roll_mean(data_merge$CO2_GGA_mumol_per_s_m2,5,fill=NA)
data_merge$CO2_flux2 <- RcppRoll::roll_mean(data_merge$CO2_GGA_mumol_per_s_m2,10,fill=NA)
data_merge$CH4_flux <- RcppRoll::roll_mean(data_merge$CH4_mumol_per_s_m2 * 10^3,5,fill=NA)
data_merge$CH4_flux2 <- RcppRoll::roll_mean(data_merge$CH4_mumol_per_s_m2 * 10^3,10,fill=NA)


FCO2_plt <- ggplot(data_merge)+
  geom_line(aes(date,CO2_flux))
FCH4_plt <- ggplot(data_merge)+
  geom_line(aes(date,CH4_flux))
P_roll_plot <- ggplot(data_merge)+
  geom_line(aes(date,P_1 - P_3))
T_plot <- ggplot(data_merge)+
  geom_line(aes(date,T_C))
swc_plot <- ggplot(data_merge)+
  geom_line(aes(date,swc_14))

ggpubr::ggarrange(FCO2_plt,P_roll_plot,T_plot,ncol=1)
ggpubr::ggarrange(FCH4_plt,P_roll_plot,T_plot,swc_plot,ncol=1)
