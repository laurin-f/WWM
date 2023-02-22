hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Kammermessungen_Arduino"
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS","data.table")
check.packages(packages)

#############################
#flux

##################
#Metadata
pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen_hartheim.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

injections <- read_ods(paste0(metapfad_PP,"injektionen_hartheim.ods"))
injections$Start <- dmy_hm(injections$Start)
injections$Ende <- dmy_hm(injections$Ende)

#Versuch <- nrow(pp_chamber)
#Versuch <- 9
#for(Versuch in 20:nrow(pp_chamber)){
#datelim <- c(pp_chamber$Start[Versuch]-3600*24*0.5,pp_chamber$Ende[Versuch]+3600*24*0.5)
plot <-  T
load <-  T
# if(is.na(datelim[2])){
#   datelim[2] <- now()
# }

#i <- 4
p <- list()
#for(i in 4:12){
now <- now()
tz(now) <- "UTC"
datelim <- c(ymd_h("22.09.27 10"),now)
datelim <- c(ymd_h("23.02.16 10","23.02.22 10"))



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
                               gas = c("CO2_GGA","CH4"))
    flux_new <- flux_ls[[1]]
    flux_data_new <- flux_ls[[2]]
    # flux <- flux_ls[[1]]
    # flux_data <- flux_ls[[2]]
    
    flux <- rbind(flux,flux_new)
    flux_data <- rbind(flux_data,flux_data_new)
    
    save(flux,flux_data,file=paste0(datapfad_PP_Kammer,"flux_ls.RData"))
  }
}else{
  flux_ls <- chamber_arduino(datelim=datelim,
                             gga_data = T,
                             return_ls = T,
                             t_init=1,
                             t_min=2,
                             t_max=2,
                             gas = c("CO2_GGA","CH4"),
                             plot = "facets")
  flux <- flux_ls[[1]]
  flux_data <- flux_ls[[2]]
  
}
names(flux) <- str_remove(names(flux),"_GGA")
#rm(flux_data)
#datelim <- c(pp_chamber$Start[i]-3600*10,pp_chamber$Ende[i]+3600*10)
#datelim <- c(ymd_h("2022-04-13 18"),now())

data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "wide")
data_long <- read_sampler("sampler1u2",datelim = datelim, format = "long")
data_PPC <- read_PP(datelim = datelim,table.name = "PP_1min",format = "wide")
for(i in 1:6){
  data_PPC[,paste0("P_roll_",i)] <- RcppRoll::roll_mean(data_PPC[,paste0("P_",i)],3*60,fill=NA)
}
names(data_PPC)

data_probe1u2$Versuch <- NA
data_long$Versuch <- NA

for(i in 1:nrow(pp_chamber)){
  id_wide <- which(daterange_id(data_probe1u2,c(pp_chamber$Start[i] - 3600 * 10,pp_chamber$Ende[i] + 3600 * 24)))
  id_long <- which(daterange_id(data_long,c(pp_chamber$Start[i] - 3600 * 10,pp_chamber$Ende[i] + 3600 * 24)))
  data_probe1u2$Versuch[id_wide] <- i
  data_long$Versuch[id_long] <- i
}

data_long <- data_long %>% 
  group_by(tiefe) %>% 
  mutate(CO2_smp2 = RcppRoll::roll_mean(CO2_smp2,10,fill=NA))


data_30min <- data_probe1u2 %>% 
  select(matches("date|CO2_tiefe\\d_smp2|Versuch")) %>% 
  mutate(date = round_date(date,"30 mins")) %>% 
  group_by(date) %>% 
  summarise(across(everything(),mean,na.rm=T)) %>% 
  mutate(Versuch = round(Versuch)) %>% 
  rename_with(.cols=matches("CO2_tiefe\\d_smp2"),~str_remove(.,"_smp2"))

DT <- copy(flux_data)
setDT(DT)



DT[,chamber_buffer := RcppRoll::roll_maxr(chamber,40,fill = NA)]

CO2_atm <- DT[chamber_buffer == 0,date := round_date(date, "30 mins")][,.(CO2_tiefe0 = mean(CO2_GGA)) ,by = date]

#flux_data <- as.data.frame(flux_data)
CO2_atm <- as.data.frame(CO2_atm)


data <- merge(data_30min,flux[,c("date","T_C","CO2_mumol_per_s_m2","CH4_mumol_per_s_m2")],by = "date",all=T)
data <- merge(data,CO2_atm)
data <- merge(data,data_PPC)
names(data)
#######################
#CO2 calm: die Zeitraume mit artificial PPC  werden rausgeschnitten und linear interpoliert 
for(i in grep("CO2_tiefe",names(data),value = T)){
  data[,paste0(i,"_calm")] <- data[,i]
  data[which(data$PPC_2 > 0.1),paste0(i,"_calm")] <- NA
  data[,paste0(i,"_calm")] <- imputeTS::na_interpolation(data[,paste0(i,"_calm")])
  data[,paste0(i,"_PPC")] <- data[,i] - data[,paste0(i,"_calm")]
}

ggplot(data)+
  #geom_line(aes(date, CO2_tiefe6_PPC,col=""))
  geom_line(aes(date, CO2_tiefe4,col="CO2"))+
  geom_line(aes(date, CO2_tiefe4_calm,col="calm"))+
  geom_line(aes(date, PPC_2*1000+300,col="calm"))+
  facet_wrap(~Versuch,scales="free_x")
#swc
source("./PP_kammer_hartheim/SWC_hartheim.R")

swc_sub <- sub_daterange(swc_wide,datelim)
setDT(swc_sub)
swc_sub[,date:= round_date(date,"30 mins")]
swc_sub <- swc_sub[order(date)]
swc_agg <- swc_sub[,lapply(.SD,mean),by=date] %>% as.data.frame()
swc_agg$swc_7_roll <- RcppRoll::roll_mean(swc_agg$swc_7,3,fill=NA)
swc_agg$swc_7_diff <- c(NA,diff(swc_agg$swc_7_roll))

swc_ids <- which(abs(swc_agg$swc_7_diff) > 0.25)
timeperiod <- (swc_ids[1]-30):(swc_ids[2]+30)
period_vals <- swc_agg[timeperiod,"swc_7"]

swc_agg$swc_7_cal <- RcppRoll::roll_mean(swc_agg$swc_7,60,fill=NA)
swc_agg$swc_7_cal[timeperiod] <- NA
swc_agg$swc_7_cal <- imputeTS::na_interpolation(swc_agg$swc_7_cal)

#swc_agg[swc_ids[1]:swc_ids[2],"swc_7_cal"] <- swc_agg[swc_ids[1]:swc_ids[2],"swc_7"] - mean(timeperiod - ,na.rm =T)
ggplot(swc_agg)+
  geom_line(aes(date,swc_21))
#swc_agg[swc_ids[1]:swc_ids[2],"swc_7_cal"] <- swc_agg[swc_ids[1]:swc_ids[2],"swc_7"] - mean(timeperiod - ,na.rm =T)
ggplot(swc_agg)+
  geom_line(aes(date,swc_7_cal))
ggplot(swc_agg)+
  geom_vline(xintercept = swc_agg$date[swc_ids],col="grey")+
  geom_hline(yintercept = 0.25)+
  geom_line(aes(date,abs(swc_7_diff)))

data <- merge(data,swc_agg,all.x =T)

data$T_C <- imputeTS::na_interpolation(data$T_C)



data$CO2_flux <- RcppRoll::roll_mean(data$CO2_mumol_per_s_m2,10,fill=NA,na.rm = T)
data$CH4_flux <- RcppRoll::roll_mean(data$CH4_mumol_per_s_m2*10^3,10,fill=NA,na.rm = T)
names(data)
data <- data %>% 
  mutate(across(matches("CO2_tiefe\\d"),~RcppRoll::roll_mean(.,10,fill = NA)),
    across(matches("CO2_tiefe\\d"),~ppm_to_mol(.,T_C = data$T_C),.names = "{.col}_mol"),
         across(matches("CO2_tiefe\\d") & where(~length(which(!is.na(.)))>1),imputeTS::na_interpolation,maxgap=5))

data$modus <- factor(data$Versuch,levels = 1:nrow(pp_chamber),labels = pp_chamber$Modus)

data$dC_0_10 <- rowMeans(cbind(data$CO2_tiefe0_mol - data$CO2_tiefe1_mol,data$CO2_tiefe1_mol - data$CO2_tiefe2_mol,data$CO2_tiefe2_mol - data$CO2_tiefe3_mol)/-0.035)#mol/m4
#Ficks Law
#Fz = DS * dC/dz -> DS = Fz / (dC/dz)
data$DS <- data$CO2_flux * 10^-6 / data$dC_0_10 #mol/s/m2 / (mol/m4) = m2/s
data$D0_m2_s <- D0_T_p(data$T_C,unit = "m2/s")
data$DSD0 <- data$DS / data$D0_m2_s


P_step_thr <- 0.5
names(data_PPC)

data_PPC <- data_PPC[order(data_PPC$date),]

PPC_steps <- data_PPC %>%
  mutate(date = round_date(date,"10 min")) %>%
  group_by(date) %>%
  summarise(across(everything(),mean)) %>%
  mutate(P_diff = abs(c(NA,diff(P_2))),
         P_diff_3 = abs(c(NA,diff(P_3))),
         P_step = ifelse(P_diff > P_step_thr | P_diff_3 > P_step_thr,1,0)
  )
ggplot(PPC_steps)+
  
  geom_line(aes(date,P_diff))

P_step_date <- sort(unique(PPC_steps$date[PPC_steps$P_step == 1]))
P_step_date <- P_step_date[c(as.numeric(diff(P_step_date)),100) > 100]
P_step_date <- P_step_date[!is.na(P_step_date)]

p[[paste0("CO2_")]] <-ggplot(subset(data_long,tiefe > -12))+
  geom_vline(xintercept = P_step_date,col="grey",linetype = 2)+
  #  geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  #geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  #geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe),linetype="probe 2"))+
  geom_line(aes(date,CO2_smp2,col=as.factor(-tiefe)))+
  scale_fill_manual(values = "black")+
  coord_cartesian(xlim=datelim)+
  guides(fill=F)+
  labs(y = expression(CO[2]~"(ppm)"),fill="",col="tiefe")+
  coord_cartesian(xlim=datelim)


p[[paste0("flux_")]] <- ggplot(data)+
  #geom_point(aes(date,CO2_mumol_per_s_m2,col="GGA"))+
  geom_vline(xintercept = P_step_date,col="grey",linetype = 2)+
  geom_line(aes(date,CO2_flux,col="GGA"))+
  # geom_rect(data=pp_chamber,aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  #geom_rect(data=pp_chamber[i,],aes(xmin=Start,xmax=Ende,ymin=-Inf,ymax=Inf,fill="PP_chamber"),alpha=0.1)+
  labs(x="",y=expression(italic(F[CO2])~"("*mu * mol ~ m^{-2} ~ s^{-1}*")"),col="")+
  scale_fill_grey()+
  guides(fill  = F)+
  coord_cartesian(xlim=datelim)
names(data_PPC)
p[[paste0("PPC_")]] <- ggplot(data)+
  geom_line(aes(date, PPC_2, col="PPC_2"))+
  geom_line(aes(date, PPC_6, col="PPC_atm"))+
  coord_cartesian(xlim=datelim)
p[[paste0("P")]] <- ggplot(data)+
  geom_vline(xintercept = P_step_date,col="grey",linetype = 2)+
  geom_line(aes(date, P_roll_2, col="P_2"))+
  geom_line(aes(date, P_roll_3, col="P_3"))+
  coord_cartesian(xlim=datelim)
p[[paste0("DSD0_")]] <- 
  ggplot(data)+
  geom_vline(xintercept = P_step_date,col="grey",linetype = 2)+
  geom_line(aes(date,DSD0))+
  coord_cartesian(xlim=datelim)
#p[[paste0("dC_0_10")]] <- 
  ggplot(data)+
  geom_vline(xintercept = P_step_date,col="grey",linetype = 2)+
  geom_line(aes(date,dC_0_10))+
  coord_cartesian(xlim=datelim)
#}


egg::ggarrange(plots=p[-3],ncol=1)


ggplot(data)+
  geom_point(aes(dC_0_10,DSD0,col=CO2_flux))
ggplot(data)+
  geom_point(aes(CO2_flux,DSD0,col=dC_0_10))
ggplot(data)+
  geom_point(aes(PPC_6,dC_0_10,col=swc_14))
ggplot(data)+
  geom_point(aes(PPC_2,dC_0_10,col=T_C))+
  facet_wrap(~cut(swc_14,4))+
  scale_color_viridis_c()
ggplot(data)+
  geom_point(aes(T_C,DSD0,col=swc_14))
ggplot(data)+
  geom_point(aes(swc_14,DSD0,col=T_C))
names(data)
ggplot(data)+
  geom_line(aes(date,dC_0_10))+
  geom_line(aes(date,PPC_2+0.5))+
  facet_wrap(~paste(Versuch,modus),scales = "free_x")


names(data)
data_select <- data[,c("DSD0","PPC_2","PPC_6","T_C","swc_7","swc_14","CO2_flux","CH4_flux","P_roll_2")]
M <- cor(data_select)
#corrplot::corrplot.mixed(M)
PerformanceAnalytics::chart.Correlation(data_select)


