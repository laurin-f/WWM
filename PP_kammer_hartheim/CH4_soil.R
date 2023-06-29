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
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)
theme_set(theme_classic())

##################
#Metadata
bemerkungen <- read_ods(paste0(metapfad_PP,"PP_Kammer_Bemerkungen_hartheim.ods"))
bemerkungen$Start <- dmy_hm(bemerkungen$Start)
bemerkungen$Ende <- dmy_hm(bemerkungen$Ende)


pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen_hartheim.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

injections <- read_ods(paste0(metapfad_PP,"injektionen_hartheim.ods"))
injections$Start <- dmy_hm(injections$Start)
injections$Ende <- dmy_hm(injections$Ende)

Versuch <- nrow(pp_chamber)
Versuch <- 29
#for(Versuch in 1:nrow(pp_chamber)){
datelim <- c(pp_chamber$Start[Versuch]-3600*24*0.5,pp_chamber$Ende[Versuch]+3600*24*0.5)

datelim[2] <- ymd_h("23.04.12 14")


#############
#klima_data

load(paste0(klimapfad_CR1000,"klima_data_PP_kammer.RData"))
klima <- sub_daterange(klima,datelim)
names(klima)
###############
#load PPC

data_PPC <- read_PP(datelim = datelim,table="PP_1min")

data_PPC <- subset(data_PPC,id %in% c(1:6))
dt <- round(median(diff_time(data_PPC$date[data_PPC$id == 1]),na.rm=T),2)

data_PPC <- data_PPC %>% 
  group_by(id) %>%
  mutate(dt = diff_time(date,"secs"),
         PPC5 = RcppRoll::roll_mean(P_diff,10*60/!!dt,fill=NA),
         #P_roll = RcppRoll::roll_mean(P,3*60/!!dt,fill=NA),
         P_roll = RcppRoll::roll_mean(P,30,fill=NA),
  )

data_PPC$id[data_PPC$id == 6] <- "outside"

data_PPC[which(data_PPC$dt > 3600),c("PPC","PPC5","P_roll")] <- NA
names(data_PPC)
data_Proll_wide <- tidyr::pivot_wider(data_PPC,date,values_from = P_roll,names_from = id,names_prefix = "id_")
data_Proll_wide[paste0("id_",1:4)] <- data_Proll_wide[paste0("id_",1:4)] - data_Proll_wide$id_5
data_Proll <- tidyr::pivot_longer(data_Proll_wide[,1:6],paste0("id_",1:5),names_prefix = "id_",names_to = "id",values_to="P_roll")

step_thr <- 0.03
P_step_thr <- 0.5
data_PPC <- data_PPC[order(data_PPC$date),]
PPC_steps <- data_PPC %>%
  filter(id %in% 1:4) %>%
  mutate(date = round_date(date,"10 min")) %>%
  group_by(id,date) %>%
  summarise(across(everything(),mean)) %>%
  mutate(PPC_diff = abs(c(NA,diff(PPC5))),
         P_diff = abs(c(NA,diff(P_roll))),
         step = ifelse(PPC_diff > step_thr,1,0),
         P_step = ifelse(P_diff > P_step_thr,1,0)
  )


# step_date <- sort(unique(PPC_steps$date[PPC_steps$step == 1]))
# step_date <- step_date[c(as.numeric(diff(step_date)),100) > 60]
# step_date <- step_date[!is.na(step_date)]

P_step_date <- sort(unique(PPC_steps$date[PPC_steps$P_step == 1]))
P_step_date <- P_step_date[c(as.numeric(diff(P_step_date)),100) > 60]
P_step_date <- P_step_date[!is.na(P_step_date)]



step_df <- PPC_steps %>% 
  mutate(step = ifelse(date %in% !!P_step_date,1,0),
         step_id=cumsum(step)) %>% 
  group_by(step_id) %>% 
  summarise(PPC = mean(PPC,na.rm=T),
            Start = min(date),
            End = max(date))


#################
#probe 1u2
data_probe1u2 <- read_sampler("sampler1u2",datelim = datelim, format = "long")

data_probe1u2 <- data_probe1u2 %>% 
  group_by(tiefe) %>% 
  mutate(CO2_smp1_roll = RcppRoll::roll_mean(CO2_smp1,5,fill=NA),
         CO2_smp2_roll = RcppRoll::roll_mean(CO2_smp2,5,fill=NA)
  )



######################
#CH4 im Boden
gga_times <- bemerkungen[grep("soil CH4",bemerkungen$Bemerkung),]
gga_times$gga <- str_extract(gga_times$Bemerkung,"gga|micro")
gga_times[2,2] <- datelim[2]
#gga_times[2,2] <- now()

gga_soil <- read_GGA(datelim = c(gga_times[1,1],gga_times[1,2]),table.name = gga_times$gga[1])
micro_soil <- read_GGA(datelim = c(gga_times[2,1],gga_times[2,2]),table.name = gga_times$gga[2])
gga_soil$gga <- "gga"
micro_soil$gga <- "micro"
gga <- rbind(gga_soil,micro_soil)
gga <- gga[order(gga$date),]
gga$CH4[gga$CH4 > 10 | gga$CH4 < 0] <- NA

##################
#Kammermessungen

flux_ls <- chamber_arduino(datelim=c(gga_times[2,1],gga_times[2,2]),
                           gga_data = T,
                           return_ls = T,
                           t_init=1,
                           plot="facets",
                           t_offset = "from_df",
                           t_min=2,
                           t_max=5)
flux <- flux_ls[[1]]
flux_data <- flux_ls[[2]]

flux$CO2_flux <- RcppRoll::roll_mean(flux$CO2_GGA_mumol_per_s_m2,5,fill=NA)
flux$CH4_flux <- RcppRoll::roll_mean(flux$CH4_mumol_per_s_m2 * 10^3,5,fill=NA)


GGA_soil_CO2 <- 
  ggplot(gga)+
  geom_vline(xintercept = P_step_date,linetype=2,color="grey")+
  geom_line(aes(date,CO2,col=gga))+
  geom_line(data = subset(data_probe1u2,tiefenstufe == 3),aes(date,CO2_smp1_roll,col="profile 2"))+
  geom_line(data = subset(data_probe1u2,tiefenstufe == 3),aes(date,CO2_smp2_roll,col="profile 1"))+
  coord_cartesian(xlim=datelim)+
  labs(x = "",y = CO[2]~(ppm),col="tiefe = -10.5 cm")
GGA_soil_CH4 <- 
  ggplot(gga)+
  geom_vline(xintercept = P_step_date,linetype=2,color="grey")+
  geom_line(aes(date,CH4,col=gga))+
  scale_color_manual(values = scales::hue_pal()(4)[1:2])+
  guides(col=F)+
#    xlim(ymd_h("23.04.11 10","23.04.13 10"))
 coord_cartesian(xlim=datelim)+
  labs(x = "", y = CH[4]~(ppm))

FCO2_plt <- 
  ggplot(flux)+
  geom_vline(xintercept = P_step_date,linetype=2,color="grey")+
  geom_line(aes(date,CO2_flux))+
  coord_cartesian(xlim=datelim)+
  labs(x = "", y = expression(italic(F[CO2])~(mu * mol ~ m^{-2} ~ s^{-1})))
FCH4_plt <- 
  ggplot(flux)+
  geom_vline(xintercept = P_step_date,linetype=2,color="grey")+
  geom_line(aes(date,CH4_flux))+
  coord_cartesian(xlim=datelim)+
  labs(x = "", y = expression(italic(F[CH4])~(nmol ~ m^{-2} ~ s^{-1})))
  
cols <- RColorBrewer::brewer.pal(4,"PuOr")

PPC_plot <- ggplot(subset(data_PPC,id != 5))+
  geom_vline(xintercept = P_step_date,linetype=2,color="grey")+
  geom_line(aes(date,PPC,col=id))+
  coord_cartesian(xlim=datelim)+
  scale_color_manual("subspace",values = c(cols,1))
  labs(x="",y="PPC (Pa/s)")
P_plot <- ggplot(subset(data_Proll,id %in% c(1)))+
  geom_vline(xintercept = P_step_date,linetype=2,color="grey")+
  geom_line(aes(date,P_roll))+
  coord_cartesian(xlim=datelim)+
  labs(x="",y=expression(P["mean"]~"(Pa)"))

#Prec_plot <- 
#ggplot(klima)+
#  geom_line(aes(date,Precip_24Tot),col="blue")
png(paste0(plotpfad_PPchamber,"GGA_soil_",Versuch,".png"),width = 9,height = 9,units = "in",res=300)

egg::ggarrange(GGA_soil_CO2,
               FCO2_plt,
               GGA_soil_CH4,
               FCH4_plt,
               P_plot,
               PPC_plot,ncol = 1,heights = c(rep(2,4),1,1))

dev.off()
