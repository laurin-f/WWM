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
datelim <- c(ymd_h("22.09.27 10","23.02.22 10"))
#datelim <- c(ymd_h("23.02.16 10","23.02.22 10"))
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
#GGA
 # load(paste0(datapfad_PP_Kammer,"flux_ls_i.RData"))

if(load){
  load(paste0(datapfad_PP_Kammer,"flux_ls_Dyn_GGA.RData"))
  
}else{
  flux_ls_i <- list()
  date_seq <- seq.POSIXt(datelim[1],datelim[2],by = "7 days")
  date_seq[length(date_seq)] <- datelim[2]
  for(i in seq_along(date_seq[-1])){
    print(i)
    datelim_i <- date_seq[c(i,i+1)]
    
    flux_ls_i[[i]] <- chamber_arduino(datelim=datelim_i,
                               gga_data = T,
                               return_ls = T,
                               t_init=0.5,
                               plot="",
                               t_min=2,
                               t_max=4)
  }
  
  flux_ls <- lapply(flux_ls_i,"[[",1)
  flux_data_ls <- lapply(flux_ls_i,"[[",2)
  #save(flux_ls_i,file=paste0(datapfad_PP_Kammer,"flux_ls_i.RData"))
  rm(flux_ls_i)
  cols <- sapply(flux_ls,names)
  cols_data <- sapply(flux_data_ls,names)
  all_cols <- cols[[which(sapply(cols,length) == 33)[1]]]
  dyn_cols <- cols[[which(sapply(cols,length) < 33)[1]]]
  
  all_data_cols <- cols_data[[which(sapply(cols,length) == 33)[1]]]
  dyn_data_cols <- cols_data[[which(sapply(cols,length) < 33)[1]]]
  
  for(i in which(sapply(cols,length) < 33)){
    flux_ls[[i]][,all_cols[!all_cols %in% dyn_cols]] <- NA
    flux_data_ls[[i]][,all_data_cols[!all_data_cols %in% dyn_data_cols]] <- NA
  }
  flux <- do.call(rbind,flux_ls)
  flux_data <- do.call(rbind,flux_data_ls)
  rm(flux_ls)
  rm(flux_data_ls)
  save(flux,flux_data,file=paste0(datapfad_PP_Kammer,"flux_ls_Dyn_GGA.RData"))
}


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



flux$CO2_Dyn <- flux$CO2_mumol_per_s_m2
flux$CO2_Dyn_roll <- RcppRoll::roll_mean(flux$CO2_mumol_per_s_m2,20,fill=NA,na.rm = T)
flux$CO2_GGA <- flux$CO2_GGA_mumol_per_s_m2
sub <- subset(flux,!is.na(CO2_GGA))



ggplot(flux)+
  geom_line(aes(date,CO2_Dyn,col="Dyn"),alpha = 0.5)+
  #geom_point(data = subset(flux, CO2_R2 > 0.4),aes(date,CO2_Dyn,col="Dyn"),alpha = 0.5)+
  geom_line(aes(date,CO2_Dyn_roll,col="Dyn"))+
  #geom_line(data = subset(sub,CO2_R2 > 0.5),aes(date,CO2_Dyn,col="Dyn"))+
  geom_line(aes(date,CO2_GGA,col="GGA"))
ggplot(sub,aes(CO2_GGA,CO2_Dyn_roll,col=CO2_R2))+
  geom_point(aes(CO2_GGA,CO2_Dyn),alpha=0.2)+
  geom_point()+
  ggpubr::stat_cor(aes(label = ..rr.label..))+
  #geom_point(data = subset(sub,CO2_R2 > 0.2),aes(CO2_GGA,CO2_Dyn_roll,col=CO2_R2))+
  geom_smooth(method = "glm")+
  geom_abline(slope = 1)
names(flux)

datelim2 <- ymd_h("23.01.09 00","23.01.16 10")
flux_plot <- ggplot(sub_daterange(flux,datelim2))+
  geom_line(aes(date,CO2_Dyn),alpha=0.2)+
  geom_line(aes(date,CO2_GGA,col="GGA"))+
  geom_line(aes(date,CO2_Dyn_roll,col="Dyn"))
PPC_sub <- subset(data_PPC,id %in% 1:4)
P_plot <- ggplot(sub_daterange(PPC_sub,datelim2))+
  geom_line(aes(date,P,col=factor(id)))

ggpubr::ggarrange(flux_plot,P_plot,ncol = 1,align = "v")
###################
#flux_data
flux_data <- flux_data %>% 
  mutate(messid_2 = RcppRoll::roll_max(messid,120,fill=NA,na.rm = T))


flux_data_2 <- flux_data %>% 
  filter(!is.na(messid_2) & !is.infinite(messid_2)) %>% 
  group_by(messid_2) %>% 
  mutate(CO2_int = imputeTS::na_interpolation(CO2),
         CO2_roll = RcppRoll::roll_mean(CO2_int,30,fill=NA)
  )
ggplot(sub_daterange(flux_data_2,datelim2))+
  geom_point(aes(date,CO2-mean(CO2,na.rm = T),col = "Dyn",group = messid_2),alpha=0.3)+
  geom_line(aes(date,CO2_roll-mean(CO2_roll,na.rm = T),col = "Dyn",group = messid_2))+
  geom_line(aes(date,CO2_GGA-mean(CO2_GGA,na.rm = T),col = "GGA",group = messid_2))+
  facet_wrap(~round(messid_2),scales ="free")
