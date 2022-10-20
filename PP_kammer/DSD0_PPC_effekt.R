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

pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

########################
Versuche <- list.files(datapfad_PP_Kammer,pattern = "data_merge_\\d.RData")
data_merge_ls <- list()
step_date_ls <- list()
nDS <- 1
file_suffix <- "_40cm"
#file_suffix <- ""
for(i in seq_along(Versuche)){
  #load(file=paste0(datapfad_PP_Kammer,"data_merge_",i,".RData"))
  load(file=paste0(datapfad_PP_Kammer,"data_merge_",nDS,"DS_",i,file_suffix,".RData"))
  data_merge_ls[[i]] <- data_merge
  step_date_ls[[i]] <- data.frame(date=step_date,Versuch=i)
  data_merge_ls[[i]]$Versuch <- i
}

data_merge <- do.call(rbind,data_merge_ls)
step_date <- do.call(rbind,step_date_ls)
# data_merge <- data_merge %>% 
#   mutate()
data_merge <- data_merge %>% 
  filter(tiefe == 1) %>%
  filter(date %in% round_date(date,"hours")) %>% 
  group_by(Versuch) %>%
  mutate(
    #PPC5 = imputeTS::na_interpolation(PPC5),
    #PPC5_int = ifelse(is.na(PPC5),min(PPC5,na.rm=T),PPC5),
    P_roll_int = imputeTS::na_interpolation(P_roll),
    Proll_meanr12 = RcppRoll::roll_meanr(P_roll_int,12,fill=NA),
    Proll_maxr6 = RcppRoll::roll_maxr(abs(P_roll_int),6,fill=NA),
    Proll_maxr6_int = RcppRoll::roll_maxr(abs(P_roll_int),6,fill=NA),
    Proll_meanr6 = RcppRoll::roll_meanr((P_roll_int),6,fill=NA),
    #Proll_maxr6 = ifelse(is.na(P_roll),NA,Proll_maxr6),
    PPC_meanr12 = RcppRoll::roll_meanr(PPC5_int,12,fill=NA),
    PPC_meanr6 = RcppRoll::roll_meanr(PPC5_int,6,fill=NA),
    PPC_meanr3 = RcppRoll::roll_meanr(PPC5_int,3,fill=NA),
  )
#test <- subset(data_merge,Versuch==1)
data_merge$Proll_maxr6[is.na(data_merge$P_roll)] <- NA
for(i in na.omit(unique(data_merge$Versuch))){
  notNAdates <- range(subset(data_merge,Versuch == i & !is.na(P_roll))$date)+(3600*3*c(-1,1))
  dateIDs <- daterange_id(data_merge,notNAdates)
  data_merge$P_roll[dateIDs] <- data_merge$P_roll_int[dateIDs]
  data_merge$PPC5[dateIDs] <- data_merge$PPC5_int[dateIDs]
}

data_probe <- read_sampler(datelim =range(data_merge$date),format="wide")

data_merge <- merge(data_merge,data_probe,all.x = T)

data_steps <- subset(data_merge, !is.na(step))


#hist((data_depth1$RMSE))
#range(data$inj_mol_m2_s[data$inj_mol_m2_s !=0])

#names(data_depth1)

####################
#swc

source("~/FVA/P01677_WindWaldMethan/Programme/Eigenentwicklung/PP_kammer/Bodenfeuchte_PP_Kammer.R")

swc_sub <- subset(swc_wide,date %in% (data_merge$date))

data_DS <- merge(data_steps,swc_sub,all.x = T)
data_DS <- subset(data_DS,P_roll > -1 & P_roll < 1)
titles <- c("2D PP",
            "2D PP",
            "1D PP",
            "2D PP and P mov.mean",
            "2D PP",
            "2D PP", 
            "1D PP",
            "1D PP - 2D PP")
titles2 <- c("2D PP",
             "Überdruck",
            "2D PP",
            "1D PP",
            "2D PP and P mov.mean",
            "2D PP",
            "2D PP", 
            "1D PP",
            "1D PP",
            "2D PP")
data_DS$mode <- factor(data_DS$Versuch,levels=1:8,labels = titles)
data_DS$Versuch[data_DS$Versuch == 8 & data_DS$date > pp_chamber$Ende[30]] <- 9
data_DS$mode[data_DS$Versuch == 9] <- "2D PP"
data_DS$mode[data_DS$Versuch == 8] <- "1D PP"
data_DS$Versuch[data_DS$Versuch == 1 & data_DS$date > pp_chamber$Ende[16]] <- 1.2
data_DS$mode[data_DS$Versuch == 1] <- "2D PP"
data_DS$mode[data_DS$Versuch == 6] <- "2D PP"

data_merge$Versuch[data_merge$date >= min(subset(data_DS,Versuch == 9)$date)] <- 9
data_merge$Versuch[data_merge$date >= min(subset(data_DS,Versuch == 1.2)$date)&
                     data_merge$date <= max(subset(data_merge,Versuch == 1)$date)] <- 1.2

data_merge$Versuch[data_merge$date >= ymd_h("2022.07.27 12") &
                     data_merge$date <= ymd_h("2022.07.27 17")] <- NA
step_date$Versuch[step_date$date >= min(subset(data_DS,Versuch == 9)$date)] <- 9
data_merge$mode <- factor(data_merge$Versuch,labels=titles2)
data_DS_swc <- merge(data_merge,swc_sub,all.x = T)

data_DS2 <- subset(data_DS_swc,P_roll > -1 & P_roll < 1)

data_DS3 <- subset(data_DS_swc, Proll_maxr6 < 1.5 & P_roll < 0.5)
# ggplot(subset(data_merge))+
#   geom_line(aes(date,PPC5))+
#   geom_line(aes(date,PPC_meanr12),col=2)+
#   geom_line(aes(date,PPC_meanr6),col=3)+
#   geom_line(aes(date,PPC_meanr3),col=4)+
#   facet_wrap(~Versuch,scales="free")
# ggplot(subset(data_DS,Versuch %in% c(2,3,5,7,8)))+
#   geom_point(aes(P_roll,DSD0,col=factor(Versuch)))+
#   facet_wrap(~mode)

Versuche_auswahl <- c(2,3,5,6,7,8,9)

data_1D <- subset(data_DS2,mode == "1D PP" & Versuch  %in% Versuche_auswahl)
data_2D <- subset(data_DS2,mode == "2D PP" & Versuch  %in% Versuche_auswahl)

fm_1D <- glm(DSD0 ~ (PPC_meanr6) ,data=data_1D)
fm_2D <- glm(DSD0 ~ (PPC_meanr6) ,data=data_2D)
 
# fm_1D_full <- glm(DSD0 ~ PPC_meanr12 + factor(Versuch),data=data_1D)
# fm_2D_full <- glm(DSD0 ~ PPC_meanr12 + factor(Versuch),data=data_2D)
# cor(data_2D$PPC_meanr12,data_2D$P_roll,"complete")
# cor(data_2D$PPC_meanr12,data_2D$DSD0,"complete")
# cor(data_2D$P_roll,data_2D$DSD0,"complete")

# summary(fm_1D)
# summary(fm_2D)
R2_2D <- 1- fm_2D$deviance/fm_2D$null.deviance
#R2_2D_full <- 1- fm_2D_full$deviance/fm_2D_full$null.deviance
R2_1D <- 1- fm_1D$deviance/fm_1D$null.deviance
#R2_1D_full <- 1- fm_1D_full$deviance/fm_1D_full$null.deviance


 R2_2D
# R2_2D_full
 R2_1D
# R2_1D_full
# ggplot(subset(data_merge,!is.na(PPC)))+
#   geom_line(aes(date,Proll_maxr6))+
#   geom_point(data=data_DS3,aes(date,Proll_maxr6))+
#   geom_point(data=data_DS2,aes(date,P_roll),col=2,alpha=0.7)+
#   geom_line(aes(date,P_roll))+
#   facet_wrap(~Versuch,scales="free")


   
 
slope_1D <- round(fm_1D$coefficients[1],2)
slope_2D <- round(fm_2D$coefficients[1],2)
data_DS3$mode <- factor(data_DS3$mode,levels=c("1D PP","2D PP"))

################################
#PPC_DSD0 plot
subset(data_DS3,Versuch %in% Versuche_auswahl) %>% 
  group_by(mode) %>% 
  summarise(range(DSD0))
0.02/0.3
plot_formula <- y~ (x)
ggplot(subset(data_DS3,Versuch %in% Versuche_auswahl),aes(PPC_meanr6,DSD0))+
  geom_smooth(formula=plot_formula,method="glm",col=1,lwd=0.7,linetype=2)+
  ggpubr::stat_regline_equation(aes(label= ..eq.label..),formula=plot_formula)+
  ggpubr::stat_regline_equation(label.y = 0.35,aes(label= ..rr.label..),formula=plot_formula)+
  #geom_smooth(aes(PPC_meanr6,DSD0,col=factor(Versuch,levels=Versuche_auswahl,labels=1:length(Versuche_auswahl))),formula=(y~exp(x)),method="glm",lwd=0.7,linetype=2)+
  # geom_text(data = 
  #             data.frame(x=c(0.05,0.05),
  #                        y=c(Inf,Inf),
  #                        mode=c("1D PP","2D PP"),
  #                        label=c(
  #                          paste("y = ",round(fm_1D$coefficients[2],2),"exp(x)",
  #                                ifelse(slope_1D < 0,"-","+"),abs(slope_1D)),
  #                          paste("y = ",round(fm_2D$coefficients[2],2),"exp(x)",
  #                                ifelse(slope_2D < 0,"-","+"),abs(slope_2D))
  #                        )),
  #           aes(x,y,label=label),hjust=0,vjust=2)+
  geom_point(aes(col=factor(Versuch,levels=Versuche_auswahl,labels=1:length(Versuche_auswahl))))+
  facet_wrap(~mode)+
  labs(x="PPC (Pa/s)",y=expression(D[eff]/D[0]),col="experiment")+
  scale_color_brewer(palette = 2,type="qual")+
  theme_bw()+
  ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_modes",nDS,"DS",file_suffix,".png"),width = 7,height = 4)

###########################################
#P_roll_plot
# ggplot(subset(data_DS3,Versuch %in% Versuche_auswahl))+
#   geom_point(aes(P_roll,DSD0,col=factor(Versuch)))+
#   #geom_line(aes(date,DSD0,col=factor(Versuch,levels=Versuche_auswahl,labels=1:length(Versuche_auswahl))))+
#   facet_wrap(~mode)+
#   labs(x="PPC (Pa/s)",y=expression(D[eff]/D[0]),col="experiment")+
#   scale_color_brewer(palette = 2,type="qual")+
#   theme_bw()


##############################################
#data_overview
names(data_merge)
# ggplot(subset(data_merge,Versuch == 7))+
#   geom_line(aes())

ggplot(subset(data_merge,Versuch %in% Versuche_auswahl & !is.na(PPC5)))+
  #geom_vline(data=subset(step_date,Versuch %in% Versuche_auswahl),aes(xintercept=date),linetype=2,alpha=0.2)+
  geom_ribbon(aes(date,ymin=DSD0_min,ymax=DSD0_max,fill=mode),alpha=0.3)+
  geom_line(aes(date,DSD0,col=mode))+
  #geom_point(data=subset(data_merge, Versuch %in% Versuche_auswahl & !is.na(step)),aes(date,DSD0,col=factor(mode)))+
  geom_line(aes(date,PPC5,linetype=""))+
  geom_line(aes(date,PPC_meanr6,linetype="meanr 6h"))+
  scale_y_continuous(sec.axis = sec_axis(~.,name="PPC (Pa/s)"))+  
  scale_x_datetime(date_labels = "%H:%M")+
  #geom_line(aes(date,PPC_meanr12))+
  facet_wrap(~factor(Versuch,labels=1:length(Versuche_auswahl)),scales="free_x")+
  #facet_wrap(~factor(Versuch,levels=c(2,5,9,3,7,8),labels=1:length(Versuche_auswahl)),scales="free_x")+
  labs(x="",y =expression(D[eff]/D[0]),linetype="PPC (Pa/s)",color=expression(D[eff]/D[0]),fill = expression(D[eff]/D[0]))+
  ggsave(paste0(plotpfad_PPchamber,"DSD0_facets",nDS,"DS",file_suffix,".png"),width = 9,height = 6)



# ggplot(subset(data_DS3,Versuch %in% Versuche_auswahl))+
#   geom_smooth(aes(PPC_meanr6,DSD0),formula=(y~exp(x)),method="glm",col=1,lwd=0.7,linetype=2)+
#   geom_point(aes(PPC_meanr6,DSD0,col=swc_14))+
#   facet_wrap(~mode)+
#   scale_color_viridis_c(direction=-1)+
#   labs(x="PPC (Pa/s)",y=expression(D[eff]/D[0]),col="SWC (Vol %)")#+
# #  ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_swc.png"),width = 7,height = 4)
# 
# ggplot(subset(data_DS3,Versuch %in% Versuche_auswahl))+
#   geom_point(aes(PPC_meanr6,DSD0,col=Proll_meanr12))+
#   facet_wrap(~mode)+
#   scale_color_viridis_c()
#   
# ggplot(subset(data_DS3,Versuch %in% Versuche_auswahl & Versuch != 2))+
#   geom_point(aes(PPC_meanr6,DSD0,col=dP_horiz))+
#   facet_wrap(~mode)+
#   scale_color_viridis_c()
#   
# ggplot(subset(data_DS,Versuch %in% c(2,3,5,7,8)))+
#   geom_point(aes(PPC,DSD0,col=factor(Versuch)))+
#   facet_wrap(~mode)
# #ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_Versuche.png"),width = 6,height = 5)
# ggplot(subset(data_DS,Versuch %in% c(1,2,3,5,7,8) & mode %in% c("1D PP","2D PP")))+
#   geom_point(aes(PPC_meanr12,DSD0,col=as.factor(mode)))+
#   facet_wrap(~mode)
# ggplot(subset(data_DS,Versuch %in% c(1,2,3,5,7,8) & mode %in% c("1D PP","2D PP")))+
#   geom_point(aes(PPC_meanr12,DSD0,col=swc_7))+
#   facet_wrap(~mode)
# #ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_dP.png"),width = 6,height = 5)
# 
# # ggplot(data_DS)+
# #   geom_point(aes(PPC,DSD0,col=swc_7))+
# #   scale_color_continuous(trans="reverse")+
# #   ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_swc.png"),width = 6,height = 5)
# 
# ggplot(data_DS)+
#   geom_point(aes(DSD0,PPC,col=P_roll))+
#   scale_color_viridis_c()
# #ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_P_roll.png"),width = 6,height = 5)
# ggplot(data_DS)+
#   geom_point(aes(P_roll,DSD0,col=as.factor(Versuch)))+
#   facet_wrap(~Versuch)
# #ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_P_roll.png"),width = 6,height = 5)
# 
# cor(data_DS$DSD0,data_DS$PPC5,use="complete.obs")
# cor(data_DS$DSD0,data_DS$P_roll,use="complete.obs")
# 
# ggplot(data_depth1)+geom_point(aes(date,DSD0,col=step))+facet_wrap(~Versuch,scales="free")
# 
# ggplot(subset(data_DS,P_roll > -2 & P_roll < 2))+
#   geom_point(aes(DSD0,P_roll,col=PPC5))#+
# #ylim(c(-1,1))
# #  facet_wrap(~Versuch)
# ggplot(subset(data_merge,!is.na(step)))+
#   geom_point(aes(DSD0,P_roll,col=PPC))+
#   facet_wrap(~tiefe,scales="free")
# 
# data_t1 <- subset(data_merge, tiefe == 1)
# DSD0_fm <- glm(DSD0 ~ PPC5 + P_roll + (PPC5 * P_roll),data=data_t1)
# summary(DSD0_fm)
# data_t1$DSD0_pred <- predict(DSD0_fm,newdata = data.frame(data_t1))
# ggplot(data_t1)+
#   geom_line(aes(date,DSD0_pred,col="preds"))+
#   geom_line(aes(date,DSD0,col="obs"))+
#   facet_wrap(~Versuch,scales = "free",ncol=1)
# 
# geom_point(aes(DSD0,DSD0_pred,col=P_roll))
# 

##############################
#P_roll Versuch


#devtools::install_github("thomasp85/ggforce", ref = '4008a2e')
P_roll_data <- subset(data_merge,Versuch == 1.2)

P_roll_data$P_roll_offset <- c(NA,P_roll_data$P_roll_int[-nrow(P_roll_data)])
P_roll_data$Proll_meanr3 <- RcppRoll::roll_meanr((P_roll_data$P_roll_int),3,fill=NA)
P_roll_data$Proll_meanr3 <- imputeTS::na_interpolation(P_roll_data$Proll_meanr3)
P_roll_data_2 <- subset(P_roll_data, Proll_maxr6_int < 2.5)
#P_roll_data_2 <- subset(P_roll_data, Proll_meanr3 < 1 & Proll_meanr3 > -2.5)

fm_Proll <- glm(DSD0 ~ Proll_meanr3,data=P_roll_data)
fm_Proll_2 <- glm(DSD0 ~ Proll_meanr3,data=P_roll_data_2)
R2_Proll <- 1- fm_Proll$deviance/fm_Proll$null.deviance
R2_Proll_2 <- 1- fm_Proll_2$deviance/fm_Proll_2$null.deviance
R2_Proll
R2_Proll_2
fm_Proll$coefficients
fm_Proll_2$coefficients
Deff_fac <- 20/nDS
Proll_plot <- ggplot(P_roll_data)+
  geom_hline(yintercept = 0,col="grey",linetype=1)+
  geom_hline(yintercept = c(2.5,-2.5),col="grey",linetype=2)+
  #geom_line(aes(date,Proll_meanr6))+
  geom_point(data=P_roll_data_2,aes(date,P_roll_int))+
  geom_line(aes(date,P_roll_int))+
#  geom_point(aes(date,Proll_meanr3),col="grey")+
  #geom_point(aes(date,Proll_meanr6),col="blue")+
  geom_ribbon(aes(date,ymax=Proll_maxr6_int,ymin=P_roll_int),alpha=0.2)+
#  geom_point(aes(date,P_roll_offset),col="grey")+
  
  scale_y_continuous(sec.axis = sec_axis(~./Deff_fac,name=expression(D[eff]/D[0])))+ 
  theme_bw()+
  theme(
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right = element_text(color = "red"),
    axis.title.x = element_blank()
  )+
  labs(y="pressure (Pa)",x="")+
  geom_line(aes(date,DSD0*Deff_fac),col=2)+
  geom_point(data=P_roll_data_2,aes(date,DSD0*Deff_fac),col=2)

# P_roll_scatter <- 
#   ggplot(P_roll_data)+
#   geom_smooth(aes(P_roll_int,DSD0),col=1,linetype=2,lwd=.7,method="glm")+
#   geom_point(aes(P_roll_int,DSD0))+
#   geom_smooth(data=P_roll_data_2,aes(P_roll,DSD0,col=""),method = "glm")+
#   geom_point(data=P_roll_data_2,aes(P_roll,DSD0,col=""),alpha=0.7)+
#   labs(y=expression(D[S]/D[0]),x=expression(P["moving average"]~"(Pa)"),col="subset")+
#   theme_bw()
P_roll_scatter_zoom <- 
  ggplot(P_roll_data)+
  geom_smooth(aes(P_roll_int,DSD0),col=1,linetype=2,lwd=.7,method="glm")+
  geom_point(aes(P_roll_int,DSD0))+
  #geom_smooth(data=P_roll_data_2,aes(P_roll,DSD0,col=""),method = "glm")+
  geom_point(data=P_roll_data_2,aes(P_roll,DSD0,col=""),alpha=0.7)+
  ggforce::facet_zoom(Proll_maxr6_int < 2.5,ylim=range(P_roll_data_2$DSD0),zoom.size = 0.8)+
  labs(y=expression(D[eff]/D[0]),x="pressure (Pa)",col=expression("abs"(P["6h"]) < 2.5 ~ Pa))+
  theme_bw()+
  theme(legend.position = "top")
#P_roll_scatter_zoom <- 
  # ggplot(P_roll_data)+
  # geom_smooth(data=P_roll_data_2,aes(P_roll,DSD0,col=""),method = "glm")+
  # geom_point(data=P_roll_data_2,aes(P_roll,DSD0,col=""))+
  # labs(y=expression(D[eff]/D[0]),x=expression(P["moving average"]~"(Pa)"),col="subset")
#scatters <- egg::ggarrange(P_roll_scatter,P_roll_scatter_zoom,widths = c(2,1))

#egg::ggarrange(Proll_plot,P_roll_scatter,ncol = 2,widths = c(3,1))
# png(paste0(plotpfad_PPchamber,"P_roll_Versuch.png"),width = 7,height=3,units = "in",res=300)
# egg::ggarrange(Proll_plot,P_roll_scatter,widths = c(3,1))
# dev.off()
png(paste0(plotpfad_PPchamber,"P_roll_Versuch_zoom",nDS,"DS",file_suffix,".png"),width = 7,height=5,units = "in",res=300)
egg::ggarrange(Proll_plot,P_roll_scatter_zoom,ncol = 1)
dev.off()

ggplot(P_roll_data)+
  geom_smooth(aes(P_roll,DSD0),method="glm")+
  geom_point(aes(P_roll,DSD0))+
  geom_smooth(data=subset(P_roll_data, Proll_maxr6_int < 2.5),aes(P_roll,DSD0,col="sub"),method = "glm")+
  geom_point(data=subset(P_roll_data, Proll_maxr6_int < 2.5),aes(P_roll,DSD0,col="sub"))

ggplot(subset(P_roll_data, Proll_maxr6_int < 2.5))+
  geom_point(aes(P_roll,DSD0))
ggplot(P_roll_data_2)+
  geom_point(aes(P_roll,DSD0))
