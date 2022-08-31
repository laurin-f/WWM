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

########################
Versuche <- list.files(datapfad_PP_Kammer,pattern = "data_merge_\\d.RData")
data_merge_ls <- list()
nDS <- 2
for(i in seq_along(Versuche)){
  #load(file=paste0(datapfad_PP_Kammer,"data_merge_",i,".RData"))
  load(file=paste0(datapfad_PP_Kammer,"data_merge_",nDS,"DS_",i,".RData"))
  data_merge_ls[[i]] <- data_merge
  data_merge_ls[[i]]$Versuch <- i
}
data_merge <- do.call(rbind,data_merge_ls)
data_merge <- data_merge %>% 
  filter(tiefe == 1) %>% 
  group_by(Versuch) %>%
  mutate(
    Proll_meanr12 = RcppRoll::roll_meanr(P_roll,12,fill=NA),
    PPC_meanr12 = RcppRoll::roll_meanr(PPC5,12,fill=NA),
    PPC_meanr6 = RcppRoll::roll_meanr(PPC5,6,fill=NA),
    PPC_meanr3 = RcppRoll::roll_meanr(PPC5,3,fill=NA),
  )

data_depth1 <- subset(data_merge, !is.na(step) & tiefe==1)


hist((data_depth1$RMSE))
#range(data$inj_mol_m2_s[data$inj_mol_m2_s !=0])

names(data_depth1)

####################
#swc
source("./PP_kammer/Bodenfeuchte_PP_Kammer.R")

swc_sub <- subset(swc_wide,date %in% (data_depth1$date))

data_DS <- merge(data_depth1,swc_sub,all.x = T)
data_DS <- subset(data_DS,P_roll > -1 & P_roll < 1)
titles <- c("2D PP",
            "2D PP",
            "1D PP",
            "2D PP and P mov.mean",
            "2D PP",
            "2D PP - 2D PP", 
            "1D PP",
            "1D PP - 2D PP")
data_DS$mode <- factor(data_DS$Versuch,levels=1:8,labels = titles)
data_DS$mode[data_DS$Versuch == 8 & data_DS$step > 4] <- "2D PP"
data_DS$mode[data_DS$Versuch == 8 & data_DS$step <= 4] <- "1D PP"
data_DS$mode[data_DS$Versuch == 1 & data_DS$step <= 8] <- "2D PP"
data_DS$mode[data_DS$Versuch == 6] <- "2D PP"

ggplot(subset(data_merge))+
  geom_line(aes(date,PPC5))+
  geom_line(aes(date,PPC_meanr12),col=2)+
  geom_line(aes(date,PPC_meanr6),col=3)+
  geom_line(aes(date,PPC_meanr3),col=4)+
  facet_wrap(~Versuch,scales="free")
ggplot(subset(data_DS,Versuch %in% c(2,3,5,7,8)))+
  geom_point(aes(P_roll,DSD0,col=factor(Versuch)))+
  facet_wrap(~mode)

data_1D <- subset(data_DS,mode == "1D PP")
data_2D <- subset(data_DS,mode == "2D PP" & Versuch  %in% c(2,3,5,7,8))

fm_1D <- glm(DSD0 ~ PPC_meanr12 + P_roll,data=data_1D)
fm_2D <- glm(DSD0 ~ PPC_meanr12 + P_roll,data=data_2D)
cor(data_2D$PPC_meanr12,data_2D$P_roll,"complete")
cor(data_2D$PPC_meanr12,data_2D$DSD0,"complete")
cor(data_2D$P_roll,data_2D$DSD0,"complete")

summary(fm_1D)
summary(fm_2D)
R2_2D <- 1- fm_2D$deviance/fm_2D$null.deviance
R2_1D <- 1- fm_1D$deviance/fm_1D$null.deviance

R2_2D
R2_1D

ggplot(subset(data_DS,Versuch %in% c(2,3,5,7,8)))+
  geom_smooth(aes(PPC_meanr12,DSD0),method="glm",col=1)+
  #  annotate("text",0.2,0.7,label=)+
  geom_text(data = 
              data.frame(x=c(0.3,0.3),
                         y=c(0.7,0.7),
                         mode=c("1D PP","2D PP"),
                         label=c(
                           paste("y = ",round(fm_1D$coefficients[2],2),"x +",round(fm_1D$coefficients[1],2)),
                           paste("y = ",round(fm_2D$coefficients[2],2),"x +",round(fm_2D$coefficients[1],2))
                         )),
            aes(x,y,label=label))+
  geom_point(aes(PPC_meanr12,DSD0,col=factor(Versuch)))+
  facet_wrap(~mode)+
  labs(x="PPC (Pa/s)",y=expression(D[S]/D[0]),col="Versuch")
ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_meanr12.png"),width = 6,height = 5)
ggplot(subset(data_DS,Versuch %in% c(1,2,3,5,7,8)))+
  geom_point(aes(PPC,DSD0,col=factor(Versuch)))+
  facet_wrap(~mode)
ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_Versuche.png"),width = 6,height = 5)
ggplot(subset(data_DS,Versuch %in% c(1,2,3,5,7,8) & mode %in% c("1D PP","2D PP")))+
  geom_point(aes(PPC_meanr12,DSD0,col=as.factor(mode)))+
  facet_wrap(~mode)
ggplot(subset(data_DS,Versuch %in% c(1,2,3,5,7,8) & mode %in% c("1D PP","2D PP")))+
  geom_point(aes(PPC_meanr12,DSD0,col=swc_7))+
  facet_wrap(~mode)
#ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_dP.png"),width = 6,height = 5)

ggplot(data_DS)+
  geom_point(aes(PPC,DSD0,col=swc_7))+
  scale_color_continuous(trans="reverse")+
  ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_swc.png"),width = 6,height = 5)

ggplot(data_DS)+
  geom_point(aes(DSD0,PPC,col=P_roll))+
  scale_color_viridis_c()
#ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_P_roll.png"),width = 6,height = 5)
ggplot(data_DS)+
  geom_point(aes(P_roll,DSD0,col=as.factor(Versuch)))+
  facet_wrap(~Versuch)
#ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_P_roll.png"),width = 6,height = 5)

cor(data_DS$DSD0,data_DS$PPC5,use="complete.obs")
cor(data_DS$DSD0,data_DS$P_roll,use="complete.obs")

ggplot(data_depth1)+geom_point(aes(date,DSD0,col=step))+facet_wrap(~Versuch,scales="free")

ggplot(subset(data_DS,P_roll > -2 & P_roll < 2))+
  geom_point(aes(DSD0,P_roll,col=PPC5))#+
#ylim(c(-1,1))
#  facet_wrap(~Versuch)
ggplot(subset(data_merge,!is.na(step)))+
  geom_point(aes(DSD0,P_roll,col=PPC))+
  facet_wrap(~tiefe,scales="free")

data_t1 <- subset(data_merge, tiefe == 1)
DSD0_fm <- glm(DSD0 ~ PPC5 + P_roll + (PPC5 * P_roll),data=data_t1)
summary(DSD0_fm)
data_t1$DSD0_pred <- predict(DSD0_fm,newdata = data.frame(data_t1))
ggplot(data_t1)+
  geom_line(aes(date,DSD0_pred,col="preds"))+
  geom_line(aes(date,DSD0,col="obs"))+
  facet_wrap(~Versuch,scales = "free",ncol=1)

geom_point(aes(DSD0,DSD0_pred,col=P_roll))
