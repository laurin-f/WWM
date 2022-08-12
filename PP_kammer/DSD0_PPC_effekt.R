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
for(i in seq_along(Versuche)){
  load(file=paste0(datapfad_PP_Kammer,"data_merge_",i,".RData"))
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
range(data$inj_mol_m2_s[data$inj_mol_m2_s !=0])

names(data_depth1)

####################
#swc
source("./PP_kammer/Bodenfeuchte_PP_Kammer.R")

swc_sub <- subset(swc_wide,date %in% (data_depth1$date))

data_DS <- merge(data_depth1,swc_sub,all.x = T)


ggplot(subset(data_merge))+
  geom_line(aes(date,PPC5))+
  geom_line(aes(date,PPC_meanr12),col=2)+
  geom_line(aes(date,PPC_meanr6),col=3)+
  geom_line(aes(date,PPC_meanr3),col=4)+
  facet_wrap(~Versuch,scales="free")
ggplot(subset(data_DS,P_roll > -2 & P_roll < 2))+
  #geom_point(aes(DSD0,PPC5,col=factor(Versuch))))
  #geom_point(aes(DSD0,PPC_meanr3,col=factor(Versuch)))
  #geom_point(aes(DSD0,PPC_meanr6,col=factor(Versuch)))
  geom_point(aes(PPC_meanr12,DSD0,col=factor(Versuch)))+
  ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_meanr12.png"),width = 6,height = 5)
ggplot(subset(data_DS,P_roll > -2 & P_roll < 2))+
  geom_point(aes(PPC,DSD0,col=factor(Versuch)))+
  ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_Versuche.png"),width = 6,height = 5)
ggplot(subset(data_DS,P_roll > -2 & P_roll < 2))+
  geom_point(aes(PPC,DSD0,col=swc_7))+
  scale_color_continuous(trans="reverse")+
  #scale_color_distiller(palette = "RdBu",trans="reverse")
  #scale_color_viridis_c()
  ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_swc.png"),width = 6,height = 5)
ggplot(subset(data_DS,P_roll > -2 & P_roll < 2))+
  geom_point(aes(DSD0,PPC,col=P_roll))+
  scale_color_viridis_c()
  ggsave(paste0(plotpfad_PPchamber,"PPC_DSD0_P_roll.png"),width = 6,height = 5)
cor(data_depth1$DSD0,data_depth1$PPC5,use="complete.obs")
cor(data_depth1$DSD0,data_depth1$P_roll,use="complete.obs")
ggplot(data_depth1)+geom_point(aes(date,DSD0,col=step))+facet_wrap(~Versuch,scales="free")

ggplot(subset(data_merge,!is.na(step)&tiefe == 1))+
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
