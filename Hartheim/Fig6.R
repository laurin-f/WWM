rm(list=ls())

#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_ms <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Methodenpaper/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
aufbereitete_ds<-paste0(hauptpfad,"Daten/aufbereiteteDaten/DS_Labor/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","ggpubr","png","cowplot","magick","ggforce","imputeTS")
check.packages(packages)
theme_set(theme_classic())
theme_replace(legend.title = element_text(size=10))

#daten laden
load(file=paste0(klimapfad,"klima_data.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(aufbereitete_ds,"Labor_Vergleich.RData"))
#aus "plots_COMSOL_out_Hartheim_sweep.R"
#load(paste0(comsolpfad,"plotdata_Methodenpaper.RData"))
#load(paste0(comsolpfad,"plotdata_Methodenpaper_drift.RData"))
load(paste0(comsolpfad,"plotdata_Methodenpaper_roll.RData"))
load(paste0(comsolpfad,"sandkiste_sweep_data_sub.RData"))
load(paste0(samplerpfad,"tracereinspeisung_sandkiste_agg.RData"))
load(paste0(datapfad_harth,"PPC_DS.RData"))
#load(paste0(datapfad_harth,"DS_long_list_SWC_drift_minmax2.RData"))

load(paste0(datapfad_harth,"DS_long_list_SWC_Ws_drift_minmax.RData"))
DS_long$method[DS_long$method == "SWC_WS"] <- "SWC_T"

#ggplot(ds_soil)+
#  geom_line(aes(date,DSD0_roll,group=paste(id,method),col=method))
#date ranges

#injektion 1 weg
data$Position[data$date > Pumpzeiten$start[10] & data$date < Pumpzeiten$start[12]] <- NA


pos8_date <- min(data$date[which(data$Position ==8 & data$Pumpstufe != 0)])
range1 <- range(data$date[data$Position ==1],na.rm = T)
range2 <- range(data$date[data$Position ==7],na.rm = T)
range3 <- range(data$date[data$Position ==8],na.rm = T)
range2u3 <- range(data$date[data$Position %in% 7:8],na.rm = T)


soil_agg$id <- as.numeric(factor(soil_agg$tiefe,levels=c(2,5,10,20,50,100),labels=c(1,1,1,2,3,3)))
soil_agg_plot <- subset(soil_agg, tiefe %in% c(5,10,20) & date > range2u3[1] & date < range2u3[2] ) %>% 
  group_by(date,range) %>% 
  summarise_all(mean)
soil_agg_plot$id <- as.character(soil_agg_plot$id)

DS_long$Versuch <- NA
DS_long$Versuch[DS_long$date > range2[1] & DS_long$date < range2[2]] <- 2
DS_long$Versuch[DS_long$date > range3[1] & DS_long$date < range3[2]] <- 3
DS_long <- DS_long[!is.na(DS_long$DS),]


DS_long <- DS_long %>%
  group_by(id,method,Versuch) %>%
  mutate(across(matches("(Fz|DSD0)(_min|_max)?$"),list(roll=~RcppRoll::roll_mean(.,n=10,fill=NA)))) %>%
  as.data.frame()


DSD0_cols <- grep("DSD0|Fz",colnames(DS_long),value=T)

DS_long[,paste0(DSD0_cols,"_aniso")] <- DS_long[,DSD0_cols]
DS_long[DS_long$method == "drift",paste0(DSD0_cols,"_aniso")] <- DS_long[DS_long$method == "drift",DSD0_cols] * 1.26

F_df$Versuch <- NA
F_df$Versuch[F_df$date > range2[1] & F_df$date < range2[2]] <- 2
F_df$Versuch[F_df$date > range3[1] & F_df$date < range3[2]] <- 3

h_steady <- 32
ds_sub <- subset(DS_long, (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))

F_df <- F_df %>% 
  group_by(method) %>% 
  mutate(across(starts_with("Fz_"),list(roll = ~RcppRoll::roll_mean(.,n=5,fill=NA)))) %>% 
  as.data.frame()

F_sub <- subset(F_df, (Versuch %in% 2 & date > (Pumpzeiten$start[13] + h_steady*3600)) | date > (Pumpzeiten$start[17] + h_steady*3600))
  
Kammer_flux_agg <- Kammer_flux %>% group_by(day) %>% summarise(CO2flux = mean(CO2flux),CO2flux_max=max(CO2flux_max),CO2flux_min=min(CO2flux_min),date=mean(date))
cols <- scales::hue_pal()(2)
mindate <- ymd_h("2020-07-06 16")
PPC$PPC_roll <- RcppRoll::roll_mean(PPC$PPC,3,fill=NA)
max(PPC$PPC_roll[PPC$date %in% PPC_DS$date])
calm_dates <- PPC$date[which(PPC$PPC_roll < 0.06)]


Kammer_flux_agg$Versuch <- NA
Kammer_flux_agg$Versuch[Kammer_flux_agg$date > range2[1] - 3600 * 40 & Kammer_flux_agg$date < range2[2]] <- 2


soil_wide$Versuch <- NA
soil_wide$Versuch[soil_wide$date > range2[1] - 3600 * 20 & soil_wide$date < range2[2] - 3600 * 20] <- 2
soil_wide$Versuch[soil_wide$date > Pumpzeiten$start[17] & soil_wide$date < Pumpzeiten$ende[17]+3600*20] <- 3

ds_sub$calm <- T
ds_sub$calm[!ds_sub$date %in% calm_dates] <- F


 ds_snopt <- subset(DS_long_roll, date %in% ds_sub$date)
ds_snopt$calm <- T
ds_snopt$calm[!ds_snopt$date %in% calm_dates] <- F



soil_agg_plot$date <- round_date(soil_agg_plot$date,"mins")
ds_soil <- merge(ds_sub[,c("date","Versuch","DSD0_roll","DSD0_roll_aniso","id","method")],soil_agg_plot) 
ds_soil$Versuch <- as.numeric(ds_soil$Versuch)
ds_soil$windy <- factor(ds_soil$Versuch,levels = 2:3,labels=c("windy period","calm period"))
PPC_DS$windy <- factor(PPC_DS$Versuch,levels = 2:3,labels=c("windy period","calm period"))

ds_soil_agg <- ds_soil %>% group_by(id,windy) %>% summarise_all(mean)


ds_sub %>% 
  group_by(method,Versuch,id) %>% 
  summarise(from=min(DSD0_roll,na.rm=T),
            to=max(DSD0_roll,na.rm=T)) %>% 
  mutate(Versuch = ifelse(Versuch == 2, "windy","calm"))
names(ds_sub)
ds_sub %>% 
  group_by(method,Versuch,id) %>% 
  summarise(from=min(Fz_roll,na.rm=T),
            to=max(Fz_roll,na.rm=T)) %>% 
  mutate(Versuch = ifelse(Versuch == 2, "windy","calm"))
range(subset(ds_snopt,Versuch==2 & id==1)$DSD0_roll,na.rm=T)
range(subset(ds_snopt,Versuch==3 & id==1)$DSD0_roll,na.rm=T)
range(subset(ds_snopt,Versuch==2 & id==1)$Fz_roll_0_10,na.rm=T)
range(subset(ds_snopt,Versuch==3 & id==1)$Fz_roll_0_10,na.rm=T)


names(ds_snopt)
range(subset(ds_sub,id==1 & method=="drift")$DSD0_roll,na.rm = T)
range(subset(ds_sub,id==2 & method=="drift")$DSD0_roll,na.rm = T)
range(subset(ds_sub,id==3 & method=="SWC_T")$DSD0_roll,na.rm = T)
range(subset(ds_sub,id==2 & method=="SWC_T")$DSD0_roll,na.rm = T)
range(subset(ds_sub,id==3 & method=="SWC_T")$DSD0_roll,na.rm = T)
range(subset(ds_sub,id==3)$DSD0_roll,na.rm = T)


ds_sub <- subset(ds_sub, method != "SWC_WS" & id < 3)


 ds_snopt$windy <- factor(ds_snopt$Versuch,levels = 2:3,labels=c("windy period","calm period"))


calm_windy <- diff(ds_sub$calm)
ds_sub$calm_id <- NA
start <- c(1,which(calm_windy == 1))
stop <- c(which(calm_windy == -1),nrow(ds_sub))
for(i in seq_along(start))  ds_sub$calm_id[start[i]:stop[i]] <- i
# 
# calm_windy <- diff(ds_snopt$calm)
# ds_snopt$calm_id <- NA
# start <- c(1,which(calm_windy == 1))
# stop <- c(which(calm_windy == -1),nrow(ds_snopt))
# for(i in seq_along(start))  ds_snopt$calm_id[start[i]:stop[i]] <- i
# 


#meth_col <- RColorBrewer::brewer.pal(n = 3, name = "Dark2")[c(2,3)]
meth_col <- scales::hue_pal()(2)

method_exp <- expression("ref"["adj"]~" stand-alone")
method_labels <- c("ref adj","stand-alone")
ds_sub$method2 <- factor(ds_sub$method,levels = c("drift","SWC_T"),labels=method_labels)

#######################
#DPPC
date_seq <- seq.POSIXt(min(ds_sub$date)-24*3600,max(ds_sub$date),by="min")

ds_approx <- ds_sub %>% 
  filter(id == 1) %>%
  #group_by(Versuch,method) %>% 
  group_by(method) %>% 
  summarise(DSD0_1 = approx(x=date,y=DSD0_roll_aniso,xout=date_seq,rule=2)$y,
         Versuch = approx(x=date,y=Versuch,xout=date_seq,method = "constant",f=1,rule=2)$y,
         date = approx(x=date,y=DSD0,xout=date_seq)$x,
         ) %>% ungroup() %>% 
  filter(!is.na(DSD0_1))



lim <- 1e-5
lim2 <- 0.01
ds_peak <- ds_approx %>% 
  filter(!is.na(Versuch)) %>% 
  group_by(Versuch,method) %>% 
  select(date,DSD0_1,Versuch) %>% 
  mutate(
    DSD0_roll = RcppRoll::roll_mean(DSD0_1,n=60,fill=NA),
    slope = c(NA, diff(DSD0_roll)),
    slope2 = RcppRoll::roll_mean(c(NA, diff(slope)),n=60,fill=NA),
    DSD0_min = RcppRoll::roll_minr(DSD0_1,19*60,na.rm = T),
    tal = ifelse(abs(slope) < lim & (slope2) > 0& !DSD0_1 > DSD0_min + lim2, DSD0_1, NA),
    tal_approx = zoo::na.approx(tal,na.rm=F),
    base = ifelse(tal_approx < DSD0_1,tal_approx,DSD0_1),
    #peak_min = DSD0_1-DSD0_min,
    peak = DSD0_1 - base
  ) %>% 
  filter(!is.na(DSD0_min) & date %in% ds_sub$date)

PPC_DS_2 <- merge(ds_peak,PPC)
PPC_DS$PPC_roll <- RcppRoll::roll_mean(PPC_DS$PPC,3,fill=NA)
PPC_DS_2$PPC_roll <- RcppRoll::roll_mean(PPC_DS_2$PPC,3,fill=NA)


###############################################################################
###############################################################################
#Figure 6 Ds profile over time
###############################################################################
###############################################################################


mean(subset(ds_sub,method=="drift"&id==2)$Fz_roll/subset(ds_sub,method=="drift"&id==1)$Fz_roll,na.rm=T)
mean(subset(ds_sub,method=="SWC_T"&id==2)$Fz_roll/subset(ds_sub,method=="SWC_T"&id==1)$Fz_roll,na.rm=T)
mean(subset(F_sub,method=="drift")$Fz_2_roll/subset(F_sub,method=="drift")$Fz_1_roll,na.rm=T)



DS_boxplot <- 
  ggplot()+
  geom_boxplot(data=subset(ds_soil_agg),aes(fill="feps",middle=PTF_median_median,ymin=PTF_min_max,ymax=PTF_max_min,lower=PTF_q25_max,upper=PTF_q75_min,x=id),stat="identity",width=0.2,position=position_nudge(x=-0.25))+
  #geom_boxplot(data=ds_snopt,aes(fill="drift",DSD0_roll,x=id),width=0.2,position = position_nudge(x = 0))+
  geom_boxplot(data=ds_snopt,aes(fill="drift",col="drift",DSD0_roll,x=id),width=0.2,position = position_nudge(x = 0),alpha=0.3)+
  #geom_boxplot(data=subset(ds_soil,method=="drift"),aes(fill=as.factor(method),col=method,DSD0_roll_aniso,x=id),width=0.2,position = position_nudge(x = 0),alpha=0.3)+
  geom_boxplot(data=subset(ds_soil,method=="SWC_T"),aes(fill=as.factor(method),col=method,ifelse(DSD0_roll_aniso > 1, DSD0_roll_aniso,DSD0_roll_aniso),x=id),width=0.2,position = position_nudge(x = 0.25),alpha=0.3)+
  facet_wrap(~windy)+
#  scale_fill_brewer(breaks=c("feps","drift","SWC_T"),type="qual",palette=2,labels=c(expression(f(epsilon)),"ref adj","SWC T"))+
  scale_fill_manual(breaks=c("feps","drift","SWC_T"),values=c("grey",meth_col),labels=c(expression(f(epsilon)),method_labels))+
  scale_color_manual(limits=c("feps","drift","SWC_T"),values=c("black",meth_col),labels=c(expression(f(epsilon)),method_labels))+
  scale_x_discrete(limits=factor(1:2),labels=c("0-10 cm","10-20 cm"))+
  labs(x="",y="exchange coefficient",fill="method",col="method")+
  guides(col = guide_legend(override.aes = list(alpha=c(1,0.2,0.2))))+
    theme(legend.text.align = 0)
DS_boxplot
leg1 <- get_legend(DS_boxplot)
legend1 <- as_ggplot(leg1)
#names(DS_long_roll)
# ggplot()+
#   geom_line(data=ds_snopt,aes(date,DSD0_roll,group=id,col="SNOPT"))+
#   geom_line(data=ds_soil,aes(date,DSD0_roll_aniso,group=paste(id,method),col=method))+
#   facet_wrap(~windy)


######################################
#Flux
######################################
names(ds_snopt)

flux_plot <- 
  ggplot(subset(Kammer_flux_agg,!is.na(Versuch)))+
  geom_linerange(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max,col=""))+
  geom_point(aes(date,CO2flux,col=""))+
  labs(col="chamber")+
  scale_color_manual(values=c(1))+
  ggnewscale::new_scale_color()+
  geom_line(data=subset(soil_wide,!is.na(Versuch)),aes(date,zoo::rollapply(R_soil,20,mean,fill=NA),col=""),linetype=2)+
  geom_ribbon(data=subset(soil_wide,!is.na(Versuch)),aes(x=date,ymin=R_min,ymax=R_max,fill=""),alpha=0.15)+
  scale_color_manual("T & SWC model",values=grey(0.3))+
  scale_fill_manual("T & SWC model",values=grey(0.3))+
  ggnewscale::new_scale_color()+
  ggnewscale::new_scale_fill()+
    geom_ribbon(data=subset(ds_sub, !is.na(calm_id)),aes(x=date,ymin=Fz_min_roll_aniso,ymax=Fz_max_roll_aniso,fill=method2,group=paste(id,calm_id,method)),alpha=0.2)+
    #geom_ribbon(data=ds_sub,aes(x=date,ymin=Fz_min_roll,ymax=Fz_max_roll,fill=method2,group=paste(id,method)),alpha=0.1)+
    geom_line(data=ds_snopt,aes(date,Fz_roll_0_10,group=id,col="ref adj",linetype="windy"))+
    geom_line(data=ds_snopt,aes(date,Fz_roll_0_10,group=id,col="ref adj",alpha=calm,linetype="calm"))+
    #geom_ribbon(data=subset(ds_sub,id==1&method=="drift"),aes(date,ymin=Fz_roll_aniso,ymax=Fz_roll_aniso,col="balm",fill="balm"))+
    #geom_ribbon(data=subset(ds_sub,id==1&method=="drift"),aes(date,ymin=Fz_roll_aniso,ymax=Fz_roll_aniso,col="aalm",fill="aalm"))+
    geom_line(data=subset(ds_sub,method=="SWC_T"),aes(date,Fz_roll_aniso,group=paste(id,method),col=method2,linetype="windy"))+
    geom_line(data=subset(ds_sub,method=="SWC_T"),aes(date,Fz_roll_aniso,group=paste(id,method),col=method2,alpha=calm,linetype="calm"))+
    geom_line(data=subset(ds_sub,method=="drift" & id ==2),aes(date,Fz_roll_aniso,group=paste(id,method),col=method2,linetype="windy"))+
    geom_line(data=subset(ds_sub,method=="drift" & id ==2),aes(date,Fz_roll_aniso,group=paste(id,method),col=method2,alpha=calm,linetype="calm"))+

    
    
  directlabels::geom_dl(data=subset(ds_sub,id < 3 & method == "drift" & Versuch == 3),aes(date,Fz_roll,label=factor(id,levels = 1:2,labels=c("0 - 10 cm", "10 - 20 cm"))),method = list(directlabels::dl.trans(x = x - 0), "last.qp", cex = 0.6))+
  #scale_linetype_manual(values = c(0,1,0,2),labels=c("","","calm","windy"))+
  guides(alpha=F,linetype=F)+
   # guides(fill= guide_legend("gradient method\n ",
   #                            override.aes = list(
   #                              fill=rep(meth_col,each=2),
   #                              col=rep(meth_col,each=2),
   #                              #linetype=rep(1:2,2),
   #                              alpha=0.2
   #                              ),ncol=2))+
  scale_x_datetime(date_label="%b %d",breaks="2 days")+
  ylim(c(0,max(ds_sub$Fz_min_roll)))+
  scale_color_manual(method_exp,limits=paste0(rep(method_labels,each=2),rep(c("","windy"),2)),values = rep(meth_col,each=2),labels=c("","","calm","windy"))+
  scale_fill_manual(method_exp,limits=paste0(rep(method_labels,each=2),rep(c("","windy"),2)),values = c(meth_col[1],NA,meth_col[2],NA),labels=c("","","calm","windy"))+
  
  #scale_fill_manual(values = rep(meth_col,2))+
  guides(col=guide_legend(override.aes = list(linetype=c(1,2,1,2)),ncol=2,order=1),
         fill=guide_legend(order=1))+
  facet_wrap(~Versuch,scales="free_x")+
  labs(x="",y=expression(F[CO2]~"["*mu * mol ~ m^{-2} ~ s^{-1}*"]"),linetype="",col="gradient method",fill="gradient method")+
  geom_errorbar(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max),col=1,width=10000)+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  geom_vline(data=data.frame(x=c(max(subset(soil_wide,Versuch==2)$date),min(subset(soil_wide,Versuch==3)$date)),Versuch=2:3),aes(xintercept=x),linetype="dashed")


flux_plot
#theme(panel.border = element_rect(fill=NA))
#geom_vline(data=data.frame(x=c(max(subset(soil_wide,Versuch==2)$date),min(subset(soil_wide,Versuch==3)$date)),Versuch=2:3),aes(xintercept=x),linetype="dashed")
soil_wide$period <- soil_wide$Versuch-1
flux_adj <- adj_grob_size(flux_plot,subset(soil_wide,!is.na(period)),breaks="2 days",date_labels="%b %d",plot=F)
#cowplot::ggdraw()+cowplot::draw_plot(flux_adj)#+cowplot::draw_text(c("0-10 cm","10-20 cm"),x=0.9,y=0.7,size=10)




# ggplot(subset(ds_sub,method=="drift"))+
#   geom_line(aes(date,DSD0_aniso,col=id),alpha=0.3)+
#   geom_line(aes(date,DSD0_roll*1.26,col=id))+
#   geom_line(data=ds_snopt,aes(date,DSD0_roll,group=id))
# 
################################
#Wind DS
axis_fac <- 10

range7 <- range(subset(DS_long_roll,Versuch == 2)$date)
range8 <- range(subset(DS_long_roll,Versuch == 3)$date)
data$Versuch <- NA
data$Versuch[data$date > range7[1] & data$date < range7[2]] <- 2
data$Versuch[data$date > range8[1] & data$date < range8[2]] <- 3


col_labs <- c("DSD0","DeffD0" ,"DPPED0")
col_labs2 <- paste0(col_labs,2)
col_exps <- c(expression(D[S]/D[0]),expression(D[eff]/D[0]),expression(D[PPE]/D[0]))

PPC_DS_sub <- subset(PPC_DS,date %in% ds_sub$date)
PPC_DS_2_sub <- subset(PPC_DS_2,date %in% ds_sub$date & method %in% c("SWC_T"))

PPC_DS_plot <- 
  ggplot(subset(data,!is.na(Versuch)))+
  geom_ribbon(data=subset(ds_sub,method%in%c("drift","SWC_T") & id == 1),aes(x=date,ymin=DSD0_min_roll_aniso,ymax=DSD0_max_roll_aniso,fill=factor(method,levels=c("drift","SWC_T"),labels=paste0("DeffD0",c("",2)))),alpha=0.2,linetype=2)+
  
  geom_line(data=PPC_DS_2_sub,aes(x=date,y=base,col="DSD02"),linetype=2)+
  #geom_line(data=PPC_DS_2_sub,aes(date,PPC,col="PPC2"))+
  geom_line(data=PPC_DS_2_sub,aes(date,peak,col="DPPED02"),linetype="11",lwd=0.8)+
  geom_line(data=PPC_DS_2_sub,aes(date,DSD0_1,col="DeffD02"))+
  
  geom_line(data=PPC_DS_sub,aes(x=date,y=base,col="DSD0"),linetype=2)+
  geom_line(data=PPC_DS_sub,aes(date,PPC_roll,linetype="PPC"),col=grey(0.2))+
  geom_line(data=PPC_DS_sub,aes(date,peak,col="DPPED0"),linetype="11",lwd=0.8)+
  geom_line(data=PPC_DS_sub,aes(date,DSD0_1,col="DeffD0"))+
  #geom_line(data=subset(ds_sub,id == "1" & method == "drift"),aes(date,DSD0_roll_aniso,col="DSD0" ))+
  #geom_line(data=subset(ds_sub,id == "1"),aes(date,DSD0_roll,col=method ))#+
  scale_x_datetime(date_label="%b %d",breaks="1 days")+
  scale_y_continuous(sec.axis = sec_axis(trans=~.,name=expression("PPC [Pa s"^{-1}*"]")))+
  #scale_fill_manual("",limits=col_labs,values = c(2,2,NA),labels=col_exps)+
  #scale_fill_manual("depth: 0-10 cm",limits=col_labs,values = c(NA,2,NA,NA),labels=col_exps)+
  scale_fill_manual(expression(atop("depth 0-10 cm","ref"["adj"]~" stand-alone")),limits=c(col_labs,col_labs2),values = c(NA,meth_col[1],NA,NA,meth_col[2],NA),labels=c(rep("",3),col_exps))+
  scale_color_manual(expression(atop("depth 0-10 cm","ref"["adj"]~" stand-alone")),limits=c(col_labs,col_labs2),values = c(meth_col[1],meth_col[1],"red",meth_col[2],meth_col[2],"blue"),labels=c(rep("",3),col_exps))+
  #scale_color_manual("depth: 0-10 cm",limits=col_labs,values = c(scales::hue_pal()(1),scales::hue_pal()(1),"red",grey(0.2),NA),labels=col_exps)+
  facet_wrap(~factor(Versuch,levels=c("2","3"),labels = c("windy period","calm period")),scales="free_x")+
  guides(col=guide_legend(ncol=2,override.aes = list(linetype=c("dashed","solid","11","dashed","solid","11"),lwd=c(0.6,0.7,0.8,0.6,0.7,0.8)),order=1),
         fill=guide_legend(ncol=2,order=1),
         linetype=guide_legend(order = 2)
         )+
  #guides(fill=guide_legend(override.aes = list(linetype=c(2,1,1,1))))+
  labs(y="exchange coefficient",x="",linetype="")+
  theme(legend.text.align = 0,panel.border = element_rect(fill=NA),legend.title = element_text(size=10),legend.title.align = 0)#+
#ggsave(paste0(plotpfad_harth,"DS_PPC_Inj1u2.jpg"),width=7,height = 5)
PPC_DS_plot



#PPC_DS$peak_rel <- PPC_DS$peak#/PPC_DS$base*100


cor(PPC_DS_sub$peak,PPC_DS_sub$PPC_roll,use = "complete")
cor(PPC_DS_2_sub$peak,PPC_DS_2_sub$PPC_roll,use = "complete")

fm_PPC_DPPE <- glm(peak~PPC_roll,data=subset(PPC_DS_sub,date %in% ds_sub$date))
R2 <- 1-(fm_PPC_DPPE$deviance/fm_PPC_DPPE$null.deviance)
intercept <- fm_PPC_DPPE$coefficients[1]
slope <- fm_PPC_DPPE$coefficients[2]
0.5108^2

# fm_PPC_drift <- glm(peak~PPC,data=subset(PPC_DS_2_sub,method=="drift"))
#R2_drift <- 1-(fm_PPC_drift$deviance/fm_PPC_drift$null.deviance)

fm_PPC_SWC_T <- glm(peak~PPC_roll,data=subset(PPC_DS_2_sub,method=="SWC_T"))
R2_SWC_T <- 1-(fm_PPC_SWC_T$deviance/fm_PPC_SWC_T$null.deviance)

DPPE_PPC <- 
  ggplot(PPC_DS_sub)+
  geom_point(aes(PPC_roll,peak),col=meth_col[1],fill=meth_col[1],alpha=0.6)+
  geom_point(data=PPC_DS_2_sub,aes(PPC_roll,peak),col=meth_col[2],fill=meth_col[2],alpha=0.6)+
  geom_smooth(data=PPC_DS_2_sub,aes(PPC_roll,peak,group=method),method = "glm",se=F,col="blue",lwd=0.6,linetype="dashed")+
  geom_smooth(aes(PPC_roll,peak),method = "glm",se=F,col="red",lwd=0.6,linetype="dashed",col="red")+
  annotate("text",x=0.28,y=0.1,col="red",label=paste("R² =",round(R2,2)))+
  annotate("text",x=0.28,y=0.05,col="blue",label=paste("R² =",round(R2_SWC_T,2)))+
  #annotate("text",x=-Inf,y=0.29,label=paste("y =",round(slope,2),"x - ",abs(round(intercept,2))),hjust=-0.1)+
  labs(y=expression(D[PPE]/D[0]~"(0-10 cm)"),x=expression("PPC [Pa s"^{-1}*"]"),col="")+
  #scale_color_manual("",limits=c("feps",method_labels),values=c("grey",meth_col),labels=c(expression(f(epsilon)),method_labels))+
  #scale_fill_manual("",limits=c("feps",method_labels),values=c("grey",meth_col),labels=c(expression(f(epsilon)),method_labels))+
  guides(col = guide_legend(override.aes = list(shape=15,size=8)))+
  theme(legend.text.align = 0)

DPPE_PPC
slope
R2
intercept
p1 <- cowplot::ggdraw()+cowplot::draw_plot(PPC_DS_plot)+cowplot::draw_text("a)",x=0.03,y=0.97)
p2 <- cowplot::ggdraw()+cowplot::draw_plot(DS_boxplot+theme(legend.position = "none"))+cowplot::draw_text("b)",x=0.05,y=0.97)
p3 <- cowplot::ggdraw()+cowplot::draw_plot(DPPE_PPC)+cowplot::draw_text("c)",x=0.05,y=0.97)
p4 <- cowplot::ggdraw()+cowplot::draw_plot(flux_adj)+cowplot::draw_text("d)",x=0.03,y=0.97)#+cowplot::draw_text("0-10    10-20 cm",x=0.82,y=0.92,size=9,hjust=0,vjust=0)#+

gg23 <- ggpubr::ggarrange(DS_boxplot,DPPE_PPC,ncol = 2,widths = c(1.5,1),common.legend = T,legend = "right")
p23 <- cowplot::ggdraw()+cowplot::draw_plot(gg23)+
  cowplot::draw_text("b)",x=0.03,y=0.97)+
  cowplot::draw_text("c)",x=0.53,y=0.97)
#rel_grid(0.01,color="grey")+
#rel_grid(0.1)
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g23 <- ggplotGrob(p23)
g4 <- ggplotGrob(p4)


fg2 <- egg::gtable_frame(g2, debug = F)
fg3 <- egg::gtable_frame(g3, debug = F)


fg23 <-
  egg::gtable_frame(gridExtra::gtable_cbind(fg2, fg3),
                    width = unit(2, "null"),
                    height = unit(1, "null"))

fg23 <- egg::gtable_frame(g23,
                          width = unit(1, "null"),
                          height = unit(1, "null"),
                          debug = F)
fg1 <-
  egg::gtable_frame(
    g1,
    width = unit(1, "null"),
    height = unit(1, "null"),
    debug = F
  )
fg4 <-
  egg::gtable_frame(
    g4,
    width = unit(1, "null"),
    height = unit(1, "null"),
    debug = F
  )
combined <- gridExtra::gtable_rbind(fg1, fg23,fg4) 
jpeg(file=paste0(plotpfad_ms,"Fig_6_PPC_DS_WS.jpg"),width=7,height = 7.5,units="in",res=300)
grid::grid.newpage()
grid::grid.draw(combined)
dev.off()
