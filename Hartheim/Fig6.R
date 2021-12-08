

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
load(paste0(datapfad_harth,"DS_long_list_SWC_drift_minmax.RData"))

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

DS_long[,grep("DSD0",colnames(DS_long))] <- DS_long[,grep("DSD0",colnames(DS_long))] * 1.26

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



soil_agg_plot$date <- round_date(soil_agg_plot$date,"mins")
ds_soil <- merge(ds_sub[,c("date","Versuch","DSD0_roll","id","method")],soil_agg_plot) 
ds_soil$Versuch <- as.numeric(ds_soil$Versuch)
ds_soil$windy <- factor(ds_soil$Versuch,levels = 2:3,labels=c("windy period","calm period"))
PPC_DS$windy <- factor(PPC_DS$Versuch,levels = 2:3,labels=c("windy period","calm period"))

ds_soil_agg <- ds_soil %>% group_by(id,windy) %>% summarise_all(mean)

ds_sub <- subset(ds_sub, method != "SWC_WS" & id < 3)

ds_sub$method2 <- factor(ds_sub$method,levels = c("drift","SWC_T"),labels=c("ref adj","stand alone"))


calm_windy <- diff(ds_sub$calm)
ds_sub$calm_id <- NA
start <- c(1,which(calm_windy == 1))
stop <- c(which(calm_windy == -1),nrow(ds_sub))
for(i in seq_along(start))  ds_sub$calm_id[start[i]:stop[i]] <- i



#meth_col <- RColorBrewer::brewer.pal(n = 3, name = "Dark2")[c(2,3)]
meth_col <- scales::hue_pal()(2)
###############################################################################
###############################################################################
#Figure 6 Ds profile over time
###############################################################################
###############################################################################


range(subset(ds_sub,id==1)$DSD0_roll,na.rm = T)
range(subset(ds_sub,id==2)$DSD0_roll,na.rm = T)
range(subset(ds_sub,id==3)$DSD0_roll,na.rm = T)
mean(F_sub$Fz_roll_10_17/F_sub$Fz_roll_0_10,na.rm=T)



DS_boxplot <- 
  ggplot()+
  geom_boxplot(data=subset(ds_soil_agg),aes(fill="feps",middle=PTF_median_median,ymin=PTF_min_max,ymax=PTF_max_min,lower=PTF_q25_max,upper=PTF_q75_min,x=id),stat="identity",width=0.2,position=position_nudge(x=-0.25))+
  geom_boxplot(data=subset(ds_soil,method=="drift"),aes(fill=as.factor(method),DSD0_roll,x=id),width=0.2,position = position_nudge(x = 0))+
  geom_boxplot(data=subset(ds_soil,method=="SWC_T"),aes(fill=as.factor(method),DSD0_roll,x=id),width=0.2,position = position_nudge(x = 0.25))+
  facet_wrap(~windy)+
#  scale_fill_brewer(breaks=c("feps","drift","SWC_T"),type="qual",palette=2,labels=c(expression(f(epsilon)),"ref adj","SWC T"))+
  scale_fill_manual(breaks=c("feps","drift","SWC_T"),values=c("grey",meth_col),labels=c(expression(f(epsilon)),"ref adj","SWC T"))+
  scale_x_discrete(limits=factor(1:2),labels=c("0-10 cm","10-20 cm"))+
  labs(x="",y="exchange coefficient",fill="method",col="in situ")+
  guides(col = guide_legend(override.aes = list(fill=scales::hue_pal()(2),alpha=0.2)))+
    theme(legend.text.align = 0)
DS_boxplot

######################################
#Flux
######################################


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
    geom_ribbon(data=subset(ds_sub, !is.na(calm_id)),aes(x=date,ymin=Fz_min_roll,ymax=Fz_max_roll,fill=method2,group=paste(id,calm_id,method)),alpha=0.2)+
    #geom_ribbon(data=ds_sub,aes(x=date,ymin=Fz_min_roll,ymax=Fz_max_roll,fill=method2,group=paste(id,method)),alpha=0.1)+
    geom_line(data=ds_sub,aes(date,Fz_roll,group=paste(id,method),col=method2,alpha=calm,linetype="calm"))+
    geom_line(data=ds_sub,aes(date,Fz_roll,group=paste(id,method),col=method2,linetype="windy"))+
    
  directlabels::geom_dl(data=subset(ds_sub,id < 3 & method == "drift" & Versuch == 3),aes(date,Fz_roll,label=factor(id,levels = 1:2,labels=c("0 - 10 cm", "10 - 20 cm"))),method = list(directlabels::dl.trans(x = x - 0), "last.qp", cex = 0.8))+

  guides(alpha=F)+
  # guides(col=F,fill=F,
  #        alpha= guide_legend("gradient method\n ",override.aes = list(fill=rep(cols[1:2],each=2)),ncol=2))+
  scale_x_datetime(date_label="%b %d",breaks="2 days")+
  ylim(c(0,8))+
  scale_color_manual(values = meth_col)+
  facet_wrap(~Versuch,scales="free_x")+
  labs(x="",y=expression(F[CO2]~"["*mu * mol ~ m^{-2} ~ s^{-1}*"]"),linetype="",col="method",fill="method")+
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
cowplot::ggdraw()+cowplot::draw_plot(flux_adj)#+cowplot::draw_text(c("0-10 cm","10-20 cm"),x=0.9,y=0.7,size=10)





################################
#Wind DS
axis_fac <- 10

range7 <- range(subset(DS_long_roll,Versuch == 2)$date)
range8 <- range(subset(DS_long_roll,Versuch == 3)$date)
data$Versuch <- NA
data$Versuch[data$date > range7[1] & data$date < range7[2]] <- 2
data$Versuch[data$date > range8[1] & data$date < range8[2]] <- 3


col_labs <- c("DSD0" ,"DSD0 peak","PPC")
col_exps <- c(expression(D[eff]/D[0]),expression(D[PPE]/D[0]),"PPC")


PPC_DS_plot <- 
  ggplot(subset(data,!is.na(Versuch)))+
  geom_ribbon(data=subset(PPC_DS,date %in% ds_sub$date),aes(x=date,ymax=DSD0_roll,ymin=base,fill="DSD0 peak"),alpha=0.2)+
  geom_line(data=subset(PPC_DS,date %in% ds_sub$date),aes(date,PPC,col="PPC"))+
  
  geom_line(data=subset(PPC_DS,date %in% ds_sub$date),aes(date,peak,col="DSD0 peak"))+
  #geom_line(data=subset(PPC_DS,date %in% ds_sub$date),aes(date,DSD0_1,col="DSD0"))+
  geom_ribbon(data=subset(ds_sub,method=="drift" & id == 1),aes(x=date,ymin=DSD0_min_roll,ymax=DSD0_max_roll,fill="DSD0",col="DSD0"),alpha=0.1,linetype=2)+
  geom_line(data=subset(ds_sub,id == "1" & method == "drift"),aes(date,DSD0_roll,col="DSD0" ))+
  #geom_line(data=subset(ds_sub,id == "1"),aes(date,DSD0_roll,col=method ))#+
  scale_x_datetime(date_label="%b %d",breaks="1 days")+
  #scale_y_continuous(limits = c(0,0.6),sec.axis = sec_axis(trans=~.,name=expression("PPC [Pa s"^{-1}*"]")))+
  scale_fill_manual("",limits=col_labs,values = c(2,2,NA),labels=col_exps)+
  #scale_fill_manual("",limits=col_labs,values = c(NA,2,NA),labels=col_exps)+
  scale_color_manual("",values = c(scales::hue_pal()(1),2,grey(0.2)),labels=col_exps)+
  facet_wrap(~factor(Versuch,levels=c("2","3"),labels = c("windy period","calm period")),scales="free_x")+
  labs(y="exchange coefficient",x="")+
  
  theme(legend.text.align = 0,panel.border = element_rect(fill=NA))#+
#ggsave(paste0(plotpfad_harth,"DS_PPC_Inj1u2.jpg"),width=7,height = 5)
PPC_DS_plot
ggplot(subset(ds_sub,method=="drift" & id ==1))+
  geom_line(aes(date,DSD0_roll))+
  geom_line(aes(date,DSD0_min_roll))+
  geom_line(aes(date,DSD0_max_roll))

names(PPC_DS)
PPC_DS$peak_rel <- PPC_DS$peak#/PPC_DS$base*100

PPC_DS_sub <- subset(PPC_DS,date %in% ds_sub$date)
cor(PPC_DS_sub$peak_rel,PPC_DS_sub$PPC,use = "complete")
fm_PPC_DPPE <- glm(peak_rel~PPC,data=subset(PPC_DS,date %in% ds_sub$date))
R2 <- 1-(fm_PPC_DPPE$deviance/fm_PPC_DPPE$null.deviance)
intercept <- fm_PPC_DPPE$coefficients[1]
slope <- fm_PPC_DPPE$coefficients[2]
DPPE_PPC <- 
  ggplot(subset(PPC_DS,date %in% ds_sub$date))+
  geom_point(aes(PPC,peak_rel,col="0-10 cm",fill="0-10 cm"),alpha=0.8)+
  geom_smooth(aes(PPC,peak_rel),method = "glm",se=F,col=1,lwd=0.6,linetype="dashed")+
  annotate("text",x=-Inf,y=0.25,label=paste("R² =",round(R2,2)),hjust=-0.1)+
  annotate("text",x=-Inf,y=0.29,label=paste("y =",round(slope,2),"x - ",abs(round(intercept,2))),hjust=-0.1)+
  labs(y=expression(D[PPE]/D[0]),x=expression("PPC [Pa s"^{-1}*"]"),col="")+
  scale_color_manual("",limits=c("0-10 cm","10-20 cm"),values=scales::hue_pal()(2))+
  scale_fill_manual("",limits=c("0-10 cm","10-20 cm"),values=scales::hue_pal()(2))+
  guides(col = guide_legend(override.aes = list(shape=15,size=8)))

DPPE_PPC
slope
R2
intercept
p1 <- cowplot::ggdraw()+cowplot::draw_plot(PPC_DS_plot)+cowplot::draw_text("a)",x=0.03,y=0.97)
p2 <- cowplot::ggdraw()+cowplot::draw_plot(DS_boxplot+theme(legend.position = "none"))+cowplot::draw_text("b)",x=0.05,y=0.97)
p3 <- cowplot::ggdraw()+cowplot::draw_plot(DPPE_PPC)+cowplot::draw_text("c)",x=0.05,y=0.97)
p4 <- cowplot::ggdraw()+cowplot::draw_plot(flux_adj)+cowplot::draw_text("d)",x=0.03,y=0.97)+cowplot::draw_text("0-10    10-20 cm",x=0.82,y=0.92,size=9,hjust=0,vjust=0)#+
#rel_grid(0.01,color="grey")+
#rel_grid(0.1)
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)


fg2 <- egg::gtable_frame(g2, debug = F)
fg3 <- egg::gtable_frame(g3, debug = F)


fg23 <-
  egg::gtable_frame(gridExtra::gtable_cbind(fg2, fg3),
                    width = unit(2, "null"),
                    height = unit(1, "null"))
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
jpeg(file=paste0(plotpfad_ms,"Fig_6_PPC_DS.jpg"),width=7,height = 7.5,units="in",res=300)
grid::grid.newpage()
grid::grid.draw(combined)
dev.off()