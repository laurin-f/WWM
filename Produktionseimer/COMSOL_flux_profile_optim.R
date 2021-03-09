detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
check.packages(c("ggplot2","lubridate","stringr","dplyr","units","ggstance"))

#Pfade
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
arduinopfad<-paste0(hauptpfad,"Daten/Urdaten/Arduino/")
datapfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Arduino/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_prod<- paste0(hauptpfad,"Daten/Metadaten/Produktionseimer/")
metapfad_comsol<- paste0(hauptpfad,"Daten/Metadaten/COMSOL/")
aufbereitetpfad_prod<- paste0(hauptpfad,"Daten/aufbereiteteDaten/Produktionseimer/")
plotpfad_prod <- paste0(hauptpfad,"Dokumentation/Berichte/plots/produktionseimer/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")

#cal <- "_new_cal"
cal <- ""
load(file=paste0(aufbereitetpfad_prod,"data_agg",cal,".RData"))
load(file=paste0(aufbereitetpfad_prod,"comsol.RData"))


meas_depths_2 <- seq(0,40,0.5)
meas_points_2 <- data.frame(R=4,Z=meas_depths_2)
write.table(meas_points_2,file = paste0(metapfad,"COMSOL/meas_points_produktionseimer_r3.txt"),row.names = F,col.names = F)

  
  

  grundflaeche <- set_units((15)^2*pi,"cm^2")
  #prod pro fläche nicht auf Fgrundfläche sondern auf 
  l <- set_units(1,"mm")
  r1u2 <- 50+0:2*40
  r3 <- 20+0:2*40
  r <- set_units(matrix(c(r1u2,r1u2,r3),3,3),"mm")
  G <- pi * (r+l/2)^2 - pi * (r-l/2)^2
  M <- l * 2 * pi * (r+l/2) + l * 2 * pi * (r-l/2)
  A <- colSums(2 * G + M)#mm^2
  A <- change_unit(A,unit_in = "mm^2",unit_out = "m^2")
  names(A) <- paste0("prod_",1:3,"_m2")
  
  
  i<-7
comsol_opt_ls <- vector("list",length(unique(data_agg$ID)))
for(i in unique(data_agg$ID)){
  sub_i <- subset(data_agg,ID == i)
  for(j in 1:7){
    write.table(sub_i[sub_i$tiefe == (1:7*-3.5)[j],"CO2_mol_per_m3"],paste0(metapfad_comsol,"dom",j,".csv"),col.names = F,row.names = F,sep=",")
  }
  
  DSD0 <- 0.26
  DS <- DSD0 * D0_T_p(T_C=mean(sub_i$temp,na.rm = T),unit = "m^2/s")
  file_i <- paste0("CO2_flux_prod_optim_DSD0=",DSD0,"_ID=",i,cal,".txt")
  names(DS) <- "DS"
  comsol_exe(model="Produktionseimer_optim",input_pars = DS,outfile_new = file_i,outfile_raw = "CO2_flux_prod_optim.txt",job="b2"
####################################             
             ,overwrite = F)
####################################
  comsol_opt_ls[[i]] <- read.csv(paste0(comsolpfad,file_i),skip=9,sep="",header=F)
  colnames(comsol_opt_ls[[i]]) <- c("r","z","c","flux",paste0("prod_",1:3),"CO2_atm")
  comsol_opt_ls[[i]]$ID <- i
}

comsol_opt <- do.call(rbind,comsol_opt_ls)

comsol_opt$tiefe <- comsol_opt$z - 40
comsol_opt$flux_mumol <- comsol_opt$flux * 10^6
comsol_opt$treat <- as.character(factor(comsol_opt$ID,levels=data_agg$ID,labels = data_agg$treat))
comsol_opt[,paste0("prod_mumol_",1:3)] <- comsol_opt[,paste0("prod_",1:3)] * 10^6 / change_unit(rep(grundflaeche,each=nrow(data_agg)),unit_out = "m^2") * rep(A,each=nrow(comsol_opt))
  
prod_comsol <- comsol_opt %>% 
  group_by(ID,treat) %>% 
  select(matches("prod_mumol")) %>% 
  summarise_at(vars(matches("prod")),mean) %>% 
  tidyr::pivot_longer(matches("prod"),names_prefix = "prod_mumol_",names_to="tiefenstufe",values_to="prod_comsol")

prod_meas <- data_agg %>% 
  group_by(ID,treat) %>% 
  select(matches("prod_\\d_mumol")) %>% 
  summarise_at(vars(matches("prod")),mean) %>% 
  tidyr::pivot_longer(matches("prod"),names_pattern = "prod_(\\d)",names_to="tiefenstufe",values_to="prod")

prod_comsol_2 <- prod_comsol %>% rename(prod = prod_comsol) %>% mutate(method = "mod")
prod_meas_2 <- prod_meas %>% mutate(method = "inj") 

prod <- merge(prod_comsol,prod_meas)
prod_long <- rbind(prod_meas_2,prod_comsol_2)

prod$tiefenstufe <- as.numeric(prod$tiefenstufe)

prod_df$tiefe[prod_df$tiefe == "30"] <- "40"


###############################################
#                PLOTS                        #
###############################################

###############################################
#flux
ggplot()+
  geom_line(data=subset(comsol),aes(flux_mumol,tiefe,col=as.factor(ID),linetype="comsol"),orientation = "y")+
  geom_line(data=subset(comsol_opt),aes(flux_mumol,tiefe,col=as.factor(ID),linetype="comsol_opt"),orientation = "y")+
  geom_point(data=subset(data_agg2),aes(as.numeric(Fz),prod_tiefe,col=as.factor(ID)))+
  geom_line(data=prod_df,aes(Fz,-as.numeric(tiefe),col=as.factor(ID),linetype="theory"),orientation = "y")+
  facet_wrap(~treat)+
  ggsave(paste0(plotpfad_prod,"Flux_profil_comsol_optim",cal,".png"),width=9,height=7)

##############################
#CO2
ggplot()+
  geom_line(data=subset(comsol_opt),aes(ppm_to_mol(c,unit_in = "mol/m^3"),tiefe,col=as.factor(ID),linetype="comsol_opt"),orientation = "y")+
  geom_line(data=subset(comsol),aes(ppm_to_mol(c,unit_in = "mol/m^3"),tiefe,col=as.factor(ID),linetype="comsol"),orientation = "y")+
  geom_point(data=subset(data_agg),aes(CO2,tiefe,col=as.factor(ID)))+
  facet_wrap(~treat,scales = "free")+
  labs(x=expression(CO[2]~"[ppm]"),y="depth [cm]")+
  ggsave(paste0(plotpfad_prod,"CO2_profil_comsol_optim",cal,".png"),width=9,height=7)


###################
#prod


ggplot(subset(prod_long))+
  geom_col(aes(x=method,y=prod,fill=as.factor(tiefenstufe),col=method),orientation="x",width = 0.4)+
  scale_color_manual(values=c(1,NA))+
  facet_wrap(~ ID,scales = "free")+
  labs(y=expression("P ["~mu*"mol m"^{-2}*s^{-1}*"]"),y="",col="")#+

prod_long$treat2 <- (factor(prod_long$treat,
                           levels=c("1_0_0", 
                                    "0_1_0", 
                                    "0_0_1", 
                                    "1_1_0", 
                                    "1_0_1", 
                                    "0_1_1", 
                                    "1_1_1", 
                                    "0_0_0"),
                           labels = c("1","2","3","1 + 2","1 + 3","2 + 3","1 + 2 + 3","")))

cols <- (scales::hue_pal()(3))
prod_barplt <- 
  ggplot(subset(prod_long,ID %in% c(1,4,5,6,7,13,16)))+
  geom_col(aes(x=method,y=prod,fill=as.factor(tiefenstufe),col=method),orientation="x",width = 0.4)+
  scale_color_manual(values=c(1,"grey"))+
  facet_grid(. ~ treat2)+
  labs(y=expression(P[CO2]*" ["~mu*"mol m"^{-2}*s^{-1}*"]"),x="",fill="depth level",col="method")+
  guides(col=guide_legend(override.aes = list(fill=NA)))+
  scale_fill_manual(values=(cols))+
  theme_minimal()+
  theme(axis.line = element_line())+
  ggsave(paste0(plotpfad_prod,"Produktion_comsol",cal,".png"),width=6,height=3) 

ggplot(subset(prod_long,!ID %in% c(1,4,5,6,7,13,14,16,17)))+
  geom_col(aes(x=method,y=prod,fill=as.factor(tiefenstufe),col=method),orientation="x",width = 0.4)+
  scale_color_manual(values=c(1,NA))+
  facet_grid(. ~ treat)+
  labs(y=expression(P[CO2]*" ["~mu*"mol m"^{-2}*s^{-1}*"]"),x="",fill="depth level",col="method")+
  guides(col=guide_legend(override.aes = list(fill=NA)))+
  ggsave(paste0(plotpfad_prod,"Produktion_comsol_replicates",cal,".png"),width=9,height=4) 






prod_agg <- prod %>% group_by(ID) %>% summarise_at(vars(matches("prod")),sum)
R2_depths <- prod %>% group_by(tiefenstufe) %>% summarise(R2=R2(prod,prod_comsol))

R2_tiefen <- R2(prod$prod,prod$prod_comsol)
R2_ges <- R2(prod_agg$prod,prod_agg$prod_comsol)

mod_obs <- ggplot(prod)+
  geom_point(data=prod_agg,aes(prod_comsol,prod,col="total"))+
  #scale_color_manual(expression(P[CO2]~"total"),values=1)+
  #ggnewscale::new_scale_color()+
  geom_point(aes(prod_comsol,prod,col=as.factor(tiefenstufe)))+geom_abline(slope=1)+
  #guides(col=F)+
  scale_color_manual(values=c(cols,1))+
  annotate("text",x=0,y=3,label=paste("R² depths",round(R2_tiefen,2),"\nR² total",round(R2_ges,2)),hjust=0)+
  labs(y=expression("inj ["~mu*"mol m"^{-2}*s^{-1}*"]"),x=expression("mod ["~mu*"mol m"^{-2}*s^{-1}*"]"),col="depth")+
  theme_classic()+
  theme(legend.position = "left")+
  ggsave(paste0(plotpfad_prod,"prod_mod_obs_left",cal,".png"),width=4,height=3)


leg1 <- ggpubr::get_legend(mod_obs)
legend1 <- ggpubr::as_ggplot(leg1)
leg2 <- ggpubr::get_legend(prod_barplt)
legend2 <- ggpubr::as_ggplot(leg2)
leg <- ggpubr::ggarrange(legend2,legend1,ncol=1,heights = c(1,1),align = "v")



prod_mod_obs_no_leg <- egg::ggarrange(prod_barplt+theme(legend.position = "none"),mod_obs+theme(legend.position = "none"),widths = c(5,1),draw=F)
ggpubr::ggarrange(prod_mod_obs_no_leg,leg,widths=c(10,1))+ggsave(paste0(plotpfad_prod,"prod_barplt_mod_obs",cal,".png"),width=11,height=4)

#ggpubr::ggarrange(prod_barplt,mod_obs,widths=c(3,1),common.legend = T,legend = "right")+ggsave(paste0(plotpfad_prod,"prod_barplt_mod_obs",cal,".png"),width=11,height=4)
