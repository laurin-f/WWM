#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","ggforce","units")

check.packages(packages)

#data_agg
load(paste0(samplerpfad,"tracereinspeisung_sandkiste_agg.RData"))


###################################
#
CO2_obs <- subset(data_agg, ID == "PSt_5_Nr_4nein")
CO2_obs$CO2_mol_per_m3 <- ppm_to_mol(CO2_obs$CO2)
CO2_mod <- read.table(paste0(comsolpfad,"CO2_mod.txt"),skip=9,col.names = c("r","z","CO2_mol_per_m3"))
CO2_mod$tiefe <- CO2_mod$z - 37
CO2_mod$CO2 <- ppm_to_mol(CO2_mod$CO2_mol_per_m3,"mol/m^3")

#####################################
#sweep mit CO2_atm
# CO2_mod_sweep <- readLines(paste0(comsolpfad,"CO2_mod_sweep_DS_CO2atm.txt"))
# 
# colnames_sweep_raw <- str_extract_all(CO2_mod_sweep[9],"R|Z|DS=\\d(\\.\\d+)?E-\\d m\\^2/s, CO2_atm=\\d\\.\\d+",simplify = T)
# 
# colnames_sweep <- str_replace_all(colnames_sweep_raw,c(" m\\^2/s, "="_"))
# 
# CO2_sweep_mat <- str_split(CO2_mod_sweep[10:length(CO2_mod_sweep)],"\\s+",simplify = T)
# CO2_sweep <- as.data.frame(apply(CO2_sweep_mat,2,as.numeric))
# colnames(CO2_sweep) <- colnames_sweep
# 
# sweep_long <- reshape2::melt(CO2_sweep,id=1:2,value.name="CO2_mol_per_m3",variable="par")
# 
# sweep_long$DS <- as.numeric(str_extract(sweep_long$par,"(?<=DS=)\\d(\\.\\d+)?E-\\d"))
# sweep_long$CO2_atm <- as.numeric(str_extract(sweep_long$par,"(?<=CO2_atm=)\\d\\.\\d+"))
# sweep_long$CO2_mod <- ppm_to_mol(sweep_long$CO2_mol_per_m3,"mol/m^3","units")
# CO2_obs$CO2_ppm <- ppm_to_mol(CO2_obs$CO2_mol_per_m3,"mol/m^3","units")
# sweep_long$tiefe <- set_units(sweep_long$Z - 40,cm)
# 
# sweep_long$CO2_obs <- as.numeric(as.character(factor(sweep_long$tiefe,levels = CO2_obs$tiefe,labels = CO2_obs$CO2_ppm)))
#####################################
#sweep ohne CO2_atm
CO2_mod_sweep <- readLines(paste0(comsolpfad,"CO2_mod_sweep.txt"))

colnames_sweep <- str_extract_all(CO2_mod_sweep[9],"r|z|DS=\\d(\\.\\d+)?(E-\\d)?",simplify = T)


CO2_sweep_mat <- str_split(CO2_mod_sweep[10:length(CO2_mod_sweep)],"\\s+",simplify = T)
CO2_sweep <- as.data.frame(apply(CO2_sweep_mat,2,as.numeric))
colnames(CO2_sweep) <- colnames_sweep

sweep_long <- reshape2::melt(CO2_sweep,id=1:2,value.name="CO2_mol_per_m3",variable="par")

sweep_long$DS_sand <- as.numeric(str_extract(sweep_long$par,"(?<=DS=)\\d(\\.\\d+)?(E-\\d)?"))
sweep_long$CO2_mod <- ppm_to_mol(sweep_long$CO2_mol_per_m3,"mol/m^3","units")
sweep_long$tiefe <- set_units(sweep_long$z- 37,cm)

sweep_long$CO2_obs <- set_units(as.numeric(as.character(factor(sweep_long$tiefe,levels = CO2_obs$tiefe,labels = CO2_obs$CO2))),ppm)


#Alle sweeps
ggplot(sweep_long)+geom_line(aes(y=tiefe,x=CO2_mod,col=as.factor(DS_sand)))+guides(col=F)+geom_point(aes(y=tiefe,x=CO2_obs))


######################
#fm


tiefen <- 1:8
rmse <- apply(CO2_sweep[tiefen,-(1:2)],2,RMSE,CO2_obs$CO2_mol_per_m3[tiefen])
best.fit.char <- names(which.min(rmse))
best_DS <- as.numeric(str_extract(best.fit.char,"(?<=DS=)\\d(\\.\\d+)?E-\\d"))
best_CO2_atm <- as.numeric(str_extract(best.fit.char,"(?<=CO2_atm=)\\d\\.\\d+"))

CO2_obs$DS
CO2_obs$DS_D0_CO2
D0_CO2 <- D0_T_p(15) #18°C cm/s
D0_CO2_m2 <- D0_CO2/10^4 #m2/s
DS_D0 <- best_DS/D0_CO2_m2 #m2/s
DS_D0


#sweep_long$CO2_mod <- as.numeric(sweep_long$CO2_mod)
#sweep_long$tiefe <- as.numeric(sweep_long$tiefe)
#CO2_obs$tiefe <- as.numeric(CO2_obs$tiefe)
#CO2_obs$CO2_ppm <- as.numeric(CO2_obs$CO2_ppm)
ggplot(subset(sweep_long,DS_sand==best_DS))+
  geom_line(aes(y=tiefe,x=CO2_mod,col="mod"))+geom_point(data=CO2_obs,aes(y=tiefe,x=CO2,col="obs"))+
  annotate("text",y= max(CO2_obs$tiefe),x=max(CO2_obs$CO2,na.rm=T),label=paste("DS/D0 = ",round(DS_D0,3),"\nliterature reference: \nDS/D0 = 0.239"),hjust=0.9,vjust=1)+
  labs(x="CO[2]",y="depth",col="")+ggsave(paste0(plotpfad,"comsol_mod_ob_sand.png"),width=4,height=4)
  

ggplot(subset(sweep_long,DS_sand==2.8e-6 & CO2_atm == 0.0168))+geom_line(aes(tiefe,CO2_mod,col=as.factor(CO2_atm)))+geom_point(data=CO2_obs,aes(tiefe,CO2_ppm))

residuals<-apply(CO2_sweep[,-(1:2)],2,"-",CO2_obs$CO2_mol_per_m3)


df <- data.frame(trt = c("a", "b", "c"), resp = c(2, 3, 4))



CO2_mod$tiefe <- CO2_mod$z - 40


CO2_mod$CO2 <- CO2_mod$CO2_mol_per_m3 / mol_per_m3 *10^6

model <- subset(test,r== 0 & z %in% meas_depths)

model$tiefe <- model$z - 40
ggplot(test)+geom_point(aes(r,z,col=log(CO2)))+scale_color_viridis_c()
ggplot(test)+geom_point(aes(z,log(CO2)))
ggplot(model)+geom_point(aes(CO2,z))  


ggplot()+
  geom_point(data = subset(data,Versuch == 3), aes(CO2_rollapply, tiefe, col=PSt_Nr))+
  geom_point(data=CO2_mod,aes(CO2,tiefe))+
  labs(x="CO2 [ppm]",y="tiefe [cm]")

ggplot()+
  geom_point(data=CO2_mod,aes(CO2,tiefe,col="mod"))+
  geom_point(data=CO2_obs,aes(CO2,tiefe,col="obs"))
