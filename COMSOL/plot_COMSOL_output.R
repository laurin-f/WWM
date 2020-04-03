#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

meas_depths <- (40-(1:7*3.5))
meas_points <- data.frame(R=0,Z=meas_depths)
#write.table(meas_points,file=paste0(comsolpfad,"meas_points.txt"),row.names = F,col.names = F)

mesh <- read.table(paste0(comsolpfad,"conc_test.txt"),skip=9,col.names = c("r","z","CO2_mol_per_m3"))
id<-rep(NA,length(meas_depths))
for(i in seq_along(meas_depths)){
  id[i]<-which( mesh$r < 0.1 & mesh$r > 0 & abs((mesh$z) - meas_depths[i]-0.1)< 0.2)
}
  mesh[id,]

CO2_obs <- read.table(paste0(comsolpfad,"PSt_5_Nr_3.txt"),col.names = c("z","CO2_mol_per_m3"))
CO2_mod <- read.table(paste0(comsolpfad,"conc_tiefen.txt"),skip=9,col.names = c("r","z","CO2_mol_per_m3"))
CO2_mod$tiefe <- CO2_mod$z - 40
CO2_obs$tiefe <- CO2_obs$z - 40

#CO2_mod_sweep <- read.table(paste0(comsolpfad,"conc_tiefen_sweep.txt"),skip=9)
CO2_mod_sweep <- readLines(paste0(comsolpfad,"conc_tiefen_sweep.txt"))
colnames_sweep <- str_split(CO2_mod_sweep[9],"\\s+")
colnames_sweep_raw <- str_extract_all(CO2_mod_sweep[9],"R|Z|DS_sand=\\d(\\.\\d+)?E-\\d m\\^2/s, CO2_atm=\\d\\.\\d+",simplify = T)

colnames_sweep <- str_replace_all(colnames_sweep_raw,c(" m\\^2/s, "="_"))

CO2_sweep_mat <- str_split(CO2_mod_sweep[10:length(CO2_mod_sweep)],"\\s+",simplify = T)
CO2_sweep <- as.data.frame(apply(CO2_sweep_mat,2,as.numeric))
colnames(CO2_sweep) <- colnames_sweep

sweep_long <- reshape2::melt(CO2_sweep,id=1:2,value.name="CO2_mol_per_m3",variable="par")

sweep_long$DS_sand <- as.numeric(str_extract(sweep_long$par,"(?<=DS_sand=)\\d(\\.\\d+)?E-\\d"))
sweep_long$CO2_atm <- as.numeric(str_extract(sweep_long$par,"(?<=CO2_atm=)\\d\\.\\d+"))
sweep_long$CO2_ppm <- ppm_to_mol(sweep_long$CO2_mol_per_m3,"mol/m^3")
CO2_obs$CO2_ppm <- ppm_to_mol(CO2_obs$CO2_mol_per_m3,"mol/m^3")
sweep_long$CO2_obs <- as.numeric(as.character(factor(sweep_long$tiefe,levels = CO2_obs$tiefe,labels = CO2_obs$CO2_ppm)))
head(sweep_long,20)
sweep_long$tiefe <- sweep_long$Z - 40
head(sweep_long)

ggplot(subset(sweep_long,CO2_atm==0.0188))+geom_line(aes(tiefe,CO2_ppm))
ggplot(subset(sweep_long,CO2_atm==0.0188))+geom_line(aes(tiefe,CO2_ppm,col=as.factor(DS_sand)))+guides(col=F)+geom_point(data=CO2_obs,aes(tiefe,CO2_ppm))


######################
#fm
sweep_long$residual <- sweep_long$CO2_obs - sweep_long$CO2_ppm
CO2_sweep[1:7,1:10]
RMSE <- function(mod,obs){
  sqrt(mean((mod-obs)^2,na.rm = T))
}
CO2_sweep[1:6,1:3]
tiefen <- 1:7
rmse <- apply(CO2_sweep[tiefen,-(1:2)],2,RMSE,CO2_obs$CO2_mol_per_m3[tiefen])
best.fit.char <- names(which.min(rmse))
best_DS <- as.numeric(str_extract(best.fit.char,"(?<=DS_sand=)\\d(\\.\\d+)?E-\\d"))
best_CO2_atm <- as.numeric(str_extract(best.fit.char,"(?<=CO2_atm=)\\d\\.\\d+"))

ggplot(subset(sweep_long,DS_sand==best_DS & CO2_atm == best_CO2_atm))+geom_line(aes(tiefe,CO2_ppm,col="mod"))+geom_point(data=CO2_obs,aes(tiefe,CO2_ppm,col="obs"))+
  annotate("text",label=paste("DS=",best_DS,"\nCO2_atm=",best_CO2_atm),x=-10,y=6000)
ggplot(subset(sweep_long,DS_sand==2.8e-6 & CO2_atm == 0.0168))+geom_line(aes(tiefe,CO2_ppm,col=as.factor(CO2_atm)))+geom_point(data=CO2_obs,aes(tiefe,CO2_ppm))

residuals<-apply(CO2_sweep[,-(1:2)],2,"-",CO2_obs$CO2_mol_per_m3)




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
