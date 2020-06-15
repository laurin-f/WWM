#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_tracer<- paste0(metapfad,"Tracereinspeisung/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","ggforce","units","egg")

check.packages(packages)

geometry_sweep_files <- list.files(paste0(comsolpfad,"geometry_sweep"),full.names = T)
geometry_sweeps <- lapply(geometry_sweep_files,read.csv,skip=6,sep="")
geometry_sweeps[[8]]
for(i in 1:8){
  colnames(geometry_sweeps[[i]]) <- c("r","z",paste0("c",i))
}

geometry_wide <- Reduce(merge,geometry_sweeps)
geometry <- reshape2::melt(geometry_wide, id = c("r","z"),value.name="c")
geometry$tiefe <- as.numeric(as.character(factor(geometry$variable,levels= unique(geometry$variable),labels = (0:7)*-3.5)))

z_soil <- 150
r_soil <- 180


z_mat <- subset(geometry, r==r_soil)
r_mat <- subset(geometry, z==z_soil)
z_mat <- z_mat[order(z_mat$z),]
r_mat <- r_mat[order(r_mat$r),]
for(i in unique(r_mat$tiefe)){
  rID <- r_mat$tiefe == i
  zID <- z_mat$tiefe == i
  r_mat$slope[rID] <- c(NA,diff(r_mat$c[rID]))
  z_mat$slope[zID] <- c(NA,diff(z_mat$c[zID]))
}
ggplot(r_mat)+geom_point(aes(r,c))+facet_wrap(~tiefe,scales="free")
ggplot(r_mat)+
  geom_hline(yintercept = 0)+
  geom_point(aes(r,slope))+
  facet_wrap(~tiefe,scales="free")
ggplot(z_mat)+
  geom_hline(yintercept = 0)+
  geom_point(aes(z,slope))+
  facet_wrap(~tiefe,scales="free")

ggplot(geometry)+geom_vline(xintercept = r_soil)+
  geom_line(aes(r,c,col=as.factor(z)))+
  geom_point(data = subset(geometry,r== r_soil & z == z_soil),aes(r,c))+
  facet_wrap(~tiefe,scales="free")
ggplot(geometry)+geom_vline(xintercept = z_soil)+
  geom_line(aes(z,c,col=as.factor(r)))+
  geom_point(data = subset(geometry,r== r_soil & z == z_soil),aes(z,c))+
  facet_wrap(~tiefe,scales="free")
ggplot(geometry)+geom_point(aes(z,c,col=as.factor(tiefe)))

ggplot(geometry)+geom_point(aes(c,tiefe,col=as.factor(z)))
ggplot(geometry)+geom_point(aes(c,tiefe,col=as.factor(r)))
#####################################
#sweep vorgarten
CO2_mod_sweep <- readLines(paste0(comsolpfad,"CO2_mod_free_soil_dims.txt"))
schichten <- 4
pars <- c("z_soil","r_soil")
value_regexp <- "\\d+(\\.\\d+)?(E-\\d)?"
colnames_sweep <- str_extract_all(CO2_mod_sweep[9],paste0("(?<=% )r|z|",paste0(pars,"=",value_regexp,collapse=", ")),simplify = T)

CO2_sweep_mat <- str_split(CO2_mod_sweep[10:length(CO2_mod_sweep)],"\\s+",simplify = T)
CO2_sweep <- as.data.frame(apply(CO2_sweep_mat,2,as.numeric))
colnames(CO2_sweep) <- colnames_sweep

sweep_long <- reshape2::melt(CO2_sweep,id=1:2,value.name="CO2_mol_per_m3",variable="par")

#parameter als extra spalte aus character ausschneiden
for(i in pars){
  sweep_long[,i] <- as.numeric(str_extract(sweep_long$par,paste0("(?<=",i,"=)",value_regexp)))
}

#einheit in ppm
sweep_long$CO2_mod <- ppm_to_mol(sweep_long$CO2_mol_per_m3,"mol/m^3","units")

#tiefe umrechnen
sweep_long$tiefe <- set_units(sweep_long$z - z_soil_cm,cm)




######################
#fm

D0_CO2 <- D0_T_p(15) #18°C cm2/s
D0_CO2_m2 <- D0_CO2/10^4 #m2/s


CO2_obs <- subset(data_agg,hour==data_agg$hour[3])
CO2_obs$z <- z_soil_cm + CO2_obs$tiefe
CO2_obs <- CO2_obs[order(-CO2_obs$tiefe),]
CO2_obs$z - CO2_sweep$z 

ggplot(CO2_obs)+geom_path(aes(CO2_mol_per_m3,tiefe))

sweep_sub <- CO2_sweep

rmse <- apply(sweep_sub,2,RMSE,CO2_obs$CO2_mol_per_m3)
DS_mat_ch <- str_extract_all(names(rmse),"(?<=DS_\\d=)\\d(\\.\\d+)?E-\\d",simplify = T)
DS_mat <- as.data.frame(apply(DS_mat_ch,2,as.numeric))

colnames(DS_mat) <- str_subset(pars,"DS")
DS_wide <- cbind(rmse,DS_mat)
DS_long <- reshape2::melt(DS_wide, id = "rmse",variable="Schicht",value.name="DS")

ggplot(subset(DS_long,rmse < sort(unique(rmse))[1000]))+geom_point(aes(DS,rmse))+facet_wrap(~Schicht,scales="free")

best.fit.id <- which.min(rmse)

best_DS <- as.numeric(DS_mat[best.fit.id,])

DS_D0 <- best_DS/D0_CO2_m2 #m2/s
schicht_grenzen <- seq(0,by=-7,length.out = schichten)
schicht_untergrenzen <- c(schicht_grenzen[-1],-z_soil_cm)
DS_profil <- data.frame(DS=best_DS,DS_D0 = DS_D0,tiefe=seq(-3.5,by=-7,length.out = schichten),top=schicht_grenzen,bottom=schicht_untergrenzen)
#für die vierte Schicht gibt es nicht genug punkte
#DS_profil$DS[4] <- NA
CO2_obs$CO2_mod <- ppm_to_mol(CO2_sweep[,best.fit.id],"mol/m^3")

# ende for schleife


CO2_obs$DS <- approx(DS_profil$bottom,DS_profil$DS,CO2_obs$tiefe,method="constant",rule=2)$y
CO2_obs$DS_D0 <- approx(DS_profil$bottom,DS_profil$DS_D0,CO2_obs$tiefe,method="constant",rule=2)$y
CO2_obs$DS[CO2_obs$tiefe > 0] <- NA


# sweep_plot <- ggplot(sweep_long)+
#   geom_path(aes(CO2_mod,tiefe,col=par))+
#   geom_point(data=CO2_obs,aes(tracer,tiefe),col="black")+
#   geom_point(data=CO2_obs,aes(CO2_mod,tiefe),col="red")+
#   geom_hline(yintercept = schicht_grenzen)+
#   guides(col=F)

sweep_plot <- ggplot(CO2_obs)+
  geom_point(aes(CO2_tracer,tiefe,col="obs"))+
  geom_line(aes(CO2_mod,tiefe,col="mod"))+
  geom_hline(yintercept = schicht_grenzen)

DS_D0_plot <- ggplot(CO2_obs)+geom_path(aes(DS_D0,tiefe))
DS_plot <- ggplot(CO2_obs)+geom_path(aes(DS,tiefe))

egg::ggarrange(sweep_plot,DS_plot,ncol=2,widths = c(2,1))

data_agg