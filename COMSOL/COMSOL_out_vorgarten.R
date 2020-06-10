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

#data_agg
load(paste0(samplerpfad,"tracereinspeisung_Vorgarten_inter.RData"))
data_inter$tracer[data_inter$tracer < 0] <- 0
data_inter$CO2_mol_per_m3 <- ppm_to_mol(data_inter$tracer,"ppm")

pars_fs <- read.table((paste0(metapfad,"COMSOL/parameter_freeSoil.csv")),sep=";",stringsAsFactors = F)

z_soil_ch <- str_split(pars_fs[pars_fs[,1] == "z_soil",2],"\\s",simplify = T)
unit <- str_remove_all(z_soil_ch[,2],"\\[|\\]")

z_soil <- set_units(as.numeric(z_soil_ch[,1]),unit,mode="standard")
z_soil_cm <- as.numeric(set_units(z_soil,"cm"))

#####################################
#sweep vorgarten
CO2_mod_sweep <- readLines(paste0(comsolpfad,"CO2_mod_vorgarten.txt"))
schichten <- 3
pars <- paste0("DS_",1:schichten)
value_regexp <- "\\d(\\.\\d+)?(E-)?\\d"
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


tiefen <- 1:8


  
  CO2_obs <- data_inter
  CO2_obs$z <- z_soil_cm + CO2_obs$tiefe
  CO2_obs$z - CO2_sweep$z 
  rmse <- apply(CO2_sweep[,-(1:2)],2,RMSE,CO2_obs$CO2_mol_per_m3)
  DS_mat_ch <- str_extract_all(names(rmse),"(?<=DS_\\d=)\\d(\\.\\d+)?E-\\d",simplify = T)
  DS_mat <- as.data.frame(apply(DS_mat_ch,2,as.numeric))
  
  colnames(DS_mat) <- paste0("DS_",1:3)
  DS_wide <- cbind(rmse,DS_mat)
  DS_long <- reshape2::melt(DS_wide, id = "rmse",variable="Schicht",value.name="DS")
  ggplot(subset(DS_long,rmse < sort(unique(rmse))[1000]))+geom_point(aes(DS,rmse))+facet_wrap(~Schicht,scales="free")
  
  best.fit.id <- which.min(rmse)
  
  best_DS <- as.numeric(DS_mat[best.fit.id,])
  
  DS_D0 <- best_DS/D0_CO2_m2 #m2/s
schicht_grenzen <- seq(0,by=-7,length.out = 3)
schicht_untergrenzen <- c(schicht_grenzen[-1],-z_soil_cm)
  DS_profil <- data.frame(DS=best_DS,DS_D0 = DS_D0,tiefe=seq(-3.5,by=-7,length.out = 3),top=schicht_grenzen,bottom=schicht_untergrenzen)
  #für die vierte Schicht gibt es nicht genug punkte
  #DS_profil$DS[4] <- NA
  data_inter$CO2_mod <- ppm_to_mol(CO2_sweep[,best.fit.id],"mol/m^3")
  
# ende for schleife


data_inter$DS <- approx(DS_profil$bottom,DS_profil$DS,data_inter$tiefe,method="constant",rule=2)$y
data_inter$DS_D0 <- approx(DS_profil$bottom,DS_profil$DS_D0,data_inter$tiefe,method="constant",rule=2)$y
data_inter$DS[data_inter$tiefe > 0] <- NA


# sweep_plot <- ggplot(sweep_long)+
#   geom_path(aes(CO2_mod,tiefe,col=par))+
#   geom_point(data=data_inter,aes(tracer,tiefe),col="black")+
#   geom_point(data=data_inter,aes(CO2_mod,tiefe),col="red")+
#   geom_hline(yintercept = schicht_grenzen)+
#   guides(col=F)
sweep_plot <- ggplot(data_inter)+
  geom_point(aes(tracer,tiefe,col="obs"))+
  geom_line(aes(CO2_mod,tiefe,col="mod"))+
  geom_hline(yintercept = schicht_grenzen)

DS_D0_plot <- ggplot(data_inter)+geom_path(aes(DS_D0,tiefe))
DS_plot <- ggplot(data_inter)+geom_path(aes(DS,tiefe))

egg::ggarrange(sweep_plot,DS_plot,ncol=2,widths = c(2,1))

data_inter

# ggplot(data_inter)+
#   geom_point(aes(CO2_inj_1.5,tiefe))+
#   geom_point(aes(CO2_ref_1.5,tiefe))


slope_0_5cm <- glm(CO2_ref_1.5 ~ tiefe, data= subset(data_inter,tiefe <0 & tiefe > -5))#ppm/cm

dC_dz <- -slope_0_5cm$coefficients[2]

#DS = -FZ * dz / dC

dC_dz_mol <- ppm_to_mol(dC_dz,"ppm",out_class = "units")

Fz_mol_per_min_m2 <- DS_profil$DS[DS_profil$top == 0]*60  * dC_dz_mol * 100#m2/min * mol/m3/m = mol/min/m2
Fz_ml_per_min_m2 <- DS_profil$DS[DS_profil$top == 0]*60 * 10^4 * dC_dz/10^6 * 10^4#cm2/min /cm  = ml/min/m2

Fz_ml_per_min_m2

# Fine gravel 0.235 (0.008) 0.218 0.214
# Mixture 0.185 (0.006) 0.164 0.141
#Thomas paper
# Granular substrate
# col1: Flühler method
# on soil cores
# col2: In situ method:
#   injection at
# bottom
# col3: In situ method:
#   injection at
# middle
# Sand 0.239 (0.013) 0.205 (0.011) 0.205
# Fine gravel 0.235 (0.008) 0.218 0.214
# Mixture 0.185 (0.006) 0.164 0.141



#residuals<-apply(CO2_sweep[,-(1:2)],2,"-",CO2_obs$CO2_mol_per_m3)





ggplot()+
  geom_point(data=CO2_mod,aes(CO2,tiefe,col="mod"))+
  geom_point(data=CO2_obs,aes(CO2,tiefe,col="obs"))
