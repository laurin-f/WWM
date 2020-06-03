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


z_soil_ch <- str_split(pars_fs[pars_fs[,1] == "z_soil",2],"\\s",simplify = T)
unit <- str_remove_all(z_soil_ch[,2],"\\[|\\]")

z_soil <- set_units(as.numeric(z_soil_ch[,1]),unit,mode="standard")
z_soil_cm <- as.numeric(set_units(z_soil,"cm"))

#####################################
#sweep vorgarten
CO2_mod_sweep <- readLines(paste0(comsolpfad,"CO2_mod_vorgarten.txt"))
schichten <- 4
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
  
  rmse <- apply(CO2_sweep,2,RMSE,CO2_obs$CO2_mol_per_m3)
  best.fit.id <- which.min(rmse)
  best.fit.char <- names(best.fit.id)
  
  best_DS <- as.numeric(str_extract_all(best.fit.char,"(?<=DS_\\d=)\\d(\\.\\d+)?E-\\d",simplify = T))
  names(best_DS) <- paste("Schicht",1:4)
  DS_D0 <- best_DS/D0_CO2_m2 #m2/s
  DS_profil <- data.frame(DS=best_DS,DS_D0 = DS_D0,tiefe=seq(-3.5,by=-7,length.out = 4))
  
  data_inter$CO2_mod <- ppm_to_mol(CO2_sweep[,best.fit.id],"mol/m^3")
  
# ende for schleife

  
data_inter$DS <- approx(DS_profil$tiefe,DS_profil$DS,data_inter$tiefe,method="constant",rule=2)$y
data_inter$DS_D0 <- approx(DS_profil$tiefe,DS_profil$DS_D0,data_inter$tiefe,method="constant",rule=2)$y
data_inter$DS[data_inter$tiefe > 0] <- NA
#do.call('ggarrange',c(plt_list, ncol = 3))
schicht_grenzen <- seq(0,by=-7,length.out = 4)
sweep_plot <- ggplot(sweep_long)+
  geom_path(aes(CO2_mod,tiefe,col=par))+
  geom_point(data=data_inter,aes(tracer,tiefe),col="black")+
  geom_point(data=data_inter,aes(CO2_mod,tiefe),col="red")+
  geom_hline(yintercept = schicht_grenzen)+
  guides(col=F)

DS_D0_plot <- ggplot(data_inter)+geom_path(aes(DS_D0,tiefe))
DS_plot <- ggplot(data_inter)+geom_path(aes(DS,tiefe))

egg::ggarrange(sweep_plot,DS_plot,ncol=2,widths = c(3,1))
ggplot(DS_profil)+geom_point(aes(DS,tiefe))

ggplot(data_inter)+
  geom_point(aes(CO2_mod,tiefe,col="mod"))+
  geom_point(aes(tracer,tiefe,col="obs"))

mod_results <- data_sub[data_sub$tiefe==0,c("ID","DS_D0_mod","material","DS_D0_glm","DS_mod","DS_glm")]

DS_D0_mat <- aggregate(list(DS_D0_COMSOL=mod_results$DS_D0_mod,DS_D0_glm= mod_results$DS_D0_glm),list(material=mod_results$material),mean)
DS_mat <- aggregate(list(DS_COMSOL=mod_results$DS_mod, DS_glm= mod_results$DS_glm),list(material=mod_results$material),mean)


DS_mat$DS_glm
thomas_ref <- data.frame(material=c("Sand","Kies","Sand & Kies"), DS_D0=c(0.239, 0.235, 0.185),method="Flühler \n(Laemmel et al. 2017)")

DS_D0_long <- reshape2::melt(DS_D0_mat,id="material",value.name="DS_D0",variable="method")
DS_D0_long$method <- str_remove(DS_D0_long$method,"DS_D0_")
DS_D0_long <- rbind(DS_D0_long,thomas_ref)

DS_D0_mat$DS_D0_COMSOL*D0_CO2_m2
ggplot(DS_D0_long)+geom_col(aes(material,DS_D0,fill=method),position=position_dodge2(preserve = "single"))+ggsave(paste0(plotpfad,"DS_D0_SandSplitt_vergleich.pdf"),width=9,height=4)

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
