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
load(paste0(samplerpfad,"Hartheim_CO2.RData"))

data_agg$CO2_mol_per_m3 <- ppm_to_mol(data_agg$CO2_tracer,"ppm")
A_inj <- set_units(1^2*pi,"mm^2")

inj_mol_min <- ppm_to_mol(round(data_agg$Fz,2),"cm^3/min",out_class = "units")
inj_mol_mm2_s <- set_units(inj_mol_min,"mol/s")/A_inj
data_agg$inj_mol_m2_s <- set_units(inj_mol_mm2_s,"mol/m^2/s")


pars_fs <- read.table((paste0(metapfad,"COMSOL/parameter_freeSoil.csv")),sep=";",stringsAsFactors = F)

z_soil_ch <- str_split(pars_fs[pars_fs[,1] == "z_soil",2],"\\s",simplify = T)
unit <- str_remove_all(z_soil_ch[,2],"\\[|\\]")

z_soil <- set_units(as.numeric(z_soil_ch[,1]),unit,mode="standard")
z_soil_cm <- as.numeric(set_units(z_soil,"cm"))
z_soil_cm <- 200
#####################################
#sweep vorgarten
CO2_mod_sweep <- readLines(paste0(comsolpfad,"CO2_mod_Hartheim.txt"))
schichten <- 4
pars <- c("injection_rate","CO2_atm",paste0("DS_",1:schichten))
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

kammer_date <- ymd_h("2020-06-09 11")

CO2_obs <- subset(data_agg,hour== kammer_date)
CO2_obs$z <- z_soil_cm + CO2_obs$tiefe
CO2_obs <- CO2_obs[order(-CO2_obs$tiefe),]
CO2_obs$z - CO2_sweep$z 
 
ggplot(CO2_obs)+geom_path(aes(CO2_mol_per_m3,tiefe))

injection_rate_i <-round(unique(CO2_obs$inj_mol_m2_s),6)
sweep_sub_id <- grep(paste0("injection_rate=",injection_rate_i),colnames(CO2_sweep))
sweep_sub <- CO2_sweep[,sweep_sub_id]

rmse <- apply(sweep_sub,2,RMSE,CO2_obs$CO2_mol_per_m3)
DS_mat_ch <- str_extract_all(names(rmse),"(?<=DS_\\d=)\\d(\\.\\d+)?E-\\d",simplify = T)
DS_mat <- as.data.frame(apply(DS_mat_ch,2,as.numeric))

colnames(DS_mat) <- str_subset(pars,"DS")
DS_wide <- cbind(rmse,DS_mat)
DS_sorted <- DS_wide[which(DS_wide[,2] >= DS_wide[,3] & DS_wide[,3] >= DS_wide[,4] &DS_wide[,4] >= DS_wide[,5] ),]
DS_long <- reshape2::melt(DS_wide, id = "rmse",variable="Schicht",value.name="DS")

ggplot(subset(DS_long,rmse < sort(unique(rmse))[300]))+geom_point(aes(DS,rmse))+facet_wrap(~Schicht,scales="free")

best.fit.id <- which.min(rmse)
best.fit.id2 <- which.min(DS_sorted$rmse)
best.rmse <- min(rmse)
best.rmse_sorted <- min(DS_sorted$rmse)

range(rmse)
best.rmse
best.rmse_sorted

best_DS <- as.numeric(DS_mat[best.fit.id,])
best_DS_sorted <- as.numeric(DS_sorted[best.fit.id2,-1])
names(best_DS) <- colnames(DS_mat)
names(best_DS_sorted) <- colnames(DS_mat)
paste(names(best_DS),best_DS)
write.table(paste(names(best_DS),best_DS),file=paste0(comsolpfad,"best_DS_Hartheim.txt"),col.names = F,row.names = F,quote = F)
DS_D0 <- best_DS/D0_CO2_m2 #m2/s
schicht_grenzen <- seq(0,by=-7,length.out = schichten)
schicht_untergrenzen <- c(schicht_grenzen[-1],-z_soil_cm)
DS_profil <- data.frame(DS=best_DS, DS_sorted = best_DS_sorted,DS_D0 = DS_D0,tiefe=seq(-3.5,by=-7,length.out = schichten),top=schicht_grenzen,bottom=schicht_untergrenzen)
#für die vierte Schicht gibt es nicht genug punkte
#DS_profil$DS[4] <- NA
CO2_obs$CO2_mod <- ppm_to_mol(sweep_sub[,best.fit.id],"mol/m^3")
CO2_obs$CO2_mod_sorted <- ppm_to_mol(sweep_sub[,colnames(sweep_sub) == rownames(DS_sorted[best.fit.id2,])],"mol/m^3")

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
DS_profil_long <- reshape2::melt(DS_profil[,c("DS","tiefe","top","bottom")],id="DS",value.name="tiefe")
DS_profil_long_sorted <- reshape2::melt(DS_profil[,c("DS_sorted","tiefe","top","bottom")],id="DS_sorted",value.name="tiefe")
DS_profil_long <- DS_profil_long[order(DS_profil_long$tiefe),]
DS_profil_long_sorted <- DS_profil_long_sorted[order(DS_profil_long_sorted$tiefe),]
range_mod <- aggregate(sweep_long$CO2_mod,list(tiefe = sweep_long$tiefe),range)
CO2_obs$max_mod <- rev(range_mod$x[,2])
CO2_obs$min_mod <- rev(range_mod$x[,1])
sweep_plot <- ggplot(CO2_obs)+
  geom_ribbon(aes(xmin=min_mod,xmax=max_mod,y=tiefe,fill="sweep"),alpha=0.3)+
  geom_hline(yintercept = schicht_grenzen)+
  geom_line(aes(CO2_mod,tiefe,col="mod"))+
  geom_point(aes(CO2_tracer,tiefe,col="obs"))

DS_D0_plot <- ggplot(CO2_obs)+geom_path(aes(DS_D0,tiefe))
DS_plot <- ggplot(DS_profil_long)+geom_path(aes(DS,tiefe))+
  ylim(range(CO2_obs$tiefe))

ds_profil_plot <- egg::ggarrange(sweep_plot,DS_plot,ncol=2,widths = c(2,1))
sweep_plot_sorted <- ggplot(CO2_obs)+
  geom_ribbon(aes(xmin=min_mod,xmax=max_mod,y=tiefe,fill="sweep"),alpha=0.3)+
  geom_hline(yintercept = schicht_grenzen)+
  geom_line(aes(CO2_mod_sorted,tiefe,col="mod"))+
  geom_point(aes(CO2_tracer,tiefe,col="obs"))

DS_D0_plot <- ggplot(CO2_obs)+geom_path(aes(DS_D0,tiefe))
DS_plot_sorted <- ggplot(DS_profil_long_sorted)+geom_path(aes(DS_sorted,tiefe))+
  ylim(range(CO2_obs$tiefe))

ds_profil_plot_sorted <- egg::ggarrange(sweep_plot_sorted,DS_plot_sorted,ncol=2,widths = c(2,1))

pdf(paste0(plotpfad,"DS_profil.pdf"),width=8,height=5)
ds_profil_plot
dev.off()
pdf(paste0(plotpfad,"DS_profil_sorted.pdf"),width=8,height=5)
ds_profil_plot_sorted
dev.off()

sweep_long[1:10,]
# ggplot(data_agg)+
#   geom_point(aes(CO2_inj_1.5,tiefe))+
#   geom_point(aes(CO2_ref_1.5,tiefe))

slope_0_7cm <- glm(CO2_ref ~ tiefe, data= subset(CO2_obs,tiefe >= -7))#ppm/cm
slope_0_20cm <- glm(CO2_ref ~ tiefe, data= subset(CO2_obs,tiefe >= -15))#ppm/cm
ggplot(CO2_obs)+
  geom_path(aes(CO2_ref + offset,tiefe,col="ref + offset"))+
  geom_path(aes(CO2_inj,tiefe,col="inj"))
dC_dz <- -slope_0_7cm$coefficients[2]
dC_dz <- -slope_0_20cm$coefficients[2]
dC_dz
#DS = -FZ * dz / dC

dC_dz_mol <- ppm_to_mol(dC_dz,"ppm",out_class = "units")#mol/m^3/cm

Fz_mol_per_min_m2 <- DS_profil$DS[DS_profil$top == 0]*60  * dC_dz_mol * 100#m2/min * mol/m3/m = mol/min/m2
Fz_ml_per_min_m2 <- DS_profil$DS[DS_profil$top == 0]*60 * 10^4 * dC_dz/10^6 * 10^4#cm2/min /cm  = ml/min/m2

Fz_ml_per_min_m2
colnames(CO2_obs)
ggplot(CO2_obs)+geom_path(aes(CO2_tracer,tiefe))
CO2_obs$z <- CO2_obs$z -50
CO2_obs$r <- 0
write.table(na.omit(CO2_obs[,c("r","z","CO2_mol_per_m3")]),paste0(metapfad_comsol,"CO2_obs.csv"),col.names = F,row.names = F,sep=",")
