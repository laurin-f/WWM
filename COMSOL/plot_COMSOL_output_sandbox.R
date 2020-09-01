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
theme_set(theme_classic())

#data_agg
load(paste0(samplerpfad,"tracereinspeisung_sandkiste_sub.RData"))

sandbox <- readxl::read_xlsx(paste0(metapfad_tracer,"sandeimer.xlsx"))
z_box <- sandbox$height_cm


#####################################################
#read COMCOL output without parameter sweep
CO2_mod <- read.table(paste0(comsolpfad,"CO2_mod.txt"),skip=9,col.names = c("r","z","CO2_mol_per_m3"))
CO2_mod$tiefe <- CO2_mod$z - z_box
CO2_mod$CO2 <- ppm_to_mol(CO2_mod$CO2_mol_per_m3,"mol/m^3")

#####################################
#sweep mit CO2_atm
CO2_mod_sweep <- readLines(paste0(comsolpfad,"CO2_mod_sweep_all.txt"))
pars <- c("DS","CO2_atm","injection_rate")
value_regexp <- "\\d(\\.\\d+)?(E-)?\\d"
colnames_sweep <- str_extract_all(CO2_mod_sweep[9],paste0("(?<=% )r|z|",paste0(pars,"=",value_regexp,collapse=", ")),simplify = T)

#colnames_sweep_raw <- str_extract_all(CO2_mod_sweep[9],"R|Z|DS=\\d(\\.\\d+)?E-\\d, injection_rate=\\d\\.\\d+, CO2_atm=",simplify = T)

#colnames_sweep <- str_replace_all(colnames_sweep_raw,c(" m\\^2/s, "="_"))

CO2_sweep_mat <- str_split(CO2_mod_sweep[10:length(CO2_mod_sweep)],"\\s+",simplify = T)
CO2_sweep <- as.data.frame(apply(CO2_sweep_mat,2,as.numeric))
colnames(CO2_sweep) <- colnames_sweep

sweep_long <- reshape2::melt(CO2_sweep,id=1:2,value.name="CO2_mol_per_m3",variable="par")
unique(sweep_long$DS)
#parameter als extra spalte aus character ausschneiden
for(i in pars){
  sweep_long[,i] <- as.numeric(str_extract(sweep_long$par,paste0("(?<=",i,"=)",value_regexp)))
}

#einheit in ppm
sweep_long$CO2_mod <- ppm_to_mol(sweep_long$CO2_mol_per_m3,"mol/m^3","units")

#tiefe umrechnen
sweep_long$tiefe <- set_units(sweep_long$z - z_box,cm)


######################
#fm

D0_CO2 <- D0_T_p(20) #18°C cm2/s
D0_CO2_m2 <- D0_CO2/10^4 #m2/s


#plt_list <- list()
tiefen <- 1:8


for(i in unique(data_sub$ID)) {

  CO2_obs <- subset(data_sub, ID == i)
  CO2_atm_i <- round(CO2_obs$CO2_mol_per_m3[CO2_obs$tiefe == 0],6)
  injection_rate_i <-round(unique(CO2_obs$inj_mol_m2_s),6)
  sweep_sub_id <- grep(paste0(", CO2_atm=",CO2_atm_i,", injection_rate=",injection_rate_i,"$"),colnames(CO2_sweep))
  sweep_sub <- CO2_sweep[,sweep_sub_id]
  rmse <- apply(sweep_sub[tiefen,],2,RMSE,CO2_obs$CO2_mol_per_m3[tiefen])
  
  DS <- as.numeric(str_extract(names(rmse),"(?<=DS=)\\d(\\.\\d+)?E-\\d"))
  
  plot(DS,rmse)
  title(main=unique(CO2_obs$material))
  best.fit.id <- which.min(rmse)
  #best.fit.char <- names(best.fit.id)

  best_DS <- DS[best.fit.id]

  DS_D0 <- best_DS/D0_CO2_m2 #m2/s

  data_sub$DS_D0_mod[data_sub$ID == i] <- DS_D0 
  data_sub$DS_mod[data_sub$ID == i] <- best_DS 
  data_sub$CO2_mod[data_sub$ID == i] <- ppm_to_mol(sweep_sub[,best.fit.id],"mol/m^3")

  }# ende for schleife
range_mod <- aggregate(sweep_long$CO2_mod,list(tiefe = sweep_long$tiefe),range)
data_sub$max_mod <- rev(range_mod$x[,2])
data_sub$min_mod <- rev(range_mod$x[,1])


ggplot(data_sub)+
  geom_line(aes(CO2_mod,tiefe,col="mod",linetype=as.factor(Pumpstufe),alpha=ID))+
  geom_point(aes(CO2,tiefe,col="obs"))+
  facet_wrap(~material)+guides(alpha=F)+scale_alpha_manual(values = rep(1,6))+
  scale_linetype_manual("Pumpstufe",values=2:1)+
  labs(x=expression(CO[2]*" [ppm]"),y="tiefe [cm]",col="")+
  ggsave(paste0(plotpfad,"comsol_mod_obs_sandsplitt.pdf"),width=9,height=4)

mod_results <- data_sub[data_sub$tiefe==0,c("ID","DS_D0_mod","material","DS_D0_glm","DS_mod","DS_glm")]

DS_D0_mat <- aggregate(list(DS_D0_COMSOL=mod_results$DS_D0_mod,DS_D0_glm= mod_results$DS_D0_glm),list(material=mod_results$material),mean)
DS_mat <- aggregate(list(DS_COMSOL=mod_results$DS_mod, DS_glm= mod_results$DS_glm),list(material=mod_results$material),mean)
DS_D0_mat
DS_mat
write.csv(DS_D0_mat,file=paste0(comsolpfad,"DS_D0_mat_agg.txt"),row.names = F)
write.csv(mod_results[,c("material","DS_D0_mod")],file=paste0(comsolpfad,"DS_D0_mat.txt"),row.names = F)

thomas_ref <- data.frame(material=c("Sand","Kies","Sand & Kies"), DS_D0=c(0.239, 0.235, 0.185),method="Flühler \n(Laemmel et al. 2017)")

DS_D0_long <- reshape2::melt(DS_D0_mat,id="material",value.name="DS_D0",variable="method")
DS_D0_long$method <- str_remove(DS_D0_long$method,"DS_D0_")
DS_D0_long <- rbind(DS_D0_long,thomas_ref)

DS_D0_mat$DS_D0_COMSOL*D0_CO2_m2
ggplot(DS_D0_long)+geom_col(aes(material,DS_D0,fill=method),position=position_dodge2(preserve = "single"))#+ggsave(paste0(plotpfad,"sandkiste/DS_D0_SandSplitt_vergleich.png"),width=9,height=4)

DS_D0_label <- tidyr::pivot_wider(DS_D0_long,names_from = method,values_from = DS_D0)

DS_D0_label$label <- paste0("DS/D0 = ",round(DS_D0_label$COMSOL,3))

ggplot(subset(data_sub, Versuch %in% c(5,6,9)))+
  #geom_ribbon(aes(xmin=min_mod,xmax=max_mod,y=tiefe,fill="sweep"),alpha=0.3)+
  geom_line(aes(CO2_mod,tiefe,col="mod"))+
  geom_point(aes(CO2,tiefe,fill="obs"))+
  facet_wrap(~factor(material,levels=c("Sand","Sand & Splitt","Splitt"),labels=c("sand","mixture","grit")))+
  geom_text(data=subset(DS_D0_label,material%in% data_sub$material),aes(y= -1,x=6000,label = label),hjust="right")+
  scale_color_manual(values=2)+
  #annotate("text",y= c(-1,-1,-1),x=c(6000,6000,6000),label=c(label1,label2,label3),hjust="right")+
  labs(x=expression(CO[2]*" [ppm]"),y="depth [cm]",col="",fill="")+theme_bw()+ggsave(paste0(plotpfad,"sandkiste/comsol_mod_obs.png"),width = 7,height=3)

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
