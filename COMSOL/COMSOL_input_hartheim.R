###################################
#dateien für COMSOL exportieren####

#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
COMSOL_exepath <- "C:/Program Files/COMSOL/COMSOL52a/Multiphysics/bin/win64/"
COMSOL_progammpath <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Programme/Fremdprogramme/COMSOL/"
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","ggforce","dplyr")
check.packages(packages)

load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))

Kammer_flux$date
offset_method <- "glm"
optim_method <- "snopt"
data$CO2_mol_per_m3 <- ppm_to_mol(data[,paste0("CO2_tracer_",offset_method)],"ppm",p_kPa = data$PressureActual_hPa/10,T_C = data$T_soil)

data$CO2_mol_per_m3[data$tiefe == 0]<- 0
data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0]<- 0

A_inj <- set_units(1^2*pi,"mm^2")

inj_mol_min <- ppm_to_mol(round(data$Fz,6),"cm^3/min",out_class = "units",p_kPa = data$PressureActual_hPa/10,T_C = data$Ta_2m)
inj_mol_mm2_s <- set_units(inj_mol_min,"mol/s")/A_inj
data$inj_mol_m2_s <- set_units(inj_mol_mm2_s,"mol/m^2/s")

z_soil_cm <-  150
data$z <- z_soil_cm +data$tiefe
data$r <- 0

kammer_dates_all <- aggregate(list(date=Kammer_flux$date),list(day=Kammer_flux$day),mean)
#kammer messungen

mod_dates <- sort(unique(data_agg$hour[day(data_agg$hour) %in% 10:14]))
#mod_dates <- ymd_h("2020.07.14 15")

#mod_dates <- ymd_hm(c("2020-06-09 11:00","2020.07.08 11:00","2020.07.14 15:00"))
data_sub <- lapply(mod_dates,function(x) subset(data[,c("tiefe","tiefenstufe","date","z","r","CO2_mol_per_m3","inj_mol_m2_s","DSD0_PTF_max","DSD0_PTF_min","T_soil","PressureActual_hPa","CO2_ref","CO2_inj")], date==x))


ggplot(data_sub[[1]])+geom_line(aes(CO2_mol_per_m3,z),orientation = "y")+labs(title = offset_method,subtitle = mod_dates[1])
ggplot(data_sub[[2]])+geom_line(aes(CO2_mol_per_m3,z),orientation = "y")+labs(title = offset_method,subtitle = mod_dates[2])
ggplot(data_sub[[3]])+geom_line(aes(CO2_mol_per_m3,z),orientation = "y")+labs(title = offset_method,subtitle = mod_dates[3])


for(i in seq_along(data_sub)){
data_sub[[i]] <- data_sub[[i]][order(data_sub[[i]]$z),]
D0 <- D0_T_p(data_sub[[i]]$T_soil,p_kPa = data_sub[[i]]$PressureActual_hPa/10,"m^2/s")
data_sub[[i]]$D0 <- D0
data_sub[[i]]$DS_max_m2_s <- data_sub[[i]]$DSD0_PTF_max * D0
data_sub[[i]]$DS_min_m2_s <- data_sub[[i]]$DSD0_PTF_min * D0
}

for(j in seq_along(data_sub)){
  sub_j <- data_sub[[j]]
  injection_rate <- unique(sub_j$inj_mol_m2_s)
  
  for(i in 1:7){
    write.table(sub_j[sub_j$tiefe == (1:7*-3.5)[i],"CO2_mol_per_m3"],paste0(metapfad_comsol,"dom",i,".csv"),col.names = F,row.names = F,sep=",")
  }
  #write.table(na.omit(sub_j[,c("r","z","CO2_mol_per_m3")]),paste0(metapfad_comsol,"CO2_obs",".csv"),col.names = F,row.names = F,sep=",")
  #write.table(paste("injection_rate,",unique(sub_j$inj_mol_m2_s)),paste0(metapfad_comsol,"inj_rate",".csv"),col.names = F,row.names = F,sep=",",quote=F)
  
  modelname <- "Diffusion_freeSoil_optim"
  if(optim_method == "nelder"){
  cmd <- paste0("cd ",COMSOL_exepath,"&& comsolbatch.exe -inputfile ",COMSOL_progammpath,modelname,".mph -outputfile ",COMSOL_progammpath,modelname,"_solved.mph -study std6 -pname injection_rate -plist ",injection_rate)
  }
  if(optim_method == "snopt"){
  cmd <- paste0("cd ",COMSOL_exepath,"&& comsolbatch.exe -inputfile ",COMSOL_progammpath,modelname,".mph -outputfile ",COMSOL_progammpath,modelname,"_solved.mph -job b1 -pname injection_rate -plist ",injection_rate)
  }
  
  print(paste("starting with",mod_dates[j]))
  tictoc::tic()
  #console_out <- 
    shell(cmd,translate=T,intern=F)
  #print(paste(mod_dates[j],"calculated in:"))
  tictoc::toc()
  date_chr <- format(mod_dates[j],"%m_%d_%H")
  #comsoloutfiles_raw <- paste0(comsolpfad,c("Objective_table.txt","Probe_table.txt"))
  if(optim_method == "nelder"){
    comsoloutfiles_raw <- list.files(comsolpfad,"Probe_table.txt|Probe_Table.txt",full.names = T)
  }
  if(optim_method =="snopt"){
    comsoloutfiles_raw <- list.files(comsolpfad,"CO2_optim.txt",full.names = T)
  }
  comsoloutfiles <- str_replace(comsoloutfiles_raw,"\\.txt",paste0("_",offset_method,"_",optim_method,"_",date_chr,".txt"))
  file.rename(comsoloutfiles_raw,comsoloutfiles)
}
 
##############################
#COMSOL output
##############################
date_pattern <- "\\d{2}_\\d{2}_\\d{2}.txt"
mod_files <- list.files(comsolpfad,pattern = paste(offset_method,optim_method,date_pattern,sep="_"))

mod_date_all_chr <- sort(unique(str_extract(mod_files,date_pattern)))
mod_dates_all <- ymd_h(paste("2020",mod_date_all_chr))

F_Comsol <- data.frame(date=mod_dates_all,Fz=NA,Fz_inj=NA)
plot <- T
for(j in seq_along(mod_dates_all)){
date_chr <- format(mod_dates_all[j],"%m_%d_%H")

if(optim_method == "snopt"){
CO2_optim <- read.csv(paste0(comsolpfad,"CO2_optim_",offset_method,"_",optim_method,"_",date_chr,".txt"),skip=9,sep="",header=F)
colnames(CO2_optim) <- c("r","z","CO2_mod_mol_m3",paste0("DS_",1:4))
CO2_mod <- CO2_optim[,1:3]
best_DS <- CO2_optim[1,4:7]
}
if(optim_method =="nelder"){
# DS_mod <- read.csv(paste0(comsolpfad,"Objective_table_",date_chr,".txt"),skip=5,sep="",header=F)
# colnames_DS <- read.csv(paste0(comsolpfad,"Objective_table_",date_chr,".txt"),skip=4,nrows=1,sep="",header=F,stringsAsFactors = F)
# colnames(DS_mod) <- str_remove(colnames_DS[-1],"comp1.")
# best_DS <- tail(DS_mod,1)

DS_mod <- read.csv(paste0(comsolpfad,"Probe_table_",offset_method,"_",optim_method,"_",date_chr,".txt"),skip=5,sep="",header=F)
colnames_DS_raw <- readLines(paste0(comsolpfad,"Probe_table_",offset_method,"_",date_chr,".txt"),n=5)
colnames_DS <- str_extract_all(colnames_DS_raw[5],"injection_rate|DS_\\d|Probe \\d",simplify = T)
colnames(DS_mod) <- str_replace(colnames_DS,"Probe ","CO2mod_tiefenstufe")

CO2_mod <- tidyr::pivot_longer(tail(DS_mod[,grep("CO2",colnames(DS_mod))],1),everything(),names_prefix = "CO2mod_tiefenstufe",values_to = "CO2_mod_mol_m3",names_to = "tiefenstufe")

best_DS <- tail(DS_mod[grep("DS",colnames_DS)],1)
}
obs_mod <- merge(data_sub[j],CO2_mod)

schichten <- 4
schicht_grenzen <- seq(0,by=-7,length.out = schichten)
schicht_untergrenzen <- c(schicht_grenzen[-1],-z_soil_cm)
DS_profil <- data.frame(DS=as.vector(t(best_DS[1,1:4])),tiefe=seq(-3.5,by=-7,length.out = schichten),top=schicht_grenzen,bottom=schicht_untergrenzen)
for(i in 1:nrow(DS_profil)){
  tiefenID <- obs_mod$tiefe <= DS_profil$top[i] & obs_mod$tiefe > DS_profil$bottom[i]
  DS_profil$DS_PTF_min[i] <- min(obs_mod$DS_min_m2_s[tiefenID]) 
  DS_profil$DS_PTF_max[i] <- max(obs_mod$DS_max_m2_s[tiefenID]) 
  DS_profil$DS_PTF[i] <- mean(unlist(obs_mod[tiefenID,c("DS_min_m2_s","DS_max_m2_s")]))
  DS_profil$D0[i] <- mean(unlist(obs_mod[tiefenID,c("D0")]))
}

if(plot == T){
DS_profil_long <- reshape2::melt(DS_profil,id=grep("DS|D0",colnames(DS_profil)),value.name="tiefe")

obs_mod_plot <- ggplot(obs_mod)+
  geom_line(aes(y=tiefe,x=CO2_mod_mol_m3,col="mod"))+
  geom_point(aes(y=tiefe,x=CO2_mol_per_m3,col="obs"))+
  labs(y="tiefe [cm]",x=expression(CO[2]~"[mol m"^{-3}*"]"),color="")#+ggsave(paste0(plotpfad,"Comsol_obs_mod",date_chr,".png"),width=7,height = 7)
DS_plot <- ggplot(DS_profil_long)+
  geom_ribbon(aes(y=tiefe,xmin=DS_PTF_min,xmax=DS_PTF_max,fill="PTF"),alpha=0.2)+
  geom_line(aes(y=tiefe,x=DS_PTF,col="DS_PTF"),orientation = "y")+
  geom_line(aes(DS,tiefe,col="DS_COMSOL"),orientation = "y")+
  ylim(range(obs_mod$tiefe))
DSD0_plot <- ggplot(DS_profil_long)+
  geom_ribbon(aes(y=tiefe,xmin=DS_PTF_min/D0,xmax=DS_PTF_max/D0,fill="PTF"),alpha=0.2)+
  geom_ribbon(aes(y=tiefe,xmin=DS/D0,xmax=DS/D0,fill="COMSOL"),alpha=0.2)+
  geom_line(aes(y=tiefe,x=DS_PTF/D0,col="DS_PTF"),orientation = "y")+
  geom_line(aes(DS/D0,tiefe,col="DS_COMSOL"),orientation = "y")+
  labs(x="DS/D0",y="",color="method",fill="method")+
  ylim(range(obs_mod$tiefe))#+ggsave(paste0(plotpfad,"DSD0_Comsol_PTF",date_chr,".png"),width=7,height = 7)
# 
# 
# #png(paste0(plotpfad,"DSD0_Comsol_PTF",date_chr,".png"),width=10,height = 7,units = "in",res=300)
 #ds_profil_plot <- 
   egg::ggarrange(obs_mod_plot,DSD0_plot,ncol=2)

#dev.off()
}
slope_0_7cm <- glm(CO2_ref ~ tiefe, data= subset(data_sub[[j]],tiefe >= -7))#ppm/cm
slope_0_7cm_inj <- glm(CO2_inj ~ tiefe, data= subset(data_sub[[j]],tiefe >= -7))#ppm/cm
#plot(data_sub[[j]]$tiefe,data_sub[[j]]$CO2_ref)
#abline(slope_0_7cm)
dC_dz <- -slope_0_7cm$coefficients[2]
dC_dz_inj <- -slope_0_7cm_inj$coefficients[2]

dC_dz#ppm/cm
#DS = -FZ * dz / dC

dC_dz_mol <- ppm_to_mol(dC_dz,"ppm",out_class = "units")#mol/m^3/cm
dC_dz_mol_inj <- ppm_to_mol(dC_dz_inj,"ppm",out_class = "units")#mol/m^3/cm

Fz_mumol_per_s_m2 <- best_DS$DS_1  * dC_dz_mol * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2
#Fz_mumol_per_s_m2 <- 5.603326e-06  * dC_dz_mol * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2
Fz_mumol_per_s_m2_inj <- best_DS$DS_1  * dC_dz_mol_inj * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2


F_Comsol$Fz[F_Comsol$date == mod_dates_all[[j]]] <- Fz_mumol_per_s_m2
F_Comsol$Fz_inj[F_Comsol$date == mod_dates_all[[j]]] <- Fz_mumol_per_s_m2_inj
for(k in 1:4){
F_Comsol[F_Comsol$date == mod_dates_all[[j]],paste0("DSD0",k)] <- DS_profil$DS[k]/DS_profil$D0[k]
}
}

#ggplot(subset(data))+geom_line(aes(date,CO2_tracer_glm,col=as.factor(tiefe)))+geom_vline(xintercept = kammer_dates_all$date)
ggplot(subset(Kammer_flux))+
  geom_ribbon(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max,fill=kammer),alpha=0.2)+
  geom_line(aes(date,CO2flux,col=kammer))+
  ggnewscale::new_scale_color()+
  geom_line(data=F_Comsol,aes(date,Fz,col=""))+
  #geom_point(data=F_Comsol,aes(date,Fz_inj,col=""))+
  scale_color_manual("COMSOL",values=1)+
  labs(y=expression(CO[2]*"flux ["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))#+
  #ggsave(paste0(plotpfad,"Flux_Kammer_Comsol.png"),width=7,height = 7)

ggplot(subset(CO2_flux))+
  geom_point(aes(date,mumol_per_s_m2,col="kammer"))+
  geom_line(data=F_Comsol,aes(date,Fz,col="Comsol"),size=2)+
  xlim(c(min(mod_dates_all)-3600*24*1,max(mod_dates_all)+3600*24*3))#+ggsave(paste0(plotpfad,"Flux_Kammer_Comsol",date_chr,".png"),width=7,height = 7)


data_plot <- data %>%
  group_by(tiefe,date_hour=round_date(data$date,"hours")) %>%
  summarise(DSD0_PTF_min = min(DSD0_PTF_min,na.rm=T),DSD0_PTF_max = max(DSD0_PTF_max),DSD0_PTF= mean(DSD0_PTF),date=mean(date))
F_Comsol_long <- tidyr::pivot_longer(F_Comsol,starts_with("DS"),values_to="DSD0")
ggplot(subset(data_plot))+
  geom_ribbon(aes(x=date,ymin=DSD0_PTF_min,ymax=DSD0_PTF_max,fill=as.factor(tiefe)),alpha=0.2)+
  geom_line(aes(date,DSD0_PTF,col=as.factor(tiefe)))+
  ggnewscale::new_scale_color()+
  geom_line(data=F_Comsol_long,aes(date,DSD0,col=name))+
  xlim(c(min(mod_dates_all)-3600*24*1,max(mod_dates_all)+3600*24*3))
