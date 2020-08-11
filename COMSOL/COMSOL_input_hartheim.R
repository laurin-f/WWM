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
ggopt()
check.packages(packages)

load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))

Kammer_flux$date
#which method for tracer calculation should be used glm gam offset or drift
offset_method <- "gam"
#which optimization method should be used nelder or snopt
optim_method <- "snopt"
n_DS <- "4DS"
overwrite <- T

#CO2 in tracer in mol pro m3 
data$CO2_mol_per_m3 <- ppm_to_mol(data[,paste0("CO2_tracer_",offset_method)],"ppm",p_kPa = data$PressureActual_hPa/10,T_C = data$T_soil)
#negative werte auf null setzen
data$CO2_mol_per_m3[data$tiefe == 0]<- 0
data$CO2_mol_per_m3[(data$CO2_mol_per_m3) < 0]<- 0



#Modellgeometrie an data anfügen
z_soil_cm <-  150
data$z <- z_soil_cm +data$tiefe
data$r <- 0

#D0 in abhängigkeit von T und p
data$D0 <- D0_T_p(data$T_soil,p_kPa = data$PressureActual_hPa/10,"m^2/s")

#max und min der DS PTF
data$DS_max_m2_s <- data$DSD0_PTF_max * data$D0
data$DS_min_m2_s <- data$DSD0_PTF_min * data$D0
data$DS_mean_m2_s <- data$DSD0_PTF * data$D0

kammer_dates_all <- aggregate(list(date=Kammer_flux$date),list(day=Kammer_flux$day),mean)
kammer_dates_all$date <- with_tz(kammer_dates_all$date,"UTC")


data$date_hour <- round_date(data$date,"hours")
#alle stunden zu denen injektionsrate nicht 0 ist
mod_dates_hourly <- sort(unique(data$date[data$Pumpstufe != 0 & data$date %in% data$date_hour]))

#dates für die DS modelliert werden soll
mod_dates <- sort(unique(data$date[day(data$date) %in% 4:8 & month(data$date) == 7 & data$Pumpstufe != 0 & data$date %in% data$date_hour]))

#mod_dates <- ymd_h("2020.07.14 15")
if(!exists("data_list")){
data_list <- lapply(mod_dates_hourly,function(x) subset(data[,c("tiefe","tiefenstufe","date","z","r","CO2_mol_per_m3","inj_mol_m2_s","DSD0_PTF_max","DSD0_PTF","DSD0_PTF_min","DS_max_m2_s","DS_min_m2_s","DS_mean_m2_s","T_soil","PressureActual_hPa","CO2_ref","CO2_inj","D0")], date==x))
names(data_list) <- as.character(mod_dates_hourly)
}

mod_dates <- ymd_hm(c("2020-07-08 11:00","2020.07.08 12:00","2020.07.14 15:00"))
mod_dates <- ymd_hm(c("2020-06-09 11:00","2020-07-06 15:00","2020.07.08 11:00","2020.07.08 12:00","2020.07.14 15:00"))
#mod_dates <- mod_dates[2]



data_sub <- lapply(mod_dates,function(x) subset(data[,c("tiefe","tiefenstufe","date","z","r","CO2_mol_per_m3","inj_mol_m2_s","DSD0_PTF_max","DSD0_PTF_min","T_soil","PressureActual_hPa","CO2_ref","CO2_inj")], date==x))
names(data_sub) <- mod_dates

ggplot(data_sub[[1]])+geom_line(aes(CO2_mol_per_m3,z),orientation = "y")+labs(title = offset_method,subtitle = mod_dates[1])
ggplot(data_sub[[2]])+geom_line(aes(CO2_mol_per_m3,z),orientation = "y")+labs(title = offset_method,subtitle = mod_dates[2])
ggplot(data_sub[[3]])+geom_line(aes(CO2_mol_per_m3,z),orientation = "y")+labs(title = offset_method,subtitle = mod_dates[3])
ggplot(data_sub[[4]])+geom_line(aes(CO2_mol_per_m3,z),orientation = "y")+labs(title = offset_method,subtitle = mod_dates[4])


##############################################
#Comsol ausführen
##############################################
#
for(j in seq_along(data_sub)){
  sub_j <- data_sub[[j]]
  injection_rate <- unique(sub_j$inj_mol_m2_s)
  
  #schreibe messungen in files die in COMSOL als Objective verwendet werden
  for(i in 1:7){
    write.table(sub_j[sub_j$tiefe == (1:7*-3.5)[i],"CO2_mol_per_m3"],paste0(metapfad_comsol,"dom",i,".csv"),col.names = F,row.names = F,sep=",")
  }
  #write.table(na.omit(sub_j[,c("r","z","CO2_mol_per_m3")]),paste0(metapfad_comsol,"CO2_obs",".csv"),col.names = F,row.names = F,sep=",")
  #write.table(paste("injection_rate,",unique(sub_j$inj_mol_m2_s)),paste0(metapfad_comsol,"inj_rate",".csv"),col.names = F,row.names = F,sep=",",quote=F)
  
  modelname <- "Diffusion_freeSoil_optim"
  modelname <- paste0(modelname,"_",n_DS)
  #command der an commandline gesendet wird um comsolbatch.exe zu starten
  if(optim_method == "nelder"){
    
    outfile <- "Probe_table"
    #erst cd (changedirectory) zum COMSOL_exepath
    #dann && um zwei befehle in einer Reihe zu übergeben
    #dann Modellname mit Pfad und entsprechende Injectionsrate mit -plist übergeben
    #-study std6 ist nelder mead
    cmd <- paste0("cd ",COMSOL_exepath,"&& comsolbatch.exe -inputfile ",COMSOL_progammpath,modelname,".mph -outputfile ",COMSOL_progammpath,modelname,"_solved.mph -study std6 -pname injection_rate -plist ",injection_rate)
  }
  if(optim_method == "snopt"){
    outfile <- "CO2_optim"
    #snopt gibt nicht automatisch die DS werte aus deshalb wird ein batch in der GUI angelegt in dem der Output exportiert wird
  cmd <- paste0("cd ",COMSOL_exepath,"&& comsolbatch.exe -inputfile ",COMSOL_progammpath,modelname,".mph -outputfile ",COMSOL_progammpath,modelname,"_solved.mph -job b1 -pname injection_rate -plist ",injection_rate)
  }
  date_chr <- format(mod_dates[j],"%m_%d_%H")
  outfile_name <- paste0(outfile,"_",offset_method,"_",optim_method,"_",n_DS,"_",date_chr,".txt")
  if(file.exists(paste0(comsolpfad,outfile_name)) & overwrite == F){
    print(paste("file",outfile_name,"exists"))
  }else{
  print(paste("starting with",mod_dates[j]))
  tictoc::tic()
  #console_out <- 
  #commandline befehl ausführen
    shell(cmd,translate=T,intern=F)
  #print(paste(mod_dates[j],"calculated in:"))
  tictoc::toc()
  #comsoloutfiles_raw <- paste0(comsolpfad,c("Objective_table.txt","Probe_table.txt"))
  #outputdateien von COMSOL umbenennen damit sie beim nächsten run nicht überschrieben werden
  if(optim_method == "nelder"){
    comsoloutfiles_raw <- list.files(comsolpfad,"Probe_table.txt|Probe_Table.txt",full.names = T)
  }
  if(optim_method =="snopt"){
    comsoloutfiles_raw <- list.files(comsolpfad,"CO2_optim.txt",full.names = T)
  }
  #im Dateiname steht jetzt die methode und das datum
  comsoloutfiles <- str_replace(comsoloutfiles_raw,"\\.txt",paste0("_",offset_method,"_",optim_method,"_",n_DS,"_",date_chr,".txt"))
  file.rename(comsoloutfiles_raw,comsoloutfiles)
  }
}
 
##############################
#COMSOL output
##############################
#alle dateien mit der gewünschten methode und datum 
 optim_method <- "snopt"
 n_DS <- "3DS"
 offset_method <- "gam"
plot <- F

date_pattern <- "\\d{2}_\\d{2}_\\d{2}.txt"
mod_files <- list.files(comsolpfad,pattern = paste(offset_method,optim_method,n_DS,date_pattern,sep="_"))

mod_date_all_chr <- sort(unique(str_extract(mod_files,date_pattern)))
mod_dates_all <- ymd_h(paste("2020",mod_date_all_chr))

#mod_dates_all <- mod_dates
F_list <- list()
F_Comsol <- data.frame(date=mod_dates_all,Fz=NA,Fz_inj=NA)
#for(l in c("gam","glm")){
  
#offset_method <- l

####################################
#read loop
#######################################
for(j in seq_along(mod_dates_all)){
date_chr <- format(mod_dates_all[j],"%m_%d_%H")

if(optim_method == "snopt"){
CO2_optim <- read.csv(paste0(comsolpfad,"CO2_optim_",offset_method,"_",optim_method,"_",n_DS,"_",date_chr,".txt"),skip=9,sep="",header=F)
n_DS_num <- as.numeric(str_extract(n_DS,"\\d"))
colnames(CO2_optim) <- c("r","z","CO2_mod_mol_m3",paste0("DS_",1:n_DS_num))
CO2_mod <- CO2_optim[,1:3]
best_DS <- CO2_optim[1,4:(n_DS_num+3)]
}
if(optim_method =="nelder"){
# DS_mod <- read.csv(paste0(comsolpfad,"Objective_table_",date_chr,".txt"),skip=5,sep="",header=F)
# colnames_DS <- read.csv(paste0(comsolpfad,"Objective_table_",date_chr,".txt"),skip=4,nrows=1,sep="",header=F,stringsAsFactors = F)
# colnames(DS_mod) <- str_remove(colnames_DS[-1],"comp1.")
# best_DS <- tail(DS_mod,1)

DS_mod <- read.csv(paste0(comsolpfad,"Probe_table_",offset_method,"_",optim_method,"_",n_DS,"_",date_chr,".txt"),skip=5,sep="",header=F)
colnames_DS_raw <- readLines(paste0(comsolpfad,"Probe_table_",offset_method,"_",optim_method,"_",n_DS,"_",date_chr,".txt"),n=5)
colnames_DS <- str_extract_all(colnames_DS_raw[5],"injection_rate|DS_\\d|Probe \\d",simplify = T)
colnames(DS_mod) <- str_replace(colnames_DS,"Probe ","CO2mod_tiefenstufe")

CO2_mod <- tidyr::pivot_longer(tail(DS_mod[,grep("CO2",colnames(DS_mod))],1),everything(),names_prefix = "CO2mod_tiefenstufe",values_to = "CO2_mod_mol_m3",names_to = "tiefenstufe")

best_DS <- tail(DS_mod[grep("DS",colnames_DS)],1)
print(best_DS)
}
obs_j <- data_list[[as.character(mod_dates_all)[j]]]
obs_mod <- merge(obs_j,CO2_mod)

if(n_DS == "4DS"){
schichten <- 4
schicht_grenzen <- seq(0,by=-7,length.out = schichten)
tiefen <- seq(-3.5,by=-7,length.out = schichten)
}
if(n_DS == "3DS"){
schichten <- 3
schicht_grenzen <- c(0,-10.5,-21)
tiefen <- c(-5.25,-15.75,-24.5)
}
if(n_DS == "2DS"){
schichten <- 2
schicht_grenzen <- c(0,-14)
tiefen <- c(-7,-24.5)
}
schicht_untergrenzen <- c(schicht_grenzen[-1],-z_soil_cm)
DS_profil <- data.frame(DS=unlist(best_DS),tiefe=tiefen,top=schicht_grenzen,bottom=schicht_untergrenzen)
for(i in 1:nrow(DS_profil)){
  tiefenID <- obs_mod$tiefe <= DS_profil$top[i] & obs_mod$tiefe > DS_profil$bottom[i]
  DS_profil$DS_PTF_min[i] <- min(obs_mod$DS_min_m2_s[tiefenID]) 
  DS_profil$DS_PTF_max[i] <- max(obs_mod$DS_max_m2_s[tiefenID]) 
  DS_profil$DS_PTF[i] <- mean(obs_mod$DS_mean_m2_s[tiefenID])
  DS_profil$D0[i] <- mean(unlist(obs_mod[tiefenID,c("D0")]))
}

if(plot == T){
DS_profil_long <- reshape2::melt(DS_profil,id=grep("DS|D0",colnames(DS_profil)),value.name="tiefe")

obs_mod_plot <- ggplot(obs_mod)+
  geom_line(aes(y=tiefe,x=CO2_mod_mol_m3,col="mod"))+
  geom_point(aes(y=tiefe,x=CO2_mol_per_m3,col="obs"))+
  labs(title=mod_dates_all[j],y="tiefe [cm]",x=expression(CO[2]~"[mol m"^{-3}*"]"),color="")#+ggsave(paste0(plotpfad,"Comsol_obs_mod",date_chr,".png"),width=7,height = 7)
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
   Sys.sleep(2)

#dev.off()
}
slope_0_7cm <- glm(CO2_ref ~ tiefe, data= subset(obs_j,tiefe >= -7))#ppm/cm
#plot(obs_j$tiefe,obs_j$CO2_ref)
#abline(slope_0_7cm)
dC_dz <- -slope_0_7cm$coefficients[2]

# slope_0_7cm_inj <- glm(CO2_inj ~ tiefe, data= subset(obs_j,tiefe >= -7))#ppm/cm
# dC_dz_inj <- -slope_0_7cm_inj$coefficients[2]
# dC_dz_mol_inj <- ppm_to_mol(dC_dz_inj,"ppm",out_class = "units")#mol/m^3/cm
# Fz_mumol_per_s_m2_inj <- best_DS$DS_1  * dC_dz_mol_inj * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2
# F_Comsol$Fz_inj[F_Comsol$date == mod_dates_all[[j]]] <- Fz_mumol_per_s_m2_inj

dC_dz#ppm/cm
#DS = -FZ * dz / dC

dC_dz_mol <- ppm_to_mol(dC_dz,"ppm",out_class = "units")#mol/m^3/cm

Fz_mumol_per_s_m2 <- best_DS$DS_1  * dC_dz_mol * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2
#Fz_mumol_per_s_m2 <- 3.603326e-06  * dC_dz_mol * 100 * 10^6#m2/s * mol/m3/m = mol/s/m2


F_Comsol$Fz[F_Comsol$date == mod_dates_all[[j]]] <- Fz_mumol_per_s_m2
for(k in 1:n_DS_num){
F_Comsol[F_Comsol$date == mod_dates_all[[j]],paste0("DSD0",k)] <- DS_profil$DS[k]/DS_profil$D0[k]
F_Comsol[F_Comsol$date == mod_dates_all[[j]],paste0("DS",k)] <- DS_profil$DS[k]
}
}
#F_list[[l]] <- F_Comsol
#}
#################################
#plots
#ggplot(subset(data))+geom_line(aes(date,CO2_mol_per_m3,col=as.factor(tiefe)))+geom_vline(xintercept = kammer_dates_all$date)+xlim(ymd_h(c("2020.07.06 00","2020.07.06 16")))
 
#tiefe10 <- subset(data,tiefe==-10.5)
#NA_times <- tiefe10$date[is.na(tiefe10$CO2_mol_per_m3)]

#F_Comsol_snopt <- F_Comsol
ggplot(subset(Kammer_flux))+
  geom_ribbon(aes(x=date,ymin=CO2flux_min,ymax=CO2flux_max,fill=kammer),alpha=0.2)+
  geom_line(aes(date,CO2flux,col=kammer))+
  ggnewscale::new_scale_color()+
  geom_point(data=subset(F_Comsol),aes(date,Fz))+
  #geom_point(data=F_Comsol_snopt,aes(date,Fz,col="glm"))+
  #geom_point(data=F_Comsol,aes(date,Fz_inj,col=""))+
  scale_color_manual("COMSOL",values=1:2)+
  #xlim(c(min(F_Comsol$date[-1]),max(F_Comsol$date[])))+
  labs(title=paste(offset_method,optim_method,n_DS),y=expression(CO[2]*"flux ["*mu * mol ~ m^{-2} ~ s^{-1}*"]"))#+
  #ggsave(paste0(plotpfad,"Flux_Kammer_Comsol.png"),width=7,height = 7)

# ggplot(subset(CO2_flux))+
#   geom_point(aes(date,mumol_per_s_m2,col="kammer"))+
#   geom_line(data=F_Comsol,aes(date,Fz,col="Comsol"),size=2)+
#   xlim(c(min(mod_dates_all)-3600*24*1,max(mod_dates_all)+3600*24*3))#+ggsave(paste0(plotpfad,"Flux_Kammer_Comsol",date_chr,".png"),width=7,height = 7)

data_plot <- data %>%
  group_by(tiefe,date_hour=round_date(data$date,"hours")) %>%
  summarise(DSD0_PTF_min = min(DSD0_PTF_min,na.rm=T),DSD0_PTF_max = max(DSD0_PTF_max),DSD0_PTF= mean(DSD0_PTF),date=mean(date))
#F_Comsol_long <- tidyr::pivot_longer(F_Comsol,starts_with("DS"),values_to="DSD0")
F_Comsol_long <- tidyr::pivot_longer(F_Comsol,starts_with("DS"),names_pattern = "(DSD?0?)(\\d)",values_to = "test",names_to=c(".value","number"))
#F_Comsol_snopt_long <- tidyr::pivot_longer(F_Comsol_snopt,starts_with("DS"),values_to="DSD0")

ggplot(subset(data_plot))+
  geom_line(data=F_Comsol_long,aes(date,DS,col=number,linetype="nelder"))
ggplot(subset(data_plot))+
  geom_ribbon(aes(x=date,ymin=DSD0_PTF_min,ymax=DSD0_PTF_max,fill=as.factor(tiefe)),alpha=0.2)+
  geom_line(aes(date,DSD0_PTF,col=as.factor(tiefe)))+
  ggnewscale::new_scale_color()+
  geom_line(data=F_Comsol_long,aes(date,DSD0,col=name,linetype="nelder"))+
  #geom_line(data=F_Comsol_snopt_long,aes(date,DSD0,col=name,linetype="snopt"))+
  xlim(c(min(mod_dates_all[-1])-3600*24*1,max(mod_dates_all)+3600*24*3))

ggplot(data)+
  geom_line(aes(date,CO2_tracer_glm,col=as.factor(tiefe)))+
  facet_wrap(~tiefe,scales="free")+
  geom_rect(data = subset(Pumpzeiten, Pumpstufe != 0),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf,fill="injection"),alpha=0.3)+
  xlim(c(min(mod_dates_all)-3600*24*4,max(mod_dates_all)+3600*24*4))
ggplot(data)+
  geom_line(aes(date,CO2_tracer_gam,col=as.factor(tiefe)))+
  facet_wrap(~tiefe,scales="free")+
  geom_rect(data = subset(Pumpzeiten, Pumpstufe != 0),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf,fill="injection"),alpha=0.3)+
  xlim(c(min(mod_dates_all)-3600*24*4,max(mod_dates_all)+3600*24*4))
ggplot(data)+
  geom_line(aes(date,CO2_tracer_drift,col=as.factor(tiefe)))+
  facet_wrap(~tiefe,scales="free")+
  geom_rect(data = subset(Pumpzeiten, Pumpstufe != 0),aes(xmin=start,xmax=ende,ymin=-Inf,ymax=Inf,fill="injection"),alpha=0.3)+
  xlim(c(min(mod_dates_all)-3600*24*4,max(mod_dates_all)+3600*24*4))

inj <- ggplot(data)+
  geom_line(aes(date,CO2_roll_inj,col=as.factor(tiefe)))+
  xlim(c(min(mod_dates_all)-3600*24*1,max(mod_dates_all)+3600*24*3))
ref <- ggplot(data)+
  geom_line(aes(date,CO2_roll_ref,col=as.factor(tiefe)))+
  xlim(c(min(mod_dates_all)-3600*24*1,max(mod_dates_all)+3600*24*3))
egg::ggarrange(inj,ref)  

Kammer_agg <- Kammer_flux %>% group_by(day) %>% summarise(kammer_min = min(CO2flux_min),kammer_max = max(CO2flux_max), kammerflux = mean(CO2flux))

F_Comsol$day <- as_date(F_Comsol$date)
F_vergleich <- merge(F_Comsol,Kammer_agg,by="day")
ggplot(F_vergleich)+geom_point(aes(kammerflux,Fz))+geom_abline(intercept = 0,slope=1)+geom_errorbar(aes(xmin=kammer_min,xmax=kammer_max,y=Fz))+xlim(c(1,5.5))+ylim(c(1,5.5))
