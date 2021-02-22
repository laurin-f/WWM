detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
check.packages(c("ggplot2","lubridate","stringr","dplyr","units"))

#Pfade
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
arduinopfad<-paste0(hauptpfad,"Daten/Urdaten/Arduino/")
datapfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Arduino/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_prod<- paste0(hauptpfad,"Daten/Metadaten/Produktionseimer/")
aufbereitetpfad_prod<- paste0(hauptpfad,"Daten/aufbereiteteDaten/Produktionseimer/")

load(file=paste0(aufbereitetpfad_prod,"data_prod_eimer.RData"))

meas_depths_sb <- (40-(0:7*3.5))
meas_points_sb <- data.frame(R=0,Z=meas_depths_sb)
write.table(meas_points_sb,file = paste0(metapfad,"COMSOL/meas_points_produktionseimer.txt"),row.names = F,col.names = F)

meas_depths_2 <- seq(0,40,0.5)
meas_points_2 <- data.frame(R=3.5,Z=meas_depths_2)
write.table(meas_points_2,file = paste0(metapfad,"COMSOL/meas_points_produktionseimer_r3.txt"),row.names = F,col.names = F)


l <- set_units(1,"mm")
r1u2 <- 50+0:2*40
r3 <- 20+0:2*40
r <- set_units(matrix(c(r1u2,r1u2,r3),3,3),"mm")
G <- pi * (r+l/2)^2 - pi * (r-l/2)^2
M <- l * 2 * pi * (r+l/2) + l * 2 * pi * (r-l/2)
A <- colSums(2 * G + M)#mm^2
names(A) <- paste0("prod_",1:3,"_mm2")
prod_df
#prod_mol #mol/m2/s
#########################################
#calc DS with COMSOL
tracer_prod <- subset(data,tracer==1)
prod_123 <- subset(data,treat=="1_1_1"& date < min(tracer_prod$date))
prod_mean <- prod_123 %>% 
  group_by(tiefe) %>% 
  summarise(CO2 = mean(CO2,na.rm=T))
tracer_mean <- tracer_prod %>% 
  group_by(tiefe) %>% 
  summarise_at(vars(matches("CO2|inj_mol|date|temp|prod")), mean,na.rm=T)

tracer_mean$tracer <- tracer_mean$CO2 - prod_mean$CO2
tracer_mean$CO2_ref <- prod_mean$CO2
# ggplot()+
#   geom_line(data=prod_mean,aes(CO2,tiefe,col="prod"))+
#   geom_line(data=tracer_mean,aes(CO2,tiefe,col="tracer"))+
#   geom_line(data=tracer_mean,aes(tracer,tiefe,col="tracer"))

#################################################
#runcomsol
colnames(tracer_mean) <- str_replace_all(colnames(tracer_mean),c("^temp$"="T_soil","^tracer$"="CO2_tracer_gam"))

comsol_out <- run_comsol(data=tracer_mean,mod_dates = mean(tracer_prod$date),n_DS=1,modelname = "Diffusion_Sandbox_optim",z_soil_cm = 40,read_all = F,plot=T,overwrite = F)
DS <- comsol_out$DS
DSD0 <- comsol_out$DSD0


save(DS,DSD0,file=paste0(aufbereitetpfad_prod,"Comsol_out.RData"))
