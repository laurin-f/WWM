detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
check.packages(c("ggplot2","lubridate","stringr","dplyr","units"))

#Pfade
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
arduinopfad<-paste0(hauptpfad,"Daten/Urdaten/Arduino/")
datapfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Arduino/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_prod<- paste0(hauptpfad,"Daten/Metadaten/Produktionseimer/")
metapfad_comsol<- paste0(hauptpfad,"Daten/Metadaten/COMSOL/")
aufbereitetpfad_prod<- paste0(hauptpfad,"Daten/aufbereiteteDaten/Produktionseimer/")
plotpfad_prod <- paste0(hauptpfad,"Dokumentation/Berichte/plots/produktionseimer/")

load(file=paste0(aufbereitetpfad_prod,"data_agg.RData"))


meas_depths_2 <- seq(0,40,0.5)
meas_points_2 <- data.frame(R=4,Z=meas_depths_2)
write.table(meas_points_2,file = paste0(metapfad,"COMSOL/meas_points_produktionseimer_r3.txt"),row.names = F,col.names = F)

  
  data_agg$CO2_mol_per_m3 <- ppm_to_mol(data_agg$CO2,"ppm",T_C = data_agg$temp)

  grundflaeche <- set_units(15^2*pi,"cm^2")
  #prod pro fläche nicht auf Fgrundfläche sondern auf 
  l <- set_units(1,"mm")
  r1u2 <- 50+0:2*40
  r3 <- 20+0:2*40
  r <- set_units(matrix(c(r1u2,r1u2,r3),3,3),"mm")
  G <- pi * (r+l/2)^2 - pi * (r-l/2)^2
  M <- l * 2 * pi * (r+l/2) + l * 2 * pi * (r-l/2)
  A <- colSums(2 * G + M)#mm^2
  A <- change_unit(A,unit_in = "mm^2",unit_out = "m^2")
  names(A) <- paste0("prod_",1:3,"_m2")
  
  
  
comsol_ls <- vector("list",length(unique(data_agg$ID)))
for(i in unique(data_agg$ID)){
  sub_i <- subset(data_agg,ID == i)
  for(j in 1:7){
    write.table(sub_i[sub_i$tiefe == (1:7*-3.5)[j],"CO2_mol_per_m3"],paste0(metapfad_comsol,"dom",j,".csv"),col.names = F,row.names = F,sep=",")
  }
  
  file_i <- paste0("CO2_flux_prod_optim",i,".txt")
  comsol_exe(model="Produktionseimer_optim",outfile_new = file_i,outfile_raw = "CO2_flux_prod_optim.txt",job="b2",overwrite = T)
  comsol_ls[[i]] <- read.csv(paste0(comsolpfad,file_i),skip=9,sep="",header=F)
  colnames(comsol_ls[[i]]) <- c("r","z","c","flux",paste0("prod_",1:3))
  comsol_ls[[i]]$ID <- i
}



comsol <- do.call(rbind,comsol_ls)

comsol$tiefe <- comsol$z - 40
comsol$flux_mumol <- comsol$flux * 10^6
comsol$treat <- as.character(factor(comsol$ID,levels=data_agg$ID,labels = data_agg$treat))
comsol[,paste0("prod_mumol_",1:3)] <- comsol[,paste0("prod_",1:3)] * 10^6 / change_unit(grundflaeche,unit_out = "m^2") * rep(A,each=nrow(comsol))

prod_comsol <- comsol %>% 
  group_by(ID,treat) %>% 
  select(matches("prod_mumol")) %>% 
  summarise_at(vars(matches("prod")),mean) %>% 
  tidyr::pivot_longer(matches("prod"),names_prefix = "prod_mumol_",names_to="tiefenstufe",values_to="prod_comsol")

prod_meas <- data_agg %>% 
  group_by(ID,treat) %>% 
  select(matches("prod_\\d_mumol")) %>% 
  summarise_at(vars(matches("prod")),mean) %>% 
  tidyr::pivot_longer(matches("prod"),names_pattern = "prod_(\\d)",names_to="tiefenstufe",values_to="prod")

prod <- merge(prod_comsol,prod_meas)
prod$tiefenstufe <- as.numeric(prod$tiefenstufe)

prod_df$tiefe[prod_df$tiefe == "30"] <- "40"
ggplot()+
  geom_line(data=subset(comsol),aes(flux_mumol,tiefe,col=as.factor(ID)),orientation = "y")+
  geom_point(data=subset(data_agg),aes(as.numeric(Fz),tiefe+1.75,col=as.factor(ID)))+
  geom_line(data=prod_df,aes(Fz,-as.numeric(tiefe),col=as.factor(ID)),orientation = "y")+
  facet_wrap(~treat,scales = "free")

ggplot()+
  geom_line(data=subset(comsol),aes(ppm_to_mol(c,unit_in = "mol/m^3"),tiefe,col=as.factor(ID)),orientation = "y")+
  geom_point(data=subset(data_agg),aes(CO2,tiefe,col=as.factor(ID)))+
  facet_wrap(~treat,scales = "free")




ggplot(prod)+geom_point(aes(prod_comsol,prod))


data_agg2$tiefenstufe <- data_agg2$prod_tiefe / -7
ggplot(prod)+
  geom_point(aes(prod_comsol,tiefenstufe,col="comsol"))+
  geom_point(aes(prod,tiefenstufe,col="theory"))+
  ylim(c(3,1))+
  facet_wrap(~ID)

ggplot(prod)+
  geom_point(aes(prod_comsol,tiefenstufe,col=as.factor(ID)))+
  geom_line(aes(prod,tiefenstufe,col=as.factor(ID),linetype="theory"),orientation = "y")+
  #geom_line(data=data_agg2,aes(P,tiefenstufe,col=as.factor(ID),linetype="agg2"),orientation = "y")+
  geom_line(data=data_agg,aes(P,tiefe/-7,col=as.factor(ID),linetype="agg"),orientation = "y")+
  ylim(c(3,1))+
  facet_wrap(~treat)
