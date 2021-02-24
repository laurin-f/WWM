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

load(file=paste0(aufbereitetpfad_prod,"data_agg.RData"))


meas_depths_2 <- seq(0,40,0.5)
meas_points_2 <- data.frame(R=4,Z=meas_depths_2)
write.table(meas_points_2,file = paste0(metapfad,"COMSOL/meas_points_produktionseimer_r3.txt"),row.names = F,col.names = F)


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


data_agg[,paste0("prod_",1:3,"_mol_m2_s")] <- data_agg[,paste0("prod_",1:3,"_mumol_m2_s")] / 10^6 * change_unit(grundflaeche,unit_out = "m^2") / rep(A,each=nrow(data_agg))



comsol_ls <- vector("list",length(unique(data_agg$ID)))
for(i in unique(data_agg$ID)){
input_pars_i <-data_agg %>% 
  ungroup()  %>%
  filter(ID == i) %>%
  select(matches("prod_\\d_mol_m2_s$")) %>% summarise_all(mean)

names(input_pars_i) <- paste0("prod_",1:3)
input_pars_i$DS_accurel <- DS


file_i <- paste0("CO2_flux_prod_",i,".txt")
comsol_exe(model="Produktionseimer",input_pars=input_pars_i,outfile_new = file_i,overwrite = F)
comsol_ls[[i]] <- read.csv(paste0(comsolpfad,file_i),skip=9,sep="",header=F)
colnames(comsol_ls[[i]]) <- c("r","z","c","flux")
comsol_ls[[i]]$ID <- i
}



comsol <- do.call(rbind,comsol_ls)

comsol$tiefe <- comsol$z - 40
comsol$flux_mumol <- comsol$flux * 10^6
comsol$treat <- as.character(factor(comsol$ID,levels=data_agg$ID,labels = data_agg$treat))

ggplot()+
  geom_line(data=subset(comsol),aes(flux_mumol,tiefe,col=as.factor(ID)),orientation = "y")+
  geom_point(data=subset(data_agg),aes(as.numeric(Fz),tiefe+1.75,col=as.factor(ID)))+
  facet_wrap(~treat,scales = "free")

ggplot()+
  geom_line(data=subset(comsol),aes(ppm_to_mol(c,unit_in = "mol/m^3"),tiefe,col=as.factor(ID)),orientation = "y")+
  geom_point(data=subset(data_agg),aes(CO2,tiefe,col=as.factor(ID)))+
  facet_wrap(~treat,scales = "free")

comsol_DO <- comsol
ggplot(df)+geom_line(aes(c,z),orientation = "y")



df <- data.frame(a=1:10,b=11:20,c=21:30)
v <- as.data.frame(t(c("a"=1,"b"=2,"c"=3)))
(df)*v
diag(v)
