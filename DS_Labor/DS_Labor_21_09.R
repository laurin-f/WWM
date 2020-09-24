#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
datapfad_ds<-paste0(hauptpfad,"Daten/Urdaten/DS_Labor/")
aufbereitete_ds<-paste0(hauptpfad,"Daten/aufbereiteteDaten/DS_Labor/")
metapfad_ds<-paste0(hauptpfad,"Daten/Metadaten/DS_Labor/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units")
check.packages(packages)

comsol <- read.csv(file=paste0(comsolpfad,"DS_vorgarten_gam.csv"))

datum <- "21_09"
meta <- readxl::read_xls(paste0(metapfad_ds,"Protokoll_",datum,".xls"),sheet=1)
#results 22_07
files_DSD0 <- list.files(aufbereitete_ds,pattern="^SE21.*.asc$",full.names = T)
results_list <- lapply(files_DSD0,read.csv,header=F,stringsAsFactors=F)
results <- do.call(rbind,results_list)

colnames(results) <- c("kammer","Labornummer","DS","DSD0","DSD0fqr","DSfqr","fileID","tension")
results$fileID <- str_replace_all(results$fileID,c("\\s"=""))

results$material <- tolower(as.character(factor(paste0(results$kammer,tolower(results$fileID)),levels=paste0(meta$Kammer,meta$Dateiname),labels=meta$Material)))

colnames(comsol)
comsol_long <- tidyr::pivot_longer(comsol[,grep("DSD0",names(comsol))],matches("DSD0\\d"),values_to = "DSD0")
comsol_long$material <- str_replace_all(comsol_long$name,c("DSD01" = "0cm","DSD02" = "15cm","DSD03" = "20-150 cm"))
 colMeans(comsol[,-2])

ggplot()+
  geom_boxplot(data=subset(results,material!="10cm"),aes(material,DSD0,fill="Fluehler"),alpha=0.3)+
  geom_boxplot(data=comsol_long,aes(material,DSD0,fill="gradient"),alpha=0.5)#+
#geom_point(data=thomas_ref,aes(material,DS_D0,fill="Laemmel et al."),alpha=0.5)

ggplot()+
  geom_point(data=subset(results,material!="10cm"),aes(material,DSD0,col="Lab",shape="Lab"))+
  geom_point(data=comsol_long,aes(material,DSD0,col="in situ",shape="in situ"))+labs(y="DS/D0",x="",col="",shape="")+scale_color_manual(values=1:2)+ggsave(paste0(plotpfad,"Vorgarten/Labor_Vorgarten.png"),width=5,height=3)
mean(results$DSD0[results$material=="15cm"],na.rm = T)
mean(comsol$DSD02)
mat_gr <- comsol %>% group_by(material) %>% summarise(DSD0_gradient=paste0(round(mean(DS_D0_mod),3)," (",round(sd(DS_D0_mod),3),")"))
mat_lab <- subset(results,material!="leer") %>% group_by(material) %>% summarise(DSD0_lab=paste0(round(mean(DSD0),2)," (",round(sd(DSD0),3),")"))
write.csv2(file=paste0(aufbereitete_ds,"MS_table.csv"),merge(mat_gr,mat_lab))
