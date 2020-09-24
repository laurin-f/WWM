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

comsol <- read.csv(file=paste0(comsolpfad,"DS_D0_mat.txt"))
meta_22 <- readxl::read_xls(paste0(metapfad_ds,"SAS Protokoll_22_0720Auswertung.xls"),sheet=2)
meta <- readxl::read_xls(paste0(metapfad_ds,"Protokoll_28_07.xls"),sheet=1)

files <- list.files(paste0(datapfad_ds,"Rohdaten_28_07/"),full.names = T)

fileID_pattern <- "[a-z|A-Z]{2}\\d{3}[a-z|A-Z]\\d{2}"

Lines <- lapply(files, readLines)

for(i in seq_along(Lines)){
  fileID_i <- tolower(str_extract(files[i],fileID_pattern))
  kammer_i <- meta$Kammer[meta$Dateiname == fileID_i]
  kammer_i <- str_pad(kammer_i,2,"left","0")
  material_i <- meta$Material[meta$Dateiname == fileID_i]
  material_i <- str_replace_all(material_i,c("\\s"=""))
  von_bis <- meta$Messung[meta$Dateiname == fileID_i]
  von <-  as.numeric(str_extract(von_bis,"^\\d+")) + 4
  bis <-  as.numeric(str_extract(von_bis,"\\d+$")) + 4
  lines_i <-Lines[[i]]
  for(j in seq_along(von)){
    
  lines_i[von[j]:bis[j]] <- str_replace(lines_i[von[j]:bis[j]],paste0("(?<=\\d{2}:\\d{2},)",fileID_pattern,"\\.\\d{3}(?=,c:)"),paste0(j,"_",material_i[j],"_",kammer_i[j],"16"))
  }
  writeLines(lines_i,str_replace(files[i],".asc$",".aes"))
}

#results 22_07
files_DSD0_22 <- list.files(aufbereitete_ds,pattern="^JL22.*.asc$",full.names = T)
results_22_list <- lapply(files_DSD0_22,read.csv,header=F,stringsAsFactors=F)
results_22 <- do.call(rbind,results_22_list)

colnames(results_22) <- c("kammer","Labornummer","DS","DSD0","DSD0fqr","DSfqr","fileID","tension")
results_22$fileID <- str_replace_all(results_22$fileID,c("\\s"=""))
results_22$material <- tolower(as.character(factor(results_22$fileID,levels=meta_22$Dateiname,labels=meta_22$Material)))

files_DSD0_28 <- list.files(aufbereitete_ds,pattern="^JL28.*.asc$",full.names = T)
results_28_list <- lapply(files_DSD0_28,read.csv,header=F,stringsAsFactors=F)
results_28 <- do.call(rbind,results_28_list)

colnames(results_28) <- c("kammer","Labornummer","DS","DSD0","DSD0fqr","DSfqr","fileID","tension", "material","kommentar")

results_28$material <- str_replace(results_28$material,"\\s","")
results_28$Versuch <- "Versuch 2"
results_22$Versuch <- "Versuch 1"
results_22$kommentar <- "stoff"
results <- rbind(subset(results_22,material=="splitt und sand"),results_28)

comsol$material <- tolower(str_replace(comsol$material,"Sand & Splitt","Splitt und Sand"))

thomas_ref <- data.frame(material=c("sand","splitt","splitt und sand"), DS_D0=c(0.239, 0.235, 0.185),method="Flühler \n(Laemmel et al. 2017)")
ggplot()+
  geom_boxplot(data=subset(results,material!="leer"),aes(material,DSD0,fill="Fluehler"),alpha=0.3)+
  geom_boxplot(data=comsol,aes(material,DS_D0_mod,fill="gradient"),alpha=0.5)#+
  #geom_point(data=thomas_ref,aes(material,DS_D0,fill="Laemmel et al."),alpha=0.5)

###
#save
save(results,comsol,file=paste0(aufbereitete_ds,"Labor_Vergleich.RData"))

ggplot()+
  geom_point(data=subset(results,material!="leer"),aes(factor(material,levels=c("sand","splitt und sand","splitt"),labels=c("sand","mixture","grit")),DSD0,col="Lab",shape="Lab"))+
  geom_point(data=comsol,aes(factor(material,levels=c("sand","splitt und sand","splitt"),labels=c("sand","mixture","grit")),DS_D0_mod,col="in situ",shape="in situ"))+labs(y="DS/D0",x="",col="",shape="")+scale_color_manual(values=1:2)+ylim(c(0,0.3))+ggsave(paste0(plotpfad,"sandkiste/Labor_Vergleich.png"),width=5,height=3)
  
mat_gr <- comsol %>% group_by(material) %>% summarise(DSD0_gradient=paste0(round(mean(DS_D0_mod),3)," (",round(sd(DS_D0_mod),3),")"))
mat_lab <- subset(results,material!="leer") %>% group_by(material) %>% summarise(DSD0_lab=paste0(round(mean(DSD0),2)," (",round(sd(DSD0),3),")"))
write.csv2(file=paste0(aufbereitete_ds,"MS_table.csv"),merge(mat_gr,mat_lab))
