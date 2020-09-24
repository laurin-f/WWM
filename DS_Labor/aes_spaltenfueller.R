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

datum <- "21_09"
meta <- readxl::read_xls(paste0(metapfad_ds,"Protokoll_",datum,".xls"),sheet=1)
files <- list.files(paste0(datapfad_ds,"Rohdaten_",datum,"/"),full.names = T)

fileID_pattern <- "[a-z|A-Z]{2}\\d{3}[a-z|A-Z]\\d{2}"

Lines <- lapply(files, readLines)
i<-1
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
    
    lines_i[von[j]:bis[j]] <- str_replace(lines_i[von[j]:bis[j]],paste0("(?<=\\d{2}:\\d{2},)",fileID_pattern,"\\.\\d{3}(?=,c:)"),paste0(material_i[j],"_",j,"_",kammer_i[j],"16"))
  }
  writeLines(lines_i,str_replace(files[i],".asc$",".aes"))
}