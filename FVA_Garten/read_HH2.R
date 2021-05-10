#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad_bf<- paste0(hauptpfad,"Daten/Urdaten/Bodenfeuchte_FVA_Garten/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)

files <- list.files(datapfad_bf,pattern = ".CSV$",full.names = T)
data_j <- vector("list",length(files))
for(j in seq_along(files)){
  lines <- readLines(files[j])
  pr2lines <- grep("Device >>,PR2",lines)
  emptylines <- grep("^$",lines)
  
  
  data_ls <- vector("list",length(pr2lines))
  for(i in seq_along(pr2lines)){
  endline <- emptylines[which(emptylines-pr2lines[i] > 0)[1]]
  if(is.na(endline)){
    endline <- length(lines)+1
  }
  lines_i <- lines[(pr2lines[i]+5):(endline-1)]
  lines_i <- str_remove(lines_i,",$")
  data_ls[[i]] <- as.data.frame(str_split(lines_i,",",simplify = T),stringsAsFactors = F)
  
  
  colnames(data_ls[[i]]) <- c("Time","sample","plot","device",paste0(c("SWC_","Error_"),rep((1:4)*100,each=2)))
  }
  ncols <- sapply(data_ls,ncol)
  data_j[[j]] <- do.call(rbind,data_ls[ncols == 12])
}
data <- do.call(rbind,data_j)
data$date <- dmy_hms(data$Time)

data[,grep("SWC",colnames(data))] <- sapply(data[,grep("SWC",colnames(data))],as.numeric)
data$ID <- as.numeric(data$sample) %% 4
data$plot <- factor(data$ID,levels = 0:3,labels=c("D","A","B","C"))
data_long <- tidyr::pivot_longer(data,matches("SWC"),values_to = "SWC",names_to = "tiefe",names_prefix = "SWC_")
ggplot(data_long)+geom_point(aes(date,SWC,col=tiefe,shape=plot))

