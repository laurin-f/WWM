#------------------------------------------------------------------------------------ #
#
#                           Septentest - CH4 Emission

#              Skript geschrieben von Verena Lang (Verena.Lang@forst.bwl.de)
#                              
#                                bearbeitet Laurin OSterholt
#
#-------------------------------------------------------------------------------------#


### ----------------------------------VORBEREITUNG ------------------------------------

# Alle möglichen Altdaten, werden aus der R-Arbeitsumgebung entfernt, Es werden extrener Funktionen über ein seperates R-Script geladen, die später notwendig sind. Der Ablageort, aller gemessenen Chargen (mainDir), sowie der Name der aktuell gemessenen Charge, müssen manuell festegelgt werden, bzw. mit jeder Validierung entsprechend angepasst werden (cHarge). Abschließend werden notwendige Pakete geladen und falls nötig installiert.

#pfade definieren

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Materialtest/")

# alle Altdaten, die im Zwischenspeicher sind entfernen


## Funktionen-Skript laden
source("O:/TRANSP/Lang_Verena/HIWI/Datenpruefung_Alexander/Funktionen.R")

# Working Directors definieren
mainDir<- "H:/FVA-Projekte/P01677_WindWaldMethan/Daten/Urdaten/Materialtest/"

#Charge <- "Versuch2"
setwd(mainDir)

# Abfrage und Aufrufen aller notwendigen Pakete

# Abfrage und Aufrufen aller notwendigen Pakete
list.of.packages = c("tidyr", "stringr", "dplyr", "ggplot2", "lubridate", "RFmarkerDetector")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {install.packages(new.packages)}
lapply(list.of.packages, require, character.only=T)


### ---------------------------------DATEN LADEN--------------------------------------

Septen_CO2<-read.csv(dir()[grep("^CO2_Material", dir())][1], 
                           sep = ",", dec = ".", 
                           skip = 2, header = TRUE, 
                           stringsAsFactors = FALSE, as.is = T)%>%
  mutate(Element=ifelse(grepl("O2|CO2|N2", Area), Area, NA))%>%
  fill(Element)%>%
  mutate(Element=ifelse(is.na(Element), "Ar", Element))

if(length(grep("^CO2_Material", dir()))>1){
  for(i in 2:length(grep("^CO2", dir()))){
    Data<-read.csv(dir()[grep("^CO2", dir())][i], 
                   sep = ",", dec = ".", 
                   skip = 2, header = TRUE, 
                   stringsAsFactors = FALSE, as.is = T)%>%
      mutate(Element=ifelse(grepl("O2|CO2|N2", Area), Area, NA))%>%
      fill(Element)%>%
      mutate(Element=ifelse(is.na(Element), "Ar", Element))
    Septen_CO2<-bind_rows(Septen_CO2, Data)
  }
  rm(Data)
}

Septen_CH4<-read.csv(dir()[grep("^CH4_Material", dir())][1], 
                           sep = ",", dec = ".", 
                           skip =2, header = TRUE, 
                           stringsAsFactors = FALSE, as.is = T)%>%
  mutate(Element=ifelse(grepl("C2H4", Area), Area, NA))%>%
  fill(Element)%>%
  mutate(Element=ifelse(is.na(Element), "CH4", Element))  

if(length(grep("^CH4_Material", dir()))>1){
  for(i in 2:length(grep("^CH4", dir()))){
    Data<-read.csv(dir()[grep("^CH4", dir())][i], 
                   sep = ",", dec = ".", 
                   skip =2, header = TRUE, 
                   stringsAsFactors = FALSE, as.is = T)%>%
      mutate(Element=ifelse(grepl("C2H4", Area), Area, NA))%>%
      fill(Element)%>%
      mutate(Element=ifelse(is.na(Element), "CH4", Element))  
    Septen_CH4<-bind_rows(Septen_CH4, Data)
  }
  rm(Data)
}  
  

## Abfrage, ob beide Datnblätter vorhanden sind, und falls nicht nur eins zum Komplettdatenblatt zusammenführen

Septen_both<-bind_rows(Septen_CO2, Septen_CH4)

#rm(Septen_CH4, Septen_CO2) 

# geht irgendwie nicht in der Pipe, also seperat:
# falsch getippte doppelte Leerzeichen müssen durch einzelne ersetzt werden, sonst kann der Name nicht abgetrennt werden.
Septen_both$Sample <- str_replace(Septen_both$Sample,"15m","_15m")
Septen_both$Study<-str_replace(Septen_both$Sample, "  ", " ")
Septen_both$Sample<-str_replace(Septen_both$Sample, "  ", " ")


# unnötige Spalten und Zeilen löschen
Septen<-Septen_both%>%
  select(-starts_with("X"))%>%
  rename("File_Name"="File", "Sample_Name"="Sample", "Date_of_Injection"="Date.of")%>%
  filter(grepl("File|Name", File_Name)==FALSE)%>%
  filter(grepl("Ar|O2|CO2|N2|CH4|C2H4", Area)==FALSE)%>%
  mutate(Date_of_Injection=readr::parse_date(Date_of_Injection, "%m/%d/%Y"))%>%
  select(Element, Sample_Name, Study, Date_of_Injection, Quantity, Area, Height)%>%
  filter(!is.na(Date_of_Injection))%>%
  filter(!grepl("Vorlauf|vorlauf|Ausheizen|ausheizen|Shutdown|shutdown|Aufheizen|aufheizen|H2O|Check|check", Sample_Name))%>%
  mutate(MST_ID=paste(sapply(str_split(Study, "_"), `[`, 1), 
                      sapply(str_split(Study, "_"), `[`, 2),sep="_"))%>%
  mutate(MST_ID=ifelse(grepl("AQS|aqs|luft|Luft|RL", MST_ID), "RL", MST_ID))%>%
  mutate(MST_ID=ifelse(grepl("std|STD|Std|wSTD|wStd|wstd", MST_ID), "STD", MST_ID))%>%
  mutate(MST_ID=ifelse(grepl("cal|Cal|CAL", MST_ID), "Cal", MST_ID))%>%
  mutate(MST_ID=ifelse(grepl("He", MST_ID), "He", MST_ID))%>%
  mutate_at(c('Area', 'Quantity', 'Height'), as.numeric)%>%
  mutate_at(c('Element'), as.factor)%>%
  mutate(Element = factor(Element, levels = c("Ar", "O2", "CO2", "N2", "CH4", "C2H4", "N2O")))

### ------------------------------------------------------------------------###
###                   BEGINN SPEICHERN SELEKTIERTER DATENBLÄTTER            ###
### ------------------------------------------------------------------------###

Septen$Quantity[Septen$Element == "CO2"] <- Septen$Quantity[Septen$Element == "CO2"]*10^4

gewichte <- readxl::read_xlsx(paste0(metapfad,"Laurin_Materialtest.xlsx"),sheet="Gewichte")
gewichte$ID <- str_replace_all(gewichte$ID,c("Sch"="Schl","PL"="PKM","_K"="_20C_","W"="50C_"))
gewichte_sub <- gewichte[!is.na(gewichte$`Gewicht (g)`),1:2]

PG_long<-Septen[Septen$MST_ID=="PG_20C",c("Element","Quantity")]
PG<-aggregate(list(Quantity = PG_long$Quantity),by=list(Element = PG_long$Element),mean)


Septen$gewicht <- as.numeric(as.character(factor(Septen$Sample_Name,levels = gewichte_sub$ID,labels= gewichte_sub$`Gewicht (g)`)))
Septen$PG_Anteil <- as.numeric(as.character(factor(Septen$Element,levels = PG$Element,labels= PG$Quantity)))
Septen$Quantity_pro_g <- Septen$PG_Anteil + (Septen$Quantity[id] - Septen$PG_Anteil) / ifelse(is.na(Septen$gewicht),1,Septen$gewicht)

# for(i in 1:nrow(gewichte_sub)){
#   id <- Septen$Sample_Name == gewichte_sub$ID[i]
#   Septen$Quantity_pro_g[id] <- PG$Quantity + (Septen$Quantity[id] - PG$Quantity) / gewichte_sub$`Gewicht (g)`[i]
# }


Summary<-Septen%>%
  select(Element, MST_ID, Quantity,Quantity_pro_g, Area, Height)%>%
  group_by(MST_ID, Element)%>%
  summarize_if(is.numeric, list(MEAN=mean, `%RSD`=rsd))%>%
  rename("Probenart"=MST_ID)

write.csv2(Summary, paste0("Zusammenfassung",".csv"))
### ------------------------------------------------------------------------###
###                                  ENDE                                   ###
### ------------------------------------------------------------------------###

### ------------------------------------------------------------------------###
###                     BEGINN GARFISCHE DARSTELLUNG                        ###
### ------------------------------------------------------------------------###



pltRL <- ggplot(subset(Septen,Element %in% c("CO2","CH4") & MST_ID %in% c("Gel_15m","RL")))+
  geom_boxplot(aes(MST_ID,Quantity,fill=MST_ID))+
  geom_jitter(aes(MST_ID,Quantity),width=.01)+
  ylab("ppm")+
  facet_wrap(~Element,scales="free_y")+
  ggsave("Boxplot_Materialtest_RL.pdf")

pltRL
pltPG <- ggplot(subset(Septen,Element %in% c("CO2","CH4") & !MST_ID %in% c("Cal","He","Gel_15m","RL","STD")))+
  geom_boxplot(aes(MST_ID,Quantity,fill=MST_ID))+
  geom_jitter(aes(MST_ID,Quantity),width=.01)+
  ylab("ppm")+
  facet_wrap(~Element,scales="free_y")+
  ggsave("Boxplot_Materialtest_PG.pdf")
pltPG

pltPG2 <- ggplot(subset(Septen,Element %in% c("CO2","CH4") & !MST_ID %in% c("Cal","He","Gel_15m","RL","STD")))+
  geom_boxplot(aes(MST_ID,Quantity_pro_g,fill=MST_ID))+
  geom_jitter(aes(MST_ID,Quantity_pro_g),width=.01)+
  geom_jitter(aes(MST_ID,Quantity),col=2,width=.01)+
  facet_wrap(~Element,scales="free_y")


Septen$Quantity-Septen$Quantity_pro_g
gridExtra::grid.arrange(pltRL,pltPG,ncol=1)

#################################################
#Quantitative auswertung
Summary_sub <- subset(Summary[,c(1:2,4)],Element %in% c("CO2","CH4") & !Probenart %in% c("Cal","He"))


Summary_spread <- tidyr::spread(Summary_sub, key = Probenart, value = Quantity_pro_g_MEAN)
Gel_RL <- Summary_spread$Gel_15m-Summary_spread$RL
names(Gel_RL) <- c("CO2","CH4")
Schl_PKM_PG <- Summary_spread[paste(rep(c("Schl","PKM"),each=2),c("20C","50C"),sep="_")]-Summary_spread$PG_20C
rownames(Schl_PKM_PG) <- c("CO2","CH4")


Zeit<-50
Schl_PKM_PG/Zeit*1000/60 #ppm/h/g

Summary_sub <- subset(Summary[,c(1:3)],Element %in% c("CO2","CH4") & !Probenart %in% c("Cal","He"))


Summary_spread <- tidyr::spread(Summary_sub, key = Probenart, value = Quantity_MEAN)
Gel_RL <- Summary_spread$Gel_15m-Summary_spread$RL
names(Gel_RL) <- c("CO2","CH4")
Schl_PKM_PG <- Summary_spread[paste(rep(c("Schl","PKM"),each=2),c("20C","50C"),sep="_")]-Summary_spread$PG_20C
rownames(Schl_PKM_PG) <- c("CO2","CH4")

Schl_PKM_PG/Zeit*1000/60 #ppm/h/g

zeit_Gel
