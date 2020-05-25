#pfade definieren

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"

metapfad<-paste0(hauptpfad,"Daten/Metadaten/")

#flux.kammer<-function(ort="Schauinsland",
#                      messnr){

chamber_flux <- function(messnr=NULL,
                         chamber="manuelle Kammer",
                         mess_dir="Vorgarten/",
                         kragenfile = "Vorgarten/Kragenhoehen.txt",
                         GGA = "gga"){


  Messungen<-readxl::read_xlsx(paste0(metapfad,mess_dir,"Kammermessungen.xlsx"))
  Kammer<-readxl::read_xlsx(paste0(metapfad,"Kammermessungen/Kammer_Meta.xlsx"),sheet=chamber)
  GGA_Vol <- readxl::read_xlsx(paste0(metapfad,"GGA/Kammervolumen.xlsx"))
  kragen <- read.csv(paste0(metapfad,kragenfile))

  kragen$height <- apply(str_split(kragen$height_cm,"\\s",simplify = T),1,function(x) mean(as.numeric(x),na.rm=T))

  if(is.null(messnr)){
  messnr <- seq_along(Messungen$Datum)
}

beginn<-ymd_hm(paste(Messungen$Datum[messnr],format(Messungen$beginn[messnr],"%H:%M")))
ende<-ymd_hm(paste(Messungen$Datum[messnr],format(Messungen$ende[messnr],"%H:%M")))

data.raw<-read_db("GGA.db",GGA,datelim=c(beginn,ende))



#dataset<-split.chamber(data=data.agg,closing=5,opening = -30,t_max=9)
dataset<-split_chamber(data=data.raw,
                       closing_before = 20,
                       closing_after = 40,
                       opening_before = 10,
                       opening_after = 0,
                       t_max=4,
                       t_init = 1,
                       t_min = 2)


dataset$kammer<-NA
KammerIDs<-colnames(Messungen)[-(1:3)]
for(i in seq_along(KammerIDs)){
  temp_kammer<-KammerIDs[i]
  temp_messid<-str_split(Messungen[messnr,-(1:3)][i],"\\.|,")[[1]]
  dataset$kammer[dataset$messid %in% temp_messid] <- temp_kammer
}

#Grundfl_cm2

Vol <- na.omit(Kammer$Gesamt_vol_cm3)+GGA_Vol$Volume_effektive_cm3[GGA_Vol$Analyzer==GGA]+(kragen$height-Kammer$Hoehe_cm[1])*Grundfl
flux <- calc_flux(data = dataset,
          group="kammer",
          gas = "CO2",
          Vol = Vol,
          Grundfl = Grundfl,
          aggregate = F)
flux[[1]]
}



# data.list<-lapply(KammerIDs,function(x) subset(dataset,dataset$kammer==x))
# names(data.list) <- KammerIDs
#
# fm_CO2.list<-lapply(data.list,function(x) glm(CO2~zeit,data=x))
# fm_CH4.list<-lapply(data.list,function(x) glm(CH4~zeit,data=x))
#
# #fm_CO2<-glm(co2~zeit,data=dataset)
# #fm_CH4<-glm(CH4~zeit,data=dataset)
#
# #dataset$CO2.preds<-predict(fm_CO2,newdata = data.frame(CO2=dataset$CO2,zeit=dataset$zeit))
# #dataset$CH4.preds<-predict(fm_CH4,newdata = data.frame(CH4=dataset$CH4,zeit=dataset$zeit))
#
# dataset$CO2.preds<-NA
# dataset$CH4.preds<-NA
#
# for(i in KammerIDs){
#   dataset$CO2.preds[which(dataset$kammer==i)] <- predict(fm_CO2.list[[i]],
#                                                          newdata = data.frame(CO2=dataset$CO2[which(dataset$kammer==i)],
#                                                                               zeit=dataset$zeit[which(dataset$kammer==i)]))
#   dataset$CH4.preds[which(dataset$kammer==i)] <- predict(fm_CH4.list[[i]],
#                                                          newdata = data.frame(CH4=dataset$CH4[which(dataset$kammer==i)],
#                                                                               zeit=dataset$zeit[which(dataset$kammer==i)]))
#
# }


#slope_CO2 <- fm_CO2$coefficients[2]/10^6#cm3/(cm3 min)
#slope_CH4 <- fm_CH4$coefficients[2]/10^6#cm3/(cm3 min)

# slope_CO2 <- sapply(fm_CO2.list,function(x) x$coefficients[2]/10^6)#cm3/(cm3 min)
# slope_CH4 <- sapply(fm_CH4.list, function(x) x$coefficients[2]/10^6)#cm3/(cm3 min)
#
#
#
# if(ort=="Schauinsland"){
#   Kammer<-as.data.frame(Kammer)
#
#   Grundfl<-Kammer$Grundfl._cm2[Kammer$durchmesser_typ=="unten"]
#   Vol<-Kammer$Gesamt_vol_cm3[1]
# }else{
#   #Vol in cm3
#   Grundfl<-Kammer$Kragen_Grundfl_innen_cm2 #cm2
#   Hoehe<-Kammer$Kragen_Hoehe_cm#cm
#   Grundfl_aussen<-Kammer$Kragen_Grundfl_aussen_cm2
#   Vol_kammer<-Kammer$Kammer_Volumen_cm3
#
#   Vol_basis<-Grundfl*(Hoehe-Messungen$Tiefe-Messungen$Ueberstand)+Vol_kammer
#   Vol_schlauch<-20 #cm3
#   Vol_GGA<-0
#   Vol<-Vol_basis-Messungen$Ueberstand*(Grundfl_aussen-Grundfl)+Vol_schlauch+Vol_GGA#cm3
# }
#
# p_Pa <- 101.3*1000#PA # N/m2 # kg/(m s2)
# T_deg<-15 #°C
#
# #Konstanten
# R <- 8.314 #kg m2/(s2 mol K)
# M_CO2 <- 44.01 #g/mol
# M_CH4 <- 16.04 #g/mol
# T_K <- T_deg+273.15 #K
# m_CO2<-p_Pa*M_CO2/(R*T_K)/10^6 #g/cm3
# m_CH4<-p_Pa*M_CH4/(R*T_K)/10^6 #g/cm3
#
# #berechnung Flux
#
# flux_CO2_cm3<-slope_CO2*Vol/(Grundfl/10^4) #cm3 /(min m2)
# flux_CO2_g<-flux_CO2_cm3*m_CO2 #g/(min m2)
# flux_CO2_g*60 #g/(h m2)
#
# flux_CH4_cm3<-slope_CH4*Vol/(Grundfl/10^4) #cm3 /(min m2)
# flux_CH4_g<-flux_CH4_cm3*m_CH4 #g/(min m2)
# flux_CH4_g*10^6*60 #mug/h m2

