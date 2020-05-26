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
if(chamber=="manuelle Kammer"){
  Kammer<-as.data.frame(Kammer)

  Grundfl_kammer<-Kammer$Grundfl._cm2[Kammer$durchmesser_typ=="unten"]
  kammer_Vol<-Kammer$Gesamt_vol_cm3[1]
  GGA_Vol <- GGA_Vol$Volume_effektive_cm3[GGA_Vol$Analyzer==GGA]
  kragen_Vol <- (kragen$height-Kammer$Hoehe_cm[1])*Grundfl
  kragen_rand_Vol <- kragen$height*Grundfl_kammer - kragen$height*Grundfl
  Vol <- kammer_Vol + GGA_Vol + ifelse(kragen_Vol > 0, kragen_Vol, 0) - kragen_rand_Vol
}else{
  #Vol in cm3
  Grundfl<-Kammer$Kragen_Grundfl_innen_cm2 #cm2
  Hoehe<-Kammer$Kragen_Hoehe_cm#cm
  Grundfl_aussen<-Kammer$Kragen_Grundfl_aussen_cm2
  Vol_kammer<-Kammer$Kammer_Volumen_cm3

  Vol_basis<-Grundfl*(Hoehe-Messungen$Tiefe-Messungen$Ueberstand)+Vol_kammer
  Vol_schlauch<-20 #cm3
  Vol_GGA<-0
  Vol<-Vol_basis-Messungen$Ueberstand*(Grundfl_aussen-Grundfl)+Vol_schlauch+Vol_GGA#cm3
}


flux <- calc_flux(data = dataset,
          group="kammer",
          gas = "CO2",
          Vol = Vol,
          Grundfl = Grundfl,
          aggregate = F)
flux[[1]]
}





