
#flux.kammer<-function(ort="Schauinsland",
#                      messnr){

#' function to calculate chamber flux
#'
#' @param mess_dir directory in which the metadata of the measurements is stored
#' @param messnr which measurement should be used from metadata. default is NULL = all measurements are used
#'
#' @return
#' @export
#'
#' @examples
chamber_flux <- function(mess_dir="Vorgarten",
                         messnr=NULL,
                         aggregate = F,
                         ...){


  Messungen<-readxl::read_xlsx(paste0(metapfad,mess_dir,"/Kammermessungen.xlsx"))
  kragen <- readxl::read_xlsx(paste0(metapfad,mess_dir,"/Kammermessungen.xlsx"),sheet = 2)

  if(is.null(messnr)){
    messnr <- seq_along(Messungen$Datum)
  }

  GGA <- unique(Messungen$GGA[messnr])
  chamber <- unique(Messungen$kammer[messnr])


  Kammer<-readxl::read_xlsx(paste0(metapfad,"Kammermessungen/Kammer_Meta.xlsx"),sheet=chamber)
  GGA_Vol <- readxl::read_xlsx(paste0(metapfad,"GGA/Kammervolumen.xlsx"))


  kragen$height <- apply(str_split(kragen$height_cm,",",simplify = T),1,function(x) mean(as.numeric(x),na.rm=T))


beginn<-min(ymd_hm(paste(Messungen$Datum[messnr],format(Messungen$beginn[messnr],"%H:%M"))))
ende<-max(ymd_hm(paste(Messungen$Datum[messnr],format(Messungen$ende[messnr],"%H:%M"))))

data.raw<-read_db("GGA.db",GGA,datelim=c(beginn,ende))



#dataset<-split.chamber(data=data.agg,closing=5,opening = -30,t_max=9)
dataset<-split_chamber(data=data.raw,
                       # closing_before = 40,
                       # closing_after = 40,
                       # opening_before = -30,
                       # opening_after = -10,
                       # t_max=2,
                       # t_init = 0.1,
                       # t_min = 2
                       ...)



dataset$kammer<-NA

reihenfolge <- t(str_split(Messungen$reihenfolge[messnr],",",simplify = T))


for(i in seq_along(reihenfolge)){
  dataset$kammer[dataset$messid == i] <- reihenfolge[i]
}

Vol_GGA <- GGA_Vol$Volume_effektive_cm3[GGA_Vol$Analyzer==GGA]
#Grundfl_cm2
if(chamber=="manuelle Kammer"){
  Kammer<-as.data.frame(Kammer)

  Grundfl <- Kammer$Grundfl._cm2[Kammer$durchmesser_typ=="kragen_innen"]
  Grundfl_aussen <- Kammer$Grundfl._cm2[Kammer$durchmesser_typ=="kragen_aussen"]

  Vol_kammer<-Kammer$Gesamt_vol_cm3[1]
  Vol_kragen <- (kragen$height-Kammer$Hoehe_cm[1])*Grundfl
  Vol_kragen_rand <- kragen$height*Grundfl_aussen - kragen$height*Grundfl
  Vol <- Vol_kammer + Vol_GGA + ifelse(Vol_kragen > 0, Vol_kragen, 0) - Vol_kragen_rand
}else{
  #Vol in cm3
  Grundfl<-Kammer$Kragen_Grundfl_innen_cm2 #cm2
  Grundfl_aussen<-Kammer$Kragen_Grundfl_aussen_cm2

  Hoehe<-Kammer$Kragen_Hoehe_cm#cm
  Vol_kammer<-Kammer$Kammer_Volumen_cm3

  Vol_basis<-Grundfl*(Hoehe-Messungen$Tiefe-Messungen$Ueberstand)+Vol_kammer
  #Vol_schlauch<-20 #cm3
  Vol<-Vol_basis-Messungen$Ueberstand*(Grundfl_aussen-Grundfl)+Vol_schlauch+Vol_GGA#cm3
}

flux <- list()
for(i in c("CO2","CH4")){
  flux[[i]] <- calc_flux(data = dataset,
          group="kammer",
          gas = i,
          Vol = Vol,
          Grundfl = Grundfl,
          aggregate = aggregate)
  }

return(flux)
}





