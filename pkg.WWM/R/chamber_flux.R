
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
chamber_flux <- function(mess_dir="Hartheim",
                         messnr=NULL,
                         aggregate = F,
                         ...){


  Messungen<-readxl::read_xlsx(paste0(metapfad,mess_dir,"/Kammermessungen.xlsx"))
  kragen <- readxl::read_xlsx(paste0(metapfad,mess_dir,"/Kammermessungen.xlsx"),sheet = 2)

  if(!is.null(messnr)){
    Messungen <- Messungen[messnr,]
  }

  GGA <- unique(Messungen$GGA)
  chamber <- unique(Messungen$kammer)


  Kammer<-readxl::read_xlsx(paste0(metapfad,"Kammermessungen/Kammer_Meta.xlsx"),sheet=chamber)
  Schlauch_meta <- readxl::read_xlsx(paste0(metapfad,"Kammermessungen/Kammer_Meta.xlsx"),sheet="schlauch_volumen")

  GGA_Vol <- readxl::read_xlsx(paste0(metapfad,"GGA/Kammervolumen.xlsx"))


  kragen$height <- apply(str_split(kragen$height_cm,",",simplify = T),1,function(x) mean(as.numeric(x),na.rm=T))


beginn_seq<-ymd_hm(paste(Messungen$Datum,format(Messungen$beginn,"%H:%M")))
beginn<-min(beginn_seq)
ende_seq<-ymd_hm(paste(Messungen$Datum,format(Messungen$ende,"%H:%M")))
ende<-max(ende_seq)

data.raw<-read_db("GGA.db",GGA,datelim=c(beginn,ende))

########################
#split chamber
data<-split_chamber(data=data.raw,
                       ...)

data$kammer<-NA

reihenfolge <- unlist(str_split(Messungen$reihenfolge,","))


for(i in seq_along(reihenfolge)){
  data$kammer[data$messid == i] <- reihenfolge[i]
}

Vol_GGA <- GGA_Vol$Volume_effektive_cm3[GGA_Vol$Analyzer==GGA]
#Grundfl_cm2
if(chamber=="manuelle Kammer"){
  Kammer<-as.data.frame(Kammer)

  Grundfl <- Kammer$Grundfl._cm2[Kammer$durchmesser_typ=="kragen_innen"]
  Grundfl_aussen <- Kammer$Grundfl._cm2[Kammer$durchmesser_typ=="kragen_aussen"]

  schlauch_cols <- grep("schlauch",colnames(Messungen),value=T)
  Vol_schlauch_df <- Messungen[,schlauch_cols]*Schlauch_meta[rep(3,nrow(Messungen)),schlauch_cols]
  Vol_schlauch <- rowSums(Vol_schlauch_df) #cm3

  Vol_kammer<-Kammer$Gesamt_vol_cm3[1]
  Vol_kragen <- (kragen$height-Kammer$Hoehe_cm[1])*Grundfl
  Vol_kragen_rand <- kragen$height*Grundfl_aussen - kragen$height*Grundfl
  Vol <- Vol_kammer + Vol_GGA + ifelse(Vol_kragen > 0, Vol_kragen, 0) - Vol_kragen_rand
  names(Vol) <- kragen$KragenID
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
if(length(Vol_schlauch) > 1){
  for(i in c("CO2","CH4")){
    flux[[i]] <- list(NULL,NULL)
    for(j in seq_along(Vol_schlauch)){
    flux_j <- calc_flux(data = subset(data,date > beginn_seq[j] & date < ende_seq[j]),
                           group="kammer",
                           gas = i,
                           Vol = Vol+Vol_schlauch[j],
                           Grundfl = Grundfl,
                           aggregate = aggregate)
    flux[[i]][[1]] <- rbind(flux[[i]][[1]],flux_j[[1]])
    flux[[i]][[2]] <- rbind(flux[[i]][[2]],flux_j[[2]])

    }
  }
}else{

for(i in c("CO2","CH4")){
  flux[[i]] <- calc_flux(data = data,
          group="kammer",
          gas = i,
          Vol = Vol+Vol_schlauch,
          Grundfl = Grundfl,
          aggregate = aggregate)
  }
}
return(flux)
}

