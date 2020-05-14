hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")



#' Function to calculate Injectionrate for specific Diffusionchamber measurement
#'
#' @param datelim time intervall
#' @param Pumpstufen verwendete Pumpstufen bei den unterschiedlichen Versuchen
#' @param group name of the column that should be used for grouping results
#' @param ... #other parameters parsed to calc_flux
#'
#' @return
#' @export
#'
#' @examples split <- injectionrate(datelim = datelim,Pumpstufen = c(1:5,5,rep(NA,4),1:4,1:5),group="Pumpstufe",
#' closing_before = 20,
#' closing_after = 20,
#' opening_before = 0,
#' opening_after = 10,
#' t_max=6,
#' t_init = 1,
#' t_min=3)
injectionrate <- function(datelim,
                          Pumpstufen,
                          group = "Pumpstufe",
                          closing_th = 100,
                          opening_th = 0,
                          t_max=6,
                          t_init = 2,
                          t_min=3,
                          T_C = 15,
                          ...){

  ########################
  #Daten einlesen
  ########################
  #Metadaten aus Kammer laden

  Vol.xlsx<-readxl::read_xlsx(paste0(metapfad,"Diffusionskammer.xlsx"))
  Vol_ml<-Vol.xlsx$Volumen_effektiv_ml

  #CO2 daten einlesen
  data <- read_db("dynament.db","dynament_test",datelim = datelim)

  #Spaltenname anpassen
  colnames(data) <- str_replace(colnames(data),"(?<=CO2).*","_raw")
  data$CO2 <- data$CO2_raw
  data$CO2[which(data$CO2 < 0)] <- NA
  data <- na.omit(data)
  ##########################
  #postprocessing
  #############################
  #spikes entfernen
  spikes <- which(abs(diff(data$CO2_raw)) > 500 & diff(as.numeric(data$date)) <= 10)
  data$CO2[spikes[spikes %in% (spikes + 1)]] <- NA
  data$CO2[spikes] <- NA

  #split_chmaber anwenden
  split <- split_chamber(data,
                         closing_th = closing_th,
                         opening_th = opening_th,
                         t_max=t_max,
                         t_init = t_init,
                         t_min=t_min,
                         adj_openings = T)

  #Pumpstufen den messid's zuordnen
  split$Pumpstufe <- as.numeric(as.character(factor(split$messid,levels = unique(split$messid),labels=Pumpstufen)))



  #Fluss mit calc_flux bestimmen
  flux <- calc_flux(data = split,
                    Vol = Vol_ml,
                    tracer_conc = 100,
                    group=group,
                    T_deg = T_C,
                    ...)

  #
  return(flux)
}
