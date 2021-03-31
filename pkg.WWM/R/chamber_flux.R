#' function to calculate chamber flux
#'
#' @param mess_dir directory in which the metadata of the measurements is stored
#' @param messnr which measurement should be used from metadata. default is NULL = all measurements are used
#'
#' @return
#' @export
#'
#' @examples
chamber_flux <- function(mess_dir = "Hartheim",
                         messnr = NULL,
                         aggregate = F,
                         p_kPa = 101.3,
                         T_deg = NULL,
                         ...) {
  ########################
  #Metadaten laden
  #######################

  #in den Excelsheets habe ich dolumentiert von wann bis wann ich mit welchem GGA gemessen habe.
  Messungen <-
    readxl::read_xlsx(paste0(metapfad, mess_dir, "/Kammermessungen.xlsx"))
  #in sheet 2 habe ich abgelegt wie hoch die Kragen aus dem Boden ragen
  kragen <-
    readxl::read_xlsx(paste0(metapfad, mess_dir, "/Kammermessungen.xlsx"), sheet = 2)

  #wenn eine spezielle messnr ausgewählt wird werden die Metadaten dementsprechend zugeschnitten
  if (!is.null(messnr)) {
    Messungen <- Messungen[messnr, ]
  }

  #wurde der alte oder neue GGA benutzt
  GGA <- unique(Messungen$GGA)
  #welcher Kammertyp wurde benutzt
  chamber <- unique(Messungen$kammer)


  #im Kammer_Meta.xslsx habe ich das Kammervolumen abgeschätzt
  Kammer <-
    readxl::read_xlsx(paste0(metapfad, "Kammermessungen/Kammer_Meta.xlsx"),
                      sheet = chamber)
  Schlauch_meta <-
    readxl::read_xlsx(paste0(metapfad, "Kammermessungen/Kammer_Meta.xlsx"),
                      sheet = "schlauch_volumen")
  #Hier steht drin welches interne Volumen die GGAs haben
  GGA_Vol <-
    readxl::read_xlsx(paste0(metapfad, "GGA/Kammervolumen.xlsx"))


  kragen$height <- kragen$height_cm

  ###########################
  #Datum formatieren
  #anfang und ende der Messungen als date
  beginn_seq <-
    ymd_hm(paste(Messungen$Datum, format(Messungen$beginn, "%H:%M")))
  beginn <- min(beginn_seq)
  ende_seq <-
    ymd_hm(paste(Messungen$Datum, format(Messungen$ende, "%H:%M")))
  ende <- max(ende_seq)

  ###########################
  #Daten laden
  #daten aus Database einlesen
  data.raw <- read_db("GGA.db", GGA, datelim = c(beginn, ende))

  ########################
  #split chamber function um die Zeiträume der einzelmessungen zu bestimmen
  data <- split_chamber(data = data.raw,
                        ...)

  #Messungen der Richtigen Kammer zuordnen
  data$kammer <- NA
  #die Reihenfolge in der die Kammern gemessen wurden steht in den metadaten
  reihenfolge <- unlist(str_split(Messungen$reihenfolge, ","))

  for (i in seq_along(reihenfolge)) {
    data$kammer[data$messid == i] <- reihenfolge[i]
  }


  ############################
  #Volumen bestimmen
  ############################
  Vol_GGA <- GGA_Vol$Volume_effektive_cm3[GGA_Vol$Analyzer == GGA]
  #Grundfl_cm2
  if (chamber == "manuelle Kammer") {
    Kammer <- as.data.frame(Kammer)

    Grundfl <-
      Kammer$Grundfl._cm2[Kammer$durchmesser_typ == "kragen_innen"]
    Grundfl_aussen <-
      Kammer$Grundfl._cm2[Kammer$durchmesser_typ == "kragen_aussen"]

    schlauch_cols <- grep("schlauch", colnames(Messungen), value = T)
    Vol_schlauch_df <-
      Messungen[, schlauch_cols] * Schlauch_meta[rep(3, nrow(Messungen)), schlauch_cols]
    Vol_schlauch <- rowSums(Vol_schlauch_df) #cm3

    Vol_kammer <- Kammer$Gesamt_vol_cm3[1]
    Vol_kragen <- (kragen$height - Kammer$Hoehe_cm[1]) * Grundfl
    #das Volumen des Kragens selber (ausendurchmessen -innendurchmesser) wird vom Kammervolumen abgezogen
    Vol_kragen_rand <-
      Kammer$Hoehe_cm[1] * Grundfl_aussen - Kammer$Hoehe_cm[1] * Grundfl

    #Wenn Vol_kragen größer null ist heißt dass das unter der Kammer noch zusätzliches Volumen im Kragen ist, sonst ist es null
    Vol <-
      Vol_kammer + Vol_GGA + ifelse(Vol_kragen > 0, Vol_kragen, 0) - Vol_kragen_rand
    names(Vol) <- kragen$KragenID
  } else{
    #Vol in cm3
    Grundfl <- Kammer$Kragen_Grundfl_innen_cm2 #cm2
    Grundfl_aussen <- Kammer$Kragen_Grundfl_aussen_cm2

    Hoehe <- Kammer$Kragen_Hoehe_cm#cm
    Vol_kammer <- Kammer$Kammer_Volumen_cm3

    Vol_basis <-
      Grundfl * (Hoehe - Messungen$Tiefe - Messungen$Ueberstand) + Vol_kammer
    #Vol_schlauch<-20 #cm3
    Vol <-
      Vol_basis - Messungen$Ueberstand * (Grundfl_aussen - Grundfl) + Vol_schlauch +
      Vol_GGA#cm3
  }

  ###########
  #Temperatur
  ###########

  #in Hartheim sind zusätzliche Klimadaten verfügbar
  load(file = paste0(klimapfad, "klima_data.RData"))
  klima_sub <- subset(klima, date > beginn & date < ende)

  #Liste für Output anlegen
  flux <- list()

  #Wenn mehrere unteschiedliche SchlauchVolumen verwendet wurden wird eine Schleife angeschmissen
  if (length(unique(Vol_schlauch)) > 1 ) {
    for (i in c("CO2", "CH4")) {
      flux[[i]] <- list(NULL, NULL)
      for (j in seq_along(Vol_schlauch)) {
        #falls klimadaten Vorhanden diese verwenden da interne Temperatur vom microGGA zu hoch ist (siehe Temp_micro_klimaturm.R)
        if (any(klima_sub$date > beginn_seq[j] &
                klima_sub$date < ende_seq[j])) {
          T_deg <- mean(klima_sub$Ta_2m[klima_sub$date > beginn_seq[j] &
                                          klima_sub$date < ende_seq[j]], na.rm =
                          T)
        } else{
          T_deg <- mean(data$AmbT_C[data$date > beginn_seq[j] &
                                      data$date < ende_seq[j]], na.rm = T)
        }

        #Fluss mit calc_flux berechnen
        flux_j <-
          calc_flux(
            data = subset(data, date > beginn_seq[j] & date < ende_seq[j]),
            group = "kammer",
            gas = i,
            Vol = Vol + Vol_schlauch[j],
            Grundfl = Grundfl,
            aggregate = aggregate,
            T_deg = T_deg,
            p_kPa = p_kPa
          )
        flux[[i]][[1]] <- rbind(flux[[i]][[1]], flux_j[[1]])
        flux[[i]][[2]] <- rbind(flux[[i]][[2]], flux_j[[2]])

      }
    }
  } else{
    #Wenn alle Schlauchlaengen gleich sind
    for (i in c("CO2", "CH4")) {
      if (any(klima_sub$date > beginn_seq & klima_sub$date < ende_seq)) {
        T_deg <- mean(klima_sub$Ta_2m[klima_sub$date > beginn_seq &
                                        klima_sub$date < ende_seq], na.rm = T)
      } else{
        T_deg <- mean(data$AmbT_C[data$date > beginn_seq &
                                    data$date < ende_seq], na.rm = T)
      }
      flux[[i]] <- calc_flux(
        data = data,
        group = "kammer",
        gas = i,
        Vol = Vol + unique(Vol_schlauch),
        Grundfl = Grundfl,
        aggregate = aggregate,
        T_deg = T_deg,
        p_kPa = p_kPa
      )
    }
  }
  return(flux)
}
