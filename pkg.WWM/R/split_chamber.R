#' Function to automatically identify closing and opening times of chamber measurents
#'
#'
#' @param data data.frame with gas concentrations of several chamber measurements
#' @param closing_before maximal concentration-gradient before closing the chamber
#' @param closing_after minimal concentration-gradient after closing the chamber
#' @param opening_before minimal concentration-gradient before opening the chamber
#' @param opening_after maximal concentration-gradient after opening the chamber
#' @param t_max maximal measurement time, if time between closing and opening
#' exceeds this time the rest of the measurement will not be included for the calculation
#' of the flux
#' @param t_init initial time after closing of the chamber that will be excluded from the calculations
#' @param t_min minimum timespan for each chamber measurement in minutes
#' @param gas name of the gas as character
#' @param adj_openings logical; if \code{TRUE} opening times will be adjusted
#' to never be smaller than closing times
#'
#' @return input data.frame with colums messid (number of the measurement) and zeit (time in minutes after closing) added
#' @export
#' @import lubridate
#'
#' @examples
#' test <- split.chamber(data,
#' closing_th = 40,
#' opening_th = -40,
#' t_max=Inf,
#' t_init=0,
#' t_min=5)
split_chamber<-function(data,
                        closing_before = 40,
                        closing_after = 40,
                        opening_after = -10,
                        opening_before = -10,
                        # closing_th = 40,
                        # opening_th = -40,
                        t_max = 3,
                        t_init = 0.1,
                        t_min = 2,
                        gas="CO2",
                        adj_openings = T){

  ##############################
  #datensatz aggregieren

  #remove na rows of gas and date
  data <- data[!is.na(data[,gas]) & !is.na(data$date),]

  #spalte mit minutenwerten
  hourminute<-round_date(data$date,unit="mins")

  #nach minutenwerten aggregieren
  data.agg<-aggregate(data,list(hourminute = as.character(hourminute)),mean)
  #date formatieren
  data.agg$date<-ymd_hms(data.agg$hourminute)

  #########################################################
  #am aggregierten Datensatz kammermessungen identifizieren

  #differenz der Gaswerte before und after sind identisch nur um eins verschoben
  before<-c(NA,diff(data.agg[,gas]))
  after<-c(diff(data.agg[,gas]),NA)
  #Zeitdifferenz in Minuten before und after sind identisch nur um eins verschoben
  timediff_before <- c(NA,as.numeric(diff(data.agg$date)))
  timediff_after <- c(as.numeric(diff(data.agg$date)),NA)

  #Alle Punkte an denen voriger und folgender Wert eine Minute abstand haben
  #timediff_1 <- timediff_before <= 2 & timediff_after <= 2

  #Punkte an denen die Schwellenwerte für closing bzw. opening vorliegen
  # change <- after - before
  #
  # closing<-which(change / timediff_before / timediff_after > closing_th)
  # opening<-which(change / timediff_before / timediff_after < opening_th)

  closing<-which(before / timediff_before < closing_before &
                   after / timediff_after > closing_after)
  opening<-which(before / timediff_before > opening_before &
                   after / timediff_after < opening_after)

  #closing<-which(before < closing_before & after > closing_after & timediff_1)
  #opening<-which(before > opening_before & after < opening_after & timediff_1)

  ###################################################################
  #adjust openings
  #adj openings bedeutet opening wird so umgeschrieben das immer closing und opening im Wechsel vorkommen
  if(adj_openings == T){
    #solange der erste wert bei opening kleiner ist als bei closing
    #wird solange der erste opening-wert gelöscht bis dies nicht mehr der fall ist
    while(opening[1] <= closing[1]){
      opening <- opening[-1]
    }

    #alle weiteren Werte von closing werden iterativ getestet
    for(i in 2:length(closing)){
      #wenn opening[i-1] na ist wird an dieser stelle closing[i] -1 eingesetzt
      if(is.na(opening[i-1])){
        opening[i-1]<-closing[i]-1
      }
      #wenn opening[i-1] größer ist als closing[i] wird zwischen opening[i-2] und
      #opening[i-1] closing[i]-1 eingefügt
      if(opening[i-1] > closing[i]){
        opening <- c(opening[0:(i-2)],closing[i]-1,opening[(i-1):length(opening)])
      }
      #wenn opening[i] NA ist wird an diese stelle nrow(data.agg) geschrieben
      if(is.na(opening[i])){
        opening[i]<-nrow(data.agg)
      }
      #solange opening[i] kleiner gleich closing[i] ist wird opening[i] entfernt
      #wenn kein opening[i] mehr da ist wird nrow(data.agg) eingefügt
      while(opening[i] <= closing[i]){
        opening <- opening[-i]
        if(is.na(opening[i])){
          opening[i]<-nrow(data.agg)
        }#ende if
      }#ende while
    }#ende for
  }#ende adj_openings

  #differenz der längen opening und closing
  open_close <- length(opening) - length(closing)
  #wenn closing länger ist wird am ende von opening nrow(data.agg) angehängt
  if(open_close < 0){
    opening <- c(opening,rep(nrow(data.agg),abs(open_close)))
    #wenn opening länger ist wird das ende von opening abgeschnitten
  }else if(open_close > 0){
    opening <- opening[1:(length(opening)-open_close)]
  }

  #nur die closing opening perioden die mindestens
  #t_min minutenwerte enthalten wählen
  diff_open_close <- (opening - closing) > t_min
  opening <- opening[diff_open_close]
  closing <- closing[diff_open_close]

  #################################################################
  #Kammermesszeiträume vom aggregierten auf nicht aggregierten
  #Datensatz übertragen

  #zeitpunkte von closing und opening als character
  closing.time <- data.agg$hourminute[closing]
  opening.time <- data.agg$hourminute[opening]

  #data nach datum sortieren und hourminute aus date ausschneiden
  data <- data[order(data$date),]
  data$hourminute<-paste0(format(data$date,"%Y-%m-%d %H:%M"),":00")
  #duplicate von hourminute entfernen
  #sodass immer nur der erste Werte pro minute bleibt
  data$hourminute[duplicated(data$hourminute)]<-NA

  #index von closing und opening des nicht aggregierten data.frames
  closingID <- which(data$hourminute %in% closing.time)
  openingID <- which(data$hourminute %in% opening.time)

  #zeit und messid an data anfügen
  data$zeit<-NA
  data$messid<-NA
  for(i in 1:length(openingID)){
    #zeit in minuten nach closing
    data$zeit[closingID[i]:openingID[i]] <-
      difftime(data$date[closingID[i]:openingID[i]],data$date[closingID[i]],unit="mins")
    #messid als durchlaufende Nummer für jede closing opening periode
    data$messid[closingID[i]:openingID[i]] <- i
  }

  #zeiträume zuschneiden um nur werte zwischen t_init und t_max zu haben
  data$zeit[data$zeit > t_max | data$zeit < t_init] <- NA
  #diese Zeiträume auch bei messid mit NA überschreiben
  data$messid[is.na(data$zeit)] <- NA

  ##################################################
  #plot um ergebnis zu teste

  #spalte mit opening und closing punkten
  data.agg$change <- ""
  data.agg$change[opening]<-"opening"
  data.agg$change[closing]<-"closing"

  #messidspalte
  data.agg$messid <- NA
  data.agg$messid[opening]<-seq_along(opening)
  data.agg$messid[closing]<-seq_along(closing)

  #Farben für plot
  #kein ggplot da funktion dann schneller ist
  messid_cols <-  scales::hue_pal()(max(data$messid,na.rm=T))[data$messid]

  #plot
  par(mfrow = c(2,1),mar=c(1,3,1,1))
  plot(data.agg$date, data.agg[,gas], col = ifelse(data.agg$change == "",1,NA), pch=20,xlab="")
  points(data$date,data[,gas], col = messid_cols)
  points(data.agg$date,data.agg[,gas], col = ifelse(data.agg$change == "",NA,
                                                  ifelse(data.agg$change == "opening",2,3)),
         pch=as.character(data.agg$messid))

  legend("topleft",c("opening","closing",unique(data$messid)),col = c(2:3,unique(messid_cols)),pch=20, bty = "n")

  plot(before,xlab="")
  abline(h=closing_before,col=3,lty=2)
  abline(h=closing_after,col=3)
  abline(h=opening_before,col=2,lty=2)
  abline(h=opening_after,col=2)
  abline(v=closing,col=3)
  abline(v=opening,col=2)
  points(after,pch=3,col=4)

  legend("bottomleft",c("before","after","opening","closing"),col = c(1,4,3,2),pch=c(1,3,NA,NA),lty=c(NA,NA,1,1), bty = "n")
  par(mfrow = c(1,1))

  return(data)
}

##########################
#
#' Function to calculate flux from chamber measurements
#'
#' @param data dataset with gas concentration measurements over time
#' @param group name of the column that is used for grouping measurements
#' @param gas name of the gas
#' @param Vol chamber Volum in ml (cm3)
#' @param Grundfl area in m2
#' @param P_kPA pressure in kPa
#' @param T_deg Temperature in °C
#' @param tracer_conc tracer gas concentration in volum percent (if a tracer is used)
#'
#' @return data.frame with flux in different units for each group
#' @export
#'
#' @examples calc_flux(split,Vol=Vol_ml,tracer_conc = 100)
calc_flux <- function(data,
                      group = "Pumpstufe",
                      gas = "CO2",
                      Vol,#in cm3
                      Grundfl=NULL,#in cm2
                      p_kPa = 101.3,
                      T_deg = 15,
                      aggregate = F,
                      tracer_conc=NULL){#percent

  #CO2_tara als CO2-anstieg von Nullpunkt
  #eine liste mit den gas.tara werten
  gas.tara_list <- lapply(na.omit(unique(data$messid)), function(x){
    #indizes der reihen mit messid == x
    messid.x <- data$messid == x
    #anfangszeitpunkt der messid
    min.zeit <- min(data$zeit[messid.x],na.rm = T)
    #jeden gas wert der messid minus den gas wert zu zeit == min.zeit
    data[,gas][which(messid.x)] - data[,gas][which(messid.x & data$zeit == min.zeit)]
  })

  #Spalte gas_tara anhängen
  data[,paste0(gas, "_tara")] <- NA
  #und befülen mit den tara werten
  data[,paste0(gas, "_tara")][!is.na(data$messid)] <- do.call(c,gas.tara_list)

  #Formel für glm
  formula <- paste0(gas,"_tara ~ zeit")
  #vektor mit allen werten die in der spalte "group" vorkommen

  #spalte mit group und messid zusammen
  group_messid <- paste0(group,"_messid")
  data[,group_messid] <- paste0(data[,group],"_",data$messid)

  #NAs aus dem character zu echten NAs umwandeln
  NA_NA <- grep("NA", data[,group_messid])
  data[NA_NA,group_messid] <- NA

  #unique Werte
  group_messid_unique <- na.omit(unique(data[,group_messid]))
  #die group und messid wieder außeinanderschneiden und als numeric
  gr_id_ch <- str_split(group_messid_unique,"_",simplify = T)

  gr_id <- gr_id_ch

  #für jeden werte von group wird eine regression zwische gas und zeit durchgeführt
  fm_list <- lapply(1:nrow(gr_id), function(x) glm(formula,data = data[which(data[,group] == gr_id[x,1] & data$messid == gr_id[x,2]),]))
  #mittelwerte des Datums der unterschiedlichen gruppen
  
  ###################anschauen !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  date_means <- sapply(1:nrow(gr_id), function(x) mean(data[which(data[,group] == gr_id[x,1] & data$messid == gr_id[x,2]),"date"]))

  #aus der fm_liste wird jeweils der zweite coeffizient (steigung) ausgeschnitten
  ppm_per_min <- sapply(fm_list,"[[","coefficients")[2,]#ppm/min

  #Konstanten um Einheiten umzurechnen
  #Luftdruck
  p_Pa <- p_kPa*1000#PA = N/m2 = kg/(m s2)
  #Temperatur
  T_K <- T_deg+273.15 #K
  #allgemeine Gaskonstante
  R <- 8.314 #kg m2/(s2 mol K)
  #Molare Masse CO2 und CH4
  M<-data.frame(CO2 = 44.01, CH4 = 16.04)#g/mol

  #dichte
  ro_g_per_m3 <- p_Pa*M/(R*T_K) #kg/(m s2) * g/mol / kg m2 * (s2 mol K)/ K = g/m3
  ro<- ro_g_per_m3 / 10^6 #g/cm3 = g/ml

  #berechnung Flux in unterschiedlichen Einheiten
  flux <- data.frame(ppm_per_min)
  flux$ml_per_min<-ppm_per_min /10^6 * Vol #cm3 / min
  flux$g_per_min <- flux$ml_per_min * ro[,gas] #g / min
  if(!is.null(Grundfl)){
    flux$ml_per_min_m2<-flux$ml_per_min/(Grundfl/10^4) #cm3 /(min m2)
    flux$g_per_min_m2 <- flux$g_per_min/(Grundfl/10^4) #g/(min m2)
  }
  #falls eine Tracerkonzentration angegeben wurde wird
  #eine effektive Einspeiserate berechnet
  if(!is.null(tracer_conc)){
    flux$tracer_ml_per_min <- flux$ml_per_min * tracer_conc / 100
  }
  #group spalte an flux anfügen

  flux$messid <- as.numeric(gr_id[,2])
  #####################anschauen!!!!!!!!!!!!!!!!!!!!!
  flux$date <- lubridate::as_datetime(date_means)
  if(aggregate == T){
  flux <- aggregate(flux,list("group" = gr_id[,1]),mean)

  if(group != "messid"){
    flux <- flux[,!grepl("messid",colnames(flux))]
  }
  colnames(flux) <- str_replace(colnames(flux),"group",group)
  }else{
    flux[,group] <- gr_id[,1]
  }
  return(list(flux,data))
}



