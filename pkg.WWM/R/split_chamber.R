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
#' closing_before = 5,
#' closing_after = 10,
#' opening_before = -5,
#' opening_after = -15,
#' t_max=Inf,
#' t_init=0,
#' t_min=5)
split_chamber<-function(data,
                        closing_before = 0,
                        closing_after = 5,
                        opening_before = -10,
                        opening_after = -30,
                        t_max = 5,
                        t_init = 1,
                        t_min = 5,
                        gas="CO2",
                        adj_openings = T){

  data <- na.omit(data)

  hourminute<-round(data$date,unit="mins")
  #data$hourminute<-paste0(format(data$date,"%Y-%m-%d %H:%M"),":00")
  #data.agg<-aggregate(data[,-grep("hourminute", colnames(data))],list(hourminute = data$hourminute),mean)
  data.agg<-aggregate(data,list(hourminute = as.character(hourminute)),mean)
  data.agg$date<-ymd_hms(data.agg$hourminute)

  before<-c(NA,diff(data.agg[,gas]))
  after<-c(diff(data.agg[,gas]),NA)

  closing<-which(before < closing_before & after > closing_after)

  opening<-which(before > opening_before & after < opening_after)


  if(adj_openings == T){
    while(opening[1] <= closing[1]){
      opening <- opening[-1]
    }

    for(i in 2:length(closing)){
      if(is.na(opening[i-1])){
        opening[i-1]<-closing[i]-1
      }
      if(opening[i-1] > closing[i]){
        opening <- c(opening[0:(i-2)],closing[i]-1,opening[(i-1):length(opening)])
      }
      if(is.na(opening[i])){
        opening[i]<-nrow(data.agg)
      }
      while(opening[i] <= closing[i]){
        opening <- opening[-i]
        if(is.na(opening[i])){
          opening[i]<-nrow(data.agg)
        }
      }
    }
  }

  open_close <- length(opening) - length(closing)
  if(open_close < 0){
    opening <- c(opening,rep(nrow(data.agg),abs(open_close)))
  }else if(open_close > 0){
    opening <- opening[1:(length(opening)-open_close)]
  }

  diff_open_close <- (opening - closing) > t_min
  opening <- opening[diff_open_close]
  closing <- closing[diff_open_close]

  opening.time <- as.character(data.agg$date[opening])
  closing.time <- as.character(data.agg$date[closing])

  data <- data[order(data$date),]
  data$hourminute<-paste0(format(data$date,"%Y-%m-%d %H:%M"),":00")
  data$hourminute[duplicated(data$hourminute)]<-NA

  openingID <- which(data$hourminute %in% opening.time)
  closingID <- which(as.character(data$hourminute) %in% closing.time)


  data$zeit<-NA
  data$messid<-NA
  for(i in 1:length(openingID)){
    data$zeit[closingID[i]:openingID[i]] <-
      difftime(data$date[closingID[i]:openingID[i]],data$date[closingID[i]],unit="mins")
    data$messid[closingID[i]:openingID[i]] <- i
  }

  data$zeit[data$zeit > t_max | data$zeit < t_init] <- NA
  data$messid[is.na(data$zeit)] <- NA


  data.agg$change <- ""
  data.agg$change[opening]<-"opening"
  data.agg$change[closing]<-"closing"

  data.agg$messid <- NA
  data.agg$messid[opening]<-seq_along(opening)
  data.agg$messid[closing]<-seq_along(closing)


  messid_cols <-  scales::hue_pal()(max(data$messid,na.rm=T))[data$messid]
  plot(data.agg$date, data.agg[,gas], col = ifelse(data.agg$change == "",1,NA), pch=20)
  points(data$date,data[,gas], col = messid_cols)
  points(data.agg$date,data.agg[,gas], col = ifelse(data.agg$change == "",NA,
                                                  ifelse(data.agg$change == "opening",2,3)),
         pch=as.character(data.agg$messid))


  legend("topleft",c("opening","closing",unique(data$messid)),col = c(2:3,unique(messid_cols)),pch=20, bty = "n")


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
                      P_kPA = 101.3,
                      T_deg = 15,
                      tracer_conc=NULL){#percent
  formula <- paste(gas,"~ zeit")
  group_unique <- na.omit(unique(data[,group]))
  fm_list <- lapply(group_unique, function(x) glm(formula,data = data[which(data[,group] == x),]))

  ppm_per_min <- sapply(fm_list,"[[","coefficients")[2,]#ppm/min
  #Konstanten
  p_Pa <- P_kPA*1000#PA = N/m2 = kg/(m s2)
  T_K <- T_deg+273.15 #K
  R <- 8.314 #kg m2/(s2 mol K)
  M<-data.frame(CO2 = 44.01, CH4 = 16.04)#g/mol
  m_g_per_m3 <- p_Pa*M/(R*T_K) #kg/(m s2) * g/mol / kg m2 * (s2 mol K)/ K = g/m3

  m <- m_g_per_m3 / 10^6 #g/cm3 = g/ml

  #berechnung Flux
  flux <- data.frame(ppm_per_min)
  flux$ml_per_min<-ppm_per_min /10^6 * Vol #cm3 / min
  flux$g_per_min <- flux$ml_per_min * m[,gas] #g / min
  if(!is.null(Grundfl)){
    flux$ml_per_min_m2<-flux$cm2_per_min/(Grundfl/10^4) #cm3 /(min m2)
    flux$g_per_min_m2 <- flux$ml_per_min_m2*m[gas] #g/(min m2)
  }
  if(!is.null(tracer_conc)){
    flux$tracer_ml_per_min <- flux$ml_per_min * tracer_conc / 100
  }
  flux[group] <- group_unique
  return(flux)
}
