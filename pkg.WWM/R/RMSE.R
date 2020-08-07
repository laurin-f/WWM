#' Compute RootMeanSquareError
#'
#' @param mod modelled data
#' @param obs observed data
#'
#' @return RMSE
#' @export
#'
#' @examples RMSE(CO2_mod,CO2_obs)
RMSE <- function(mod,obs,normalize=F){
  rmse <- sqrt(mean((mod-obs)^2,na.rm = T))
  if(normalize =="mean"){
    rmse <- rmse/mean(obs,na.rm = T)
  }
  if(normalize =="sd"){
    rmse <- rmse/sd(obs,na.rm = T)
  }
  return(rmse)
}
