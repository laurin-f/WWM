#' Compute RootMeanSquareError
#'
#' @param mod modelled data
#' @param obs observed data
#'
#' @return RMSE
#' @export
#'
#' @examples RMSE(CO2_mod,CO2_obs)
RMSE <- function(mod,obs){
  sqrt(mean((mod-obs)^2,na.rm = T))
}
