
#' Title
#'
#' @param data
#' @param unit_in
#' @param unit_out
#' @param p_kPa
#' @param T_C
#' @importFrom units set_units
#' @return
#' @export
#'
#' @examples
ppm_to_mol <- function(data,
                       unit_in="ppm",
                       out_class="numeric",
                       p_kPa = 101.3,
                       T_C = 20){

  data_in <- set_units(data,unit_in,mode="standard")
  p_kPa <- set_units(p_kPa,kPa)
  T_C <- set_units(T_C,"°C")
  T_K <- set_units(T_C,K)
  p <- set_units(p_kPa,kg/(m*s^2))
  R <- set_units(8.314, kg*m^2/(s^2*mol*K))
  mol_per_m3 <- p/(R*T_K)
  mol_per_m3

  if(unit_in == "ppm"){
    data_out <- data_in * mol_per_m3 / set_units(10^6,ppm)
  }else if(unit_in == "mol/m^3"){
    data_out <- data_in / mol_per_m3 *set_units(10^6,ppm)
  }
  return(as(data_out,out_class))
}

