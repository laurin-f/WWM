#' Function to change units of a vector
#'
#' @param x vector
#' @param unit_in unit of input vector as character
#' @param unit_out output unit as character
#' @param class_out class of output either \code{"numeric"} or \code{"units"}
#'
#' @return
#' @export
#'
#' @examples
change_unit <- function(x,
                        unit_in,
                        unit_out,
                        class_out="numeric"){
  x_unit_in <- units::set_units(x, unit_in, mode="standard")
  x_unit_out <- units::set_units(x_unit_in, unit_out, mode="standard")
  return(as(x_unit_out,class_out))
}
