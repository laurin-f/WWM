RMSE <- function(mod,obs){
  sqrt(mean((mod-obs)^2,na.rm = T))
}
