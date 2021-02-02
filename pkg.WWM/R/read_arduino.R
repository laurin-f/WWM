read_arduino <- function(files) {
  data_list <- lapply(files,read.csv,sep=";",stringsAsFactors = F,na.strings = c("NA","ovf","0.00","-250.00"))
  data <- do.call(rbind,data_list)

  data$date <- ymd_hms(data$date)

}
