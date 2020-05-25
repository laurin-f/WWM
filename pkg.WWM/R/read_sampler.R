#' Read sampler data from database
#'
#'
#' @param table.name name of the table in the database
#' @param format long or wide
#' @param ... other parameters parsed to read_db such as datelim or cols
#' (see help of read_db)
#'
#' @return
#' @export
#' @import stringr
#' @importFrom reshape2 melt
#' @examples datelim<-c("2020.02.19 12:05:00","2020.02.20 09:35:00")
#' data <- read_sampler("sampler1",datelim = datelim)

read_sampler <- function(table.name="sampler1",format="long", ...){

  data_wide<-read_db(db.name="dynament.db",
                     table.name=table.name, ...)

  if(format=="long"){

    data_long <- reshape2::melt(data_wide,id=which(!grepl("CO2",colnames(data_wide))),value.name="CO2")
    data_long$tiefenstufe <- as.numeric(str_extract(data_long$variable,"\\d$"))
    data_long$tiefe <- data_long$tiefenstufe * -3.5

    return(data_long)
  }else{
    return(data_wide)
  }
}
