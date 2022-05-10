pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Profileprobe3_Arduino"
file <- "000101.TXT"
data <- read.csv(file.path(pfad,file),sep=";",na.strings = c("NA","ovf"))
sapply(data,class)

data$id <- 1:nrow(data)
data_long <- tidyr::pivot_longer(data,matches("CO2|temp"),names_pattern = "(.+)_tiefe(\\d)",names_to = c(".value","tiefe"))
data_long$CO2[data_long$CO2 <= 0] <- NA
head(data_long)

ggplot(data_long)+geom_line(aes(id,CO2,col=tiefe))
