
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
check.packages(c("ggplot2","lubridate","stringr","dplyr"))
datapfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Arduino/"

datelim <- ymd_h(c("2020_05-0101","2020_06-0201"))
data <- read_sampler(table.name = "sampler1u2","long",datelim)
data_wide <- read_sampler(table.name = "sampler3","wide")


colnames(data)

data_long <- tidyr::pivot_longer(data_wide,contains("tiefe"),names_pattern="(CO2|temp)_tiefe(\\d)_smp\\d)",names_to = c(".value","tiefe","sampler"))
data_long <- tidyr::pivot_wider(data_long,names_from = sampler, values_from = CO2, names_prefix = "CO2_")

data_long <- tidyr::pivot_longer(data_wide,contains("tiefe"),names_pattern="(CO2|temp)_tiefe(\\d)_smp(\\d)",names_to = c(".value","tiefe","sampler"))





ggplot(data)+
  geom_line(aes(date,CO2,col=as.factor(tiefe)))
ggplot(long)+
  geom_line(aes(date,temp,col=as.factor(tiefe)))
  
