datelim <- c(ymd_hm("2022-11-03 09:00"),now())
unzipped_path <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/GGA/unzipped/"
files <- list.files(unzipped_path,pattern = "gga_2022-11-\\d[3-90]_f000\\d.txt",full.names = T)
test <- read.csv(files[1],skip=1)
data_list <- lapply(files,read.csv,skip=1)
data <- do.call(rbind,data_list)
data$date <- parse_date_time(data$Time,"mdYHMS")

#data <- read_GGA(datelim = datelim,table.name = "gga")

t_diff <- which(abs(diff_time(data$date))>300)
#t_diff <- which(diff_time(data$date)<1)
plot(diff_time(data$date))
data$date[t_diff]

sub <- data[unique(sort(c(t_diff,t_diff - 1,t_diff + 1))),]

change_time <- c("22.11.06 02:00","22.11.10 09:39")
