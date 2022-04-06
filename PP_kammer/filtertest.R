#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","svMisc")
check.packages(packages)

pp_filtertest <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/PP_filtertest/"
p_filt <- read.csv(paste0(pp_filtertest,"Pressure_filtered.csv"),header=F)
p <- read.csv(paste0(pp_filtertest,"Pressure.csv"),header=F)
test <- data.frame(1:nrow(p),p,p_filt)
colnames(test) <- c("x","p","p_filt")



fs <- 10#Hz
fpass <- c(0.003,0.1)
wpass <- fpass / (fs /2)



bpfilter <- gsignal::butter(n=3,w=wpass,type="pass")

test$r_filt <- gsignal::filtfilt(bpfilter,test$p)
range(test$r_filt,na.rm = T)
ggplot(test[5000:10000,])+
  geom_line(aes(x,p,col="p"))+
  geom_line(aes(x,p_filt,col="filt"))+
  geom_line(aes(x,r_filt,col="r_filt"))

mean(test$r_filt - test$p_filt)



stable <- gsignal::butter(4, c(0.1,0.3), "pass")


t <- seq(0, 1, len = 100)
x <- sin(2* pi * t * 2.3) + 0.5 * rnorm(length(t))
z1 <- gsignal::filtfilt(stable, x)

plot(t, x, type = "l", xlab = "", ylab = "")
lines(t, z1, col = "green", lwd = 2)

