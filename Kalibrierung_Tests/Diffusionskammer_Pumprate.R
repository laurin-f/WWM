


#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/Tracereinspeisung/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2")
check.packages(packages)

Vol.xlsx<-readxl::read_xlsx(paste0(metapfad,"Diffusionskammer.xlsx"))
Vol_ml<-Vol.xlsx$Volumen_effektiv_ml

datelim <- c("2020.03.11 09:00:00","2020.03.17 18:00:00")
#  datelim <- c("2020.03.11 08:00:00","2020.03.11 18:00:00")
#  datelim <- c("2020.03.12 08:00:00","2020.03.12 18:00:00")
# datelim <- c("2020.03.16 08:00:00","2020.03.16 18:00:00")
data <- read_db("dynament.db","dynament_test",datelim = datelim)

colnames(data) <- str_replace(colnames(data),"(?<=CO2).*","_raw")
data$CO2 <- data$CO2_raw
spikes <- which(abs(diff(data$CO2_raw)) > 500 & diff(as.numeric(data$date)) <= 10)
data$CO2[spikes[spikes %in% (spikes + 1)]] <- NA
data$CO2[spikes] <- NA


# ggplot(data)+
#   geom_point(aes(date,CO2_raw,col="raw"))+
#   geom_point(aes(date,CO2,col="korr"))#+
  #xlim(ymd_hms("2020.03.11 13:00:00","2020.03.17 14:00:00"))
  
#erster Zeitrahmen
split <- split_chamber(data,
                       closing_before = 20,
                       closing_after = 20,
                       opening_before = 0,
                       opening_after = 10,
                       t_max=6,
                       t_init = 1,
                       t_min=3,adj_openings = T)


#ggplot(split)+geom_point(aes(zeit,CO2,col=as.factor(messid)))+facet_wrap(~messid)

Pumpstufen <- c(1:5,5,rep(NA,4),1:4,1:5)
sampler_included <- c(rep(0,14),rep(1,5))
split$Pumpstufe <- as.numeric(as.character(factor(split$messid,levels = unique(split$messid),labels=Pumpstufen)))
split$sampler_included <- as.numeric(as.character(factor(split$messid,levels = unique(split$messid),labels=sampler_included)))

CO2.tara_list <- lapply(na.omit(unique(split$messid)), function(x){
  messid.x <- split$messid == x
  min.zeit <- min(split$zeit[messid.x],na.rm = T)
  split$CO2[which(messid.x)] - split$CO2[which(messid.x & split$zeit == min.zeit)]
})

split$CO2.tara <- NA
split$CO2.tara[!is.na(split$messid)] <- do.call(c,CO2.tara_list)

flux <- calc_flux(split,Vol=Vol_ml+100,tracer_conc = 100)
flux

label.group <- sort(unique(na.omit(Pumpstufen)))
label.char <- paste("Pumpstufe",label.group,"=",round(flux$tracer_ml_per_min,2),"ml/min")
lbl <- as_labeller(setNames(label.char, label.group))

Pumpstufen_plot <- ggplot(subset(split,!is.na(Pumpstufe)))+
  geom_line(aes(zeit,CO2.tara,linetype=as.factor(sampler_included),col=as.factor(messid)))+
  geom_smooth(aes(zeit,CO2.tara,col=as.factor(messid),linetype=as.factor(sampler_included)),method="glm")+
  facet_wrap(~Pumpstufe,labeller = lbl)
Pumpstufen_plot+guides(col=F)+labs(linetype="sampler included")


ggplot(subset(split,!is.na(Pumpstufe)))+
  geom_point(aes(zeit,CO2.tara,shape=as.factor(sampler_included),col=as.factor(Pumpstufe)))#+
  #geom_smooth(aes(zeit,CO2.tara,col=as.factor(Pumpstufe),linetype=as.factor(sampler_included)),method="glm")

ggplot(flux)+geom_point(aes(Pumpstufe,tracer_ml_per_min))+geom_abline(intercept=0,slope=0.1)

flux[nrow(flux)+1,]<-0
write.csv(flux,file = paste0(metapfad,"Pumpstufen_flux.txt"),row.names = F)

#########
#


