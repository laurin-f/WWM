#pfade definieren

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"

metapfad<-paste0(hauptpfad,"Daten/Metadaten/Kammermessungen/")

#flux.kammer<-function(ort="Schauinsland",
#                      messnr){
  
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","readxl","egg")
check.packages(packages)

ort<-"Schauinsland"
#Metadaten laden
if(ort=="Schauinsland"){
  schauimetapfad<-paste0(hauptpfad,"Daten/Metadaten/Schauinsland/")
  Messungen<-read_xlsx(paste0(schauimetapfad,"Kammermessungen.xlsx"))
  Kammer<-read_xlsx(paste0(metapfad,"Kammer_Meta.xlsx"),sheet=2)
  }else{
  Kammer<-read_xlsx(paste0(metapfad,"Kammer_Meta.xlsx"))
  Messungen<-read_xlsx(paste0(metapfad,"Messungen.xlsx"))
}

messnr<-1

beginn<-ymd_hm(paste(Messungen$Datum[messnr],format(Messungen$beginn[messnr],"%H:%M")))
ende<-ymd_hm(paste(Messungen$Datum[messnr],format(Messungen$ende[messnr],"%H:%M")))

data.raw<-read_db("GGA.db","micro",datelim=c(beginn,ende))



#dataset<-split.chamber(data=data.agg,closing=5,opening = -30,t_max=9)
dataset<-split_chamber(data=data.raw,closing_before = 0,
                       closing_after = 5,
                       opening_before = -10,
                       opening_after = -30,
                       t_max=5,
                       t_init = 1,
                       t_min = 5)


dataset$kammer<-NA
KammerIDs<-colnames(Messungen)[4:9]
for(i in 1:6){
  temp_kammer<-KammerIDs[i]
  temp_messid<-str_split(Messungen[messnr,4:9][i],"\\.|,")[[1]]
  dataset$kammer[dataset$messid %in% temp_messid] <- temp_kammer
}

ggplot(subset(dataset,!is.na(kammer)),aes(zeit,CO2))+geom_point()+geom_smooth(method="glm")+facet_wrap(~kammer)


data.list<-lapply(KammerIDs,function(x) subset(dataset,dataset$kammer==x))
names(data.list) <- KammerIDs

fm_CO2.list<-lapply(data.list,function(x) glm(CO2~zeit,data=x))
fm_CH4.list<-lapply(data.list,function(x) glm(CH4~zeit,data=x))

#fm_CO2<-glm(co2~zeit,data=dataset)
#fm_CH4<-glm(CH4~zeit,data=dataset)

#dataset$CO2.preds<-predict(fm_CO2,newdata = data.frame(CO2=dataset$CO2,zeit=dataset$zeit))
#dataset$CH4.preds<-predict(fm_CH4,newdata = data.frame(CH4=dataset$CH4,zeit=dataset$zeit))

dataset$CO2.preds<-NA
dataset$CH4.preds<-NA

for(i in KammerIDs){
  dataset$CO2.preds[which(dataset$kammer==i)] <- predict(fm_CO2.list[[i]],
                               newdata = data.frame(CO2=dataset$CO2[which(dataset$kammer==i)],
                                                    zeit=dataset$zeit[which(dataset$kammer==i)]))
  dataset$CH4.preds[which(dataset$kammer==i)] <- predict(fm_CH4.list[[i]],
                               newdata = data.frame(CH4=dataset$CH4[which(dataset$kammer==i)],
                                                    zeit=dataset$zeit[which(dataset$kammer==i)]))
  
}


#slope_CO2 <- fm_CO2$coefficients[2]/10^6#cm3/(cm3 min)
#slope_CH4 <- fm_CH4$coefficients[2]/10^6#cm3/(cm3 min)

slope_CO2 <- sapply(fm_CO2.list,function(x) x$coefficients[2]/10^6)#cm3/(cm3 min)
slope_CH4 <- sapply(fm_CH4.list, function(x) x$coefficients[2]/10^6)#cm3/(cm3 min)



if(ort=="Schauinsland"){
  Kammer<-as.data.frame(Kammer)
  
  Grundfl<-Kammer$Grundfl._cm2[Kammer$durchmesser_typ=="unten"]
  Vol<-Kammer$Gesamt_vol_cm3[1]
}else{
  #Vol in cm3
  Grundfl<-Kammer$Kragen_Grundfl_innen_cm2 #cm2
  Hoehe<-Kammer$Kragen_Hoehe_cm#cm
  Grundfl_aussen<-Kammer$Kragen_Grundfl_aussen_cm2
  Vol_kammer<-Kammer$Kammer_Volumen_cm3
  
  Vol_basis<-Grundfl*(Hoehe-Messungen$Tiefe-Messungen$Ueberstand)+Vol_kammer
  Vol_schlauch<-20 #cm3
  Vol_GGA<-0
  Vol<-Vol_basis-Messungen$Ueberstand*(Grundfl_aussen-Grundfl)+Vol_schlauch+Vol_GGA#cm3
}

p_Pa <- 101.3*1000#PA # N/m2 # kg/(m s2)
T_deg<-15 #°C

#Konstanten
R <- 8.314 #kg m2/(s2 mol K)
M_CO2 <- 44.01 #g/mol
M_CH4 <- 16.04 #g/mol 
T_K <- T_deg+273.15 #K
m_CO2<-p_Pa*M_CO2/(R*T_K)/10^6 #g/cm3
m_CH4<-p_Pa*M_CH4/(R*T_K)/10^6 #g/cm3

#berechnung Flux

flux_CO2_cm3<-slope_CO2*Vol/(Grundfl/10^4) #cm3 /(min m2)
flux_CO2_g<-flux_CO2_cm3*m_CO2 #g/(min m2)
flux_CO2_g*60 #g/(h m2)

flux_CH4_cm3<-slope_CH4*Vol/(Grundfl/10^4) #cm3 /(min m2)
flux_CH4_g<-flux_CH4_cm3*m_CH4 #g/(min m2)
flux_CH4_g*10^6*60 #mug/h m2


p<-ggplot(subset(dataset,!is.na(kammer)))+geom_point(aes(zeit,CO2))+geom_line(aes(zeit,CO2.preds))+
  facet_wrap(~kammer)

p+geom_text(data=data.frame(kammer=KammerIDs),aes(-Inf,Inf),
            label=paste(signif(flux_CO2_g,2),"g/(min m²)"),hjust=-0.25,vjust=4 )

egg::tag_facet(p, 
          x = -Inf, y = Inf, 
          vjust = 2, hjust = -0.25,
          open = "", close = "",
          tag_pool = paste(KammerIDs,"=",signif(flux_CO2_g,2),"g/(min m²)"))

p_ch4<-ggplot(subset(dataset,!is.na(kammer)))+geom_point(aes(zeit,CH4))+geom_line(aes(zeit,CH4.preds))+facet_wrap(~kammer)

p_ch4+geom_text(data=data.frame(kammer=KammerIDs),aes(-Inf,-Inf),
            label=paste(signif(flux_CH4_g,2),"g/(min m²)"),hjust=-0.25,vjust=-4 )

egg::tag_facet(p_ch4, 
          x = -Inf, y = -Inf, 
          vjust = -2, hjust = -0.1,
          open = "", close = "",
          tag_pool = paste(KammerIDs,"=",signif(flux_CH4_cm3,2),"cm³/(min m²)"))
###########################
#Vergleich mit flux package

## partition the data into data tables per chamber measurement
# then do the partitioning
test<-subset(dataset,!is.na(messid))
test$year<-2019
test$vol<-Vol/10^6
test$Grundfl<-Grundfl/10^4
test$t.air<-15
gcd.parts <- chop(test, factors = c("messid","year"), 
                  nmes = c("year","messid"))

## calculate flux rates for methane
# first define a global CH4 range limit
CH4.lim <- 30
# do the flux rate estimation (it will often be best to define
# var.par separately, note that p.air is given as a parameter)
vp.CO2 <- list(CO2 = "CO2", time = "zeit", 
               volume = "vol", t.air = "t.air", area = "Grundfl", p.air = 101325)
flux.CO2 <- flux(gcd.parts, var.par = vp.CO2)
# look at the results table
flux.CO2
mean(flux.CO2$flux.table$CO2.flux)
