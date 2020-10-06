
###################################
#dateien für COMSOL exportieren####

#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/COMSOL/")
COMSOL_exepath <- "C:/Program Files/COMSOL/COMSOL52a/Multiphysics/bin/win64/"
COMSOL_progammpath <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Programme/Fremdprogramme/COMSOL/"
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","ggforce","dplyr")

check.packages(packages)

load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))

for(i in 1:nrow(Pumpzeiten)){
  data[data$date > (round_date(Pumpzeiten$start,"hours")[i]-3600) & data$date < (round_date(Pumpzeiten$start,"hours")[i]+10*3600),"CO2_tracer_gam"]<-NA
}

data$Versuch <- ifelse(data$Position == 7,ifelse(data$date < ymd_h("2020.07.10 00"),"1","2"),"3")
data <- subset(data,Position%in%7:8 & Pumpstufe!=0)
data$CO2_tracer_gam[data$CO2_tracer_gam<0] <- 0
data$CO2_tracer_gam[data$tiefe == 0] <- 0
data$signif <- F
data_list <- list()
agg_steps <- c(1,2,6,12,24)

# for(i in agg_steps){
#   data[,paste0("date_",i,"hours")] <- round_date(data$date,paste(i,"hours"))
#
#   data_list[[paste0("date_",i,"hours")]] <- data %>% group_by_at(c(date=paste0("date_",i,"hours"),tiefe="tiefe")) %>% summarise(CO2_tracer_gam = mean(CO2_tracer_gam,na.rm=T),signif=F)
# }


agg_steps <- c(1,2,6,12,24)
for(i in agg_steps){
  data[,paste0("CO2_tracer_",i,"hours")] <- NA
  for(j in 1:7){
    for(k in 1:3){
    data[data$tiefenstufe==j & data$Versuch == k,paste0("CO2_tracer_",i,"hours")] <- zoo::rollapply(data[data$tiefenstufe==j & data$Versuch == k,"CO2_tracer_gam"],width=i*60,mean,na.rm=T,fill=NA)
    }
  }
  print(i)
}

data_wide <- tidyr::pivot_wider(subset(data,tiefe!=0),id_cols=c(date,Versuch),names_from = tiefenstufe,values_from = matches("CO2_tracer(_gam|_\\d+hours)"))

for(i in agg_steps){
  col_ids <- grep(paste0("CO2_tracer_",i,"hours"),names(data_wide))
  col_order <- str_extract(names(data_wide)[col_ids],"\\d$") %>% as.numeric() %>% order()
  conc_order <- t(apply(data_wide[,col_ids],1,order))

  data_wide[,paste0("steady_gradient_",i,"hours")] <- apply(conc_order,1,function(x) all(x == col_order))
}

steady_df <- data_wide %>% group_by(Versuch) %>% summarise_at(vars(matches("steady_gradient|CO2_tracer_\\d+hours_1")),function(x) if(class(x)=="numeric") length(which(!is.na(x))) else length(which(x)))
ggplot(steady_df)+
  geom_point(aes(Versuch,steady_gradient_1hours/CO2_tracer_1hours_1,col="1"))+
  geom_point(aes(Versuch,steady_gradient_2hours/CO2_tracer_2hours_1,col="2"))+
  geom_point(aes(Versuch,steady_gradient_6hours/CO2_tracer_6hours_1,col="6"))+
  geom_point(aes(Versuch,steady_gradient_12hours/CO2_tracer_12hours_1,col="12"))+
  geom_point(aes(Versuch,steady_gradient_24hours/CO2_tracer_24hours_1,col="24"))+
  geom_hline(yintercept = 0.95)+
  annotate("text",x="1",y=0.98,label="95 % of data")+
  labs(y="Anteil an Datenpunkten mit stetigem CO2 Tracer Profil",col="mov_avg [h]")+
  ggsave(paste0(plotpfad,"mov_avg_tests_Anteil.png"),width=7,height=7)
ggplot(data_wide)+
  geom_point(aes(date,CO2_tracer_24hours_1,col=steady_gradient_6hours))+
  geom_point(aes(date,CO2_tracer_24hours_2,col=steady_gradient_6hours))+
  geom_point(aes(date,CO2_tracer_24hours_3,col=steady_gradient_6hours))#+
  geom_point(aes(date,CO2_tracer_6hours_depth2,col=steady_gradient6hours))+
  geom_point(aes(date,CO2_tracer_6hours_depth3,col=steady_gradient6hours))+
  geom_point(aes(date,CO2_tracer_6hours_depth4,col=steady_gradient6hours))+
  geom_point(aes(date,CO2_tracer_6hours_depth5,col=steady_gradient6hours))+
  geom_point(aes(date,CO2_tracer_6hours_depth6,col=steady_gradient6hours))+
  geom_point(aes(date,CO2_tracer_6hours_depth7,col=steady_gradient6hours))
ggplot(data_wide)+geom_line(aes(date,CO2_tracer_24hours_depth3,col=steady_gradient24hours))

twohours <- ggplot(subset(data,tiefe > -11))+geom_line(aes(date,CO2_tracer_2hours,col=as.factor(tiefe)))
sixhours <- ggplot(subset(data,tiefe > -11))+geom_line(aes(date,CO2_tracer_6hours,col=as.factor(tiefe)))
twelvehours <- ggplot(subset(data,tiefe > -11))+geom_line(aes(date,CO2_tracer_12hours,col=as.factor(tiefe)))
oneday <- ggplot(subset(data,tiefe > -11))+geom_line(aes(date,CO2_tracer_24hours,col=as.factor(tiefe)))
ggpubr::ggarrange(twohours,sixhours,twelvehours,oneday,common.legend = T)+ggsave(paste0(plotpfad,"mov_avg_tests.png"),width=7,height=7)

for(j in seq_along(data_list)){
for(i in unique(data_list[[j]]$date)){
  sub <- subset(data_list[[j]],date == i & tiefe %in% (1:3*-3.5) & !is.na(CO2_tracer_gam))
  if(nrow(sub)>2){
  fm <- glm(CO2_tracer_gam~tiefe,data=sub)
  #plot(y=sub$CO2_tracer_gam,x=sub$tiefe)
  #abline(fm)
  pval <- coef(summary(fm))[2,"Pr(>|t|)"]
  slope <- coef(fm)[2]
  data_list[[j]]$signif[data_list[[j]]$date == i] <- ifelse(pval < 0.5 & slope < 0,T,F)
  data_list[[j]]$cor[data_list[[j]]$date == i] <- cor(sub$CO2_tracer_gam,sub$tiefe)
  }
  date_i <- unique(data_list[[j]]$date[data_list[[j]]$date==i])
  if(date_i == round_date(date_i,"12 hours")) print(date_i)

}
  data_list[[j]]$CO2_tracer <-  ifelse(data_list[[j]]$signif,data_list[[j]]$CO2_tracer_gam,NA)
}
ggplot()+
  geom_point(data=subset(data_list[[1]],tiefe==-7),aes(date,CO2_tracer,col=paste(agg_steps[1],"hours")))+
  geom_point(data=subset(data_list[[2]],tiefe==-7),aes(date,CO2_tracer,col=paste(agg_steps[2],"hours")))+
  geom_point(data=subset(data_list[[3]],tiefe==-7),aes(date,CO2_tracer,col=paste(agg_steps[3],"hours")))+
  geom_point(data=subset(data_list[[4]],tiefe==-7),aes(date,CO2_tracer,col=paste(agg_steps[4],"hours")))+
  geom_point(data=subset(data_list[[5]],tiefe==-7),aes(date,CO2_tracer,col=paste(agg_steps[5],"hours")))
ggplot()+
  geom_line(data=subset(data_list[[3]],tiefe==-7),aes(date,CO2_tracer_gam,col=signif))
ggplot(data=subset(data_list[[1]],tiefe>=-11))+
  geom_line(aes(date,CO2_tracer_gam,col=as.factor(tiefe),linetype=signif))+
  geom_point(aes(date,CO2_tracer_gam,col=as.factor(tiefe),linetype=signif))
  geom_line(aes(date,CO2_tracer_gam,col=cor))
data$CO2_tracer <- ifelse(data$signif,data$CO2_tracer_gam,NA)
ggplot(subset(data,tiefe >= -7))+geom_line(aes(date,CO2_tracer,col=as.factor(tiefe)))
plot(data$signif)
data_5hours
data_12hours
data_24hours
a <- runif(3,max=10)
b <- a*1.5 + rnorm(3)
plot(a,b)
fm <- glm(b~a)
abline(fm)
summary(fm)
cor(sub_0_7$CO2_tracer_gam,sub_0_7$tiefe)
pval <- coef(summary(fm))[2,"Pr(>|t|)"]

fm_sum$coefficients[]
names(fm)
fm$qr
