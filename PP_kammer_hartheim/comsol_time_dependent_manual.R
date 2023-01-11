hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
comsolpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")

inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Kammermessungen_Arduino"
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)

Versuch <- 7
load(file=paste0(datapfad_PP_Kammer,"data_tracer_hartheim.RData"))
data_all <- data

#for(Versuch in 1:8){

data <- data_all[data_all$Versuch == Versuch & !is.na(data_all$Versuch),]


data_agg <- data %>% 
  mutate(tracer_mol = ppm_to_mol(CO2_tracer_drift,"ppm",T_C = T_soil),
         D0 = D0_T_p(T_soil,unit="m^2/s"),
         tiefe = abs(tiefe),
         inj_mol_m2_s = round(inj_mol_m2_s,3)) %>% 
  dplyr::filter(inj == 1 & !is.na(tracer_mol))

  


data_agg$t <- difftime(data_agg$date, min(data_agg$date),units = "min") %>% as.numeric()
inj_rate <-  median(data_agg$inj_mol_m2_s)


data_wide <- tidyr::pivot_wider(data_agg,id_cols = c("date","PPC_2"),names_from = tiefenstufe,values_from = tracer_mol,names_prefix = "tracer_")
########################################
#PPC steps
##########################################
step_thr <- 0.02
data_wide <- data_wide[order(data_wide$date),]

PPC_steps <- data_wide %>%
  mutate(PPC_diff = abs(c(NA,diff(PPC_2))),
         step = ifelse(PPC_diff > step_thr,1,0))
ggplot(PPC_steps)+geom_line(aes(date,PPC_diff))+
  geom_hline(yintercept = step_thr)

step_date <- sort(unique(PPC_steps$date[PPC_steps$step == 1]))
step_date <- step_date[c(as.numeric(diff(step_date)),100) > 60]
step_date <- step_date[!is.na(step_date)]
data_wide <- data_wide %>% 
  mutate(step = ifelse(date %in% !!step_date,1,0),
         step_id=cumsum(step),
         date= round_date(date,"60 mins")) %>% 
  filter(step_id > 0) %>% 
  group_by(date) %>% 
  summarise(across(everything(),mean)) %>% 
  mutate(step_id = round(step_id))

data_wide$t_sec <- difftime(data_wide$date, min(data_wide$date),units = "s") %>% as.numeric()

ggplot(data_wide)+
  geom_point(aes(date,tracer_4,col=factor(step_id),group=1))

 CO2_obs_1 <- subset(data_wide,step_id == 1) %>% 
   mutate(t_sec = as.numeric(difftime(date, min(date),units = "s")))
 CO2_obs_2 <- subset(data_wide,step_id > 1) %>% 
    mutate(t_sec = as.numeric(difftime(date, min(date),units = "s")))



cols <- c("t_sec",paste0("tracer_",1:7))
write.table(
  CO2_obs_1[,cols],
  paste0(metapfad_comsol, "CO2_obs_td1.csv"),
  col.names = F,
  row.names = F,
  sep = ","
)
# write.table(
#   tail(CO2_obs_1[,cols],1)[,-1],
#   paste0(metapfad_comsol, "CO2_obs_init.csv"),
#   col.names = F,
#   row.names = F,
#   sep = ","
# )
write.table(
  CO2_obs_2[,cols],
  paste0(metapfad_comsol, "CO2_obs_td2.csv"),
  col.names = F,
  row.names = F,
  sep = ","
)
#######################################
programmpfad <- paste0(hauptpfad,"Programme/Fremdprogramme/COMSOL_td_optim/")
comsol_exe(modelname = "Diffusion_optim_timedependent_2studies",study = "std7",COMSOL_progammpath = programmpfad,overwrite_model = F)
comsol_exe(modelname = "Diffusion_optim_timedependent_2studies",job = "b1")
comsol_exe(modelname = "Diffusion_manual_timedependent_2studies",job = "b1",overwrite_model = T,
           input_pars = c("injection_rate" = inj_rate,"DS_1" = 2.179532e-06,"DS_2" = 6.085935e-07))
comsol_exe(modelname = "Diffusion_manual_timedependent_2studies",job = "b2",overwrite_model = F,
           input_pars = c("injection_rate" = inj_rate,"DS_1" = 2.179e-06,"DS_2" = 5e-7))

files <- list.files(comsolpfad,pattern="CO2_mod_td\\d.txt",full.names = T)

td_ls <- lapply(files,read.table,skip = 9)
colnames_raw <- lapply(files,readLines, n = 9)
td_long_ls <- vector("list",length(files))
for(i in seq_along(td_ls)){
colnames(td_ls[[i]]) <-   str_extract_all(colnames_raw[[i]][9],
                                    "r|z|(c|DS_\\d)( \\(.{5,7}\\))? @ t=\\d+",
                              simplify = T) %>% 
  str_replace_all(c("( \\(.{5,7}\\))? @" = ""," t=" = "_", "c" = "CO2_mod"))

td_long_ls[[i]] <-
  tidyr::pivot_longer(
    td_ls[[i]],
    cols = matches("_\\d+$"),
    names_to = c(".value", "t"),
    names_pattern = "(CO2_mod|DS_\\d)_(\\d+)"
  ) %>% 
  mutate(t = as.numeric(t),
         step = i)

if(i > 1){
  td_long_ls[[i]]$t <- td_long_ls[[i]]$t + max(td_long_ls[[(i-1)]]$t)
}
}

td <- do.call(rbind,td_long_ls)

td$tiefe <- (abs(td$z-max(td$z)))


NA_tiefen <- data %>% 
  group_by(tiefe) %>%
  summarise(n_NAS = length(which(!is.na(CO2_tracer_drift))) / length(CO2_tracer_drift))

tiefen <- (-NA_tiefen$tiefe[NA_tiefen$n_NAS > 0.9])

td <- subset(td,tiefe %in% tiefen)

td_long <- tidyr::pivot_longer(subset(td,tiefe == tiefen[1]),matches("DS_\\d"),names_to = "DS_id",values_to = "DS")
# ggplot()+
#   geom_line(data = td_long,aes(t,CO2_mod,col=tiefe,linetype = paste("DS",unique(DS_1))))+
#   geom_line(data = td_long_2,aes(t,CO2_mod,col=tiefe,linetype = paste("DS",unique(DS_1))))

CO2_plot <- ggplot(data_agg)+
  geom_line(aes(t,tracer_mol,col=as.factor(tiefe),linetype="obs"))+
  theme(axis.title.x = element_blank())+
  labs(y=expression(CO[2]~(ppm)),linetype="",col="depth (cm)")+
  geom_line(data = td,aes(t,CO2_mod,col=factor(tiefe),linetype = "mod"))

unique(td_long$DS)
DS_plot <- ggplot(td_long)+
  geom_line(aes(t,DS,col=DS_id))+
  labs(col="")
ggpubr::ggarrange(CO2_plot,DS_plot,align = "v",ncol=1,heights = c(3,1))+
  ggsave(paste0(plotpfad_PPchamber,"Comsol_timedependent_manual_test.png"),width=8,height = 7)
  


