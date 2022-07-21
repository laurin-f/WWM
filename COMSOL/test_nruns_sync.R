hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Kammermessungen_Arduino"
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)
##
load(file = paste(datapfad_PP_Kammer,"injectionrates.RData"))
#######################################
#####################################

modelname <- "Diffusion_freeSoil_anisotropy_optim_3DS_10runs"


outfile_raw <- "CO2_optim.txt"
COMSOL_exepath <- "C:/Program Files/COMSOL/COMSOL52a/Multiphysics/bin/win64/"
COMSOL_progammpath <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Programme/Fremdprogramme/COMSOL/"
         job = "b1"
         overwrite = F
         #inj_fun = "mean",
         nruns=str_extract(modelname,pattern = "\\d+(?=runs)") %>% as.numeric()
  
  outfile_full <- paste0(comsolpfad,outfile_raw)
  probe_table <- paste0(comsolpfad,"Probe_table.txt")
  probe <- F
  break_for <- F
  #par_file <- paste0(comsolpath,"input_pars.txt")
  cmd <- paste0("cd ",COMSOL_exepath,"&& comsolbatch.exe -inputfile ",COMSOL_progammpath,modelname,".mph -outputfile ",COMSOL_progammpath,modelname,"_solved.mph -job ",job)#," -paramfile ",par_file)
  
  if(file.exists(outfile_full)){
    file.remove(outfile_full)
  }
  
  
    if(comsolbatch_CPU()){
      cat("\n")
      system("taskkill /IM comsolbatch.exe /F",show.output.on.console=T)
    }
    

      
      
      probe_time <- rep(NA,nruns)
      out_time <- rep(NA,nruns)
      p <- 1
      o <- 1
      k <- 1
      while(p <= nruns & o <= nruns){
        
        #string that is parsed to commandline
        if(k == 1){
          start_time <- Sys.time()
          shell(cmd,translate=T,wait=T)
          k <- k+1
        }
        if(file.exists(probe_table)){
          probe_time[p] <- Sys.time()
          p <- p+1
          if(file.info(probe_table)$size > 1){
            
          #file.remove(probe_table)
          }
        }
        if(file.exists(outfile_full)){
          out_time[o] <- Sys.time()
          o <- o+1
          #file.remove(outfile_full)
        }
        
        
        
      }
            