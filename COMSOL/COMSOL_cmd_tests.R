COMSOL_exepath <- "C:/Program Files/COMSOL/COMSOL52a/Multiphysics/bin/win64/"
COMSOL_progammpath <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Programme/Fremdprogramme/COMSOL/"
modelname = "Diffusion_freeSoil_optim_3DS"
injection_rate <- 1e-6


  #snopt gibt nicht automatisch die DS werte aus deshalb wird ein batch in der GUI angelegt in dem der Output exportiert wird
  cmd <- paste0("cd ",COMSOL_exepath,"&& comsolbatch.exe -inputfile ",COMSOL_progammpath,modelname,".mph -outputfile ",COMSOL_progammpath,modelname,"_solved.mph -job b1 -pname injection_rate -plist ",paste0(injection_rate*c(1,1.1),collapse = ","))

  #cmd <- paste0("cd ",COMSOL_exepath,"&& comsol -run ",COMSOL_progammpath,modelname)
  shell(cmd,translate=T,intern=F)
  cmd <- paste0("cd ",COMSOL_exepath,"&& run")
  

  
  