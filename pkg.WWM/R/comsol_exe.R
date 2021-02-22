
#' Function to execute Comsol Modell with defined parameters
#'
#' @param modelname name of the Comsol Modell without .mph ending
#' @param input_pars named vector or dataframe where names represent Parameter names as defined in Comsol
#' @param outfile_name name that the Comsol Export file should get
#'
#' @return
#' @export
#'
#' @examples comsol_exe(model="Produktionseimer",input_pars=input_pars_9,outfile="CO2_flux_prod_9.txt")
comsol_exe <- function(modelname,input_pars,outfile_new=NULL,outfile_raw="CO2_flux_prod.txt") {

  par_file <- paste0(comsolpfad,"input_pars.txt")
  if(!is.data.frame(input_pars)){
    input_pars <- t(input_pars)
  }
  write.table(input_pars,file=par_file,row.names = F,quote = F,sep = " ")
  cmd <- paste0("cd ",COMSOL_exepath,"&& comsolbatch.exe -inputfile ",COMSOL_progammpath,modelname,".mph  -job b1 ",input_pars_cmd)
  cmd <- paste0("cd ",COMSOL_exepath,"&& comsolbatch.exe -inputfile ",COMSOL_progammpath,modelname,".mph  -job b1 -paramfile ",par_file)
  #commandline befehl ausführen
  shell(cmd,translate=T,intern=F)
  outfile_raw_full <- paste0(comsolpfad,outfile_raw)
  if(!is.null(outfile_new)){
  if(file.exists(outfile_raw_full)){
    file.rename(outfile_raw_full,paste0(comsolpfad,outfile_new))
  }else{
    print("no outfile found")
  }
  }

}

# input_pars <- c("prod_1"=1.2,"prod_2"=1.3,"prod_3"=0)
# modelname <- "Produktionseimer"
