
check.packages<-function(pkgs){
  old.pkgs<-installed.packages()
  new.pkgs<-pkgs[!pkgs %in% old.pkgs[,"Package"]]
  if(length(new.pkgs)>0){
    install.packages(new.pkgs, dependencies = TRUE)
  }
  sapply(pkgs, require,character.only=T)
}
