hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"

files.R <- list.files(hauptpfad,pattern="\\.(R|r)$",recursive=T,full.names = T)
files.R <- files.R[-grep("change_hauptpath.R",files.R)]
codes <- lapply(files.R,readLines)

codes.new <- lapply(codes,stringr::str_replace,pattern = "hauptpfad\\s?<-.*",replacement = paste0("hauptpfad <- \"",hauptpfad,"\""))

for(i in seq_along(codes.new)){
  writeLines(codes.new[[i]] ,files.R[i])
  }
