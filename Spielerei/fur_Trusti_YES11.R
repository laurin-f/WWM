
n <- 10^6
df <- data.frame(WS=sample(c("YES","NO"),n,replace=T,prob=c(0.1,0.9)),datum=1:n)

############
#for lösung Trusti
startfor<-Sys.time()
for(i in 12:length(df$datum)){
  
  if(df[i, "WS"] == "Yes"){
    df[(i-11):(i-1), "WS"] = "Yes"
  } 
  
}
tdifffor <- Sys.time() - startfor


##########################
#apply lösung
startapply<-Sys.time()

#ids der YES Werte
YESid <- which(df$WS=="YES")

#von YESids 11:1 abziehen
YES_apply <- sapply(11:1, function(x) YESid - x)
#alle unique werte aus der matrix als vector
YES_vec <- unique(c(YES_apply))
#werte kleiner null raus
YES_final <- YES_vec[YES_vec > 0]

###############
#anderer Ansatz mit apply
# YESid11 <- YESid - 11
# YESapply <- apply(cbind(YESid11,YESid),1,function(x) x[1]:x[2])
# YES_vec <- unique(c(YESapply))
# YES_final <- YES_vec[YES_vec > 0]

#Yes werte ändern
df$WS[YES_final] <- "YES"
tdiffapply <- Sys.time() - startapply

#Rechenzeit vergleichen
tdifffor
tdiffapply
