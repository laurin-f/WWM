library(pkg.WWM)
update_dynament.db()

?read_db()
?read.Em50()
test<-read.Em50("long",datelim = c("2020.01.01 00:00:00","2021.01.01 00:00:00"))
test
?unzip.files()

devtools::create("pkg.WWM")
