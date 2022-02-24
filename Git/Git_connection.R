# Proxy-Einstellungen für Push/Pull mit Github
## aus https --> http machen
Sys.setenv(HTTPS_PROXY=gsub("s", "", Sys.getenv("HTTPS_PROXY") ))
## Fehler unterdruecken
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)

devtools::install_github("laurin-f/pkg.WWM")

#install.packages("gitcreds")
#gitcreds::gitcreds_set()
