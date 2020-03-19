#pfade definieren
#detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)

hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"

datelim <- c("2020.03.02 00:00:00","2020.03.04 23:00:00")
data <- read_db("GGA.db","micro",datelim = datelim)

test <- split_chamber(data,
                      closing_before = 5,
                      closing_after = 10,
                      opening_before = -5,
                      opening_after = -15,
                      t_max=Inf,
                      t_min=2,
                      adj_openings = T)



test <- subset(test, !is.na(messid))
tdiff <- c(which(diff(test$date)>600),nrow(test))
period_list <- sapply(seq_along(tdiff),function(x) rep(x,each=ifelse(x>1,(tdiff[x]-tdiff[x-1]),tdiff[x])))
test$period <- do.call("c",period_list)

p <- ggplot(test)+geom_point(aes(date,CO2,col=as.factor(messid)))+facet_wrap(~period,scales="free_x",nrow=1)

# convert ggplot object to grob object
gp <- ggplotGrob(p)


# get gtable columns corresponding to the facets (5 & 9, in this case)
facet.columns <- gp$layout$l[grepl("panel", gp$layout$name)]

# get the number of unique x-axis values per facet (1 & 3, in this case)

x.var <- sapply(unique(test$period),
                function(x) difftime(max(test$date[test$period==x]),min(test$date[test$period==x]),units="mins"))
#oder
#x.var <- sapply(unique(test$period),
#                function(x) length(which(test$period==x )))


# change the relative widths of the facet columns based on
# how many unique x-axis values are in each facet
gp$widths[facet.columns] <- gp$widths[facet.columns] * (x.var/sum(x.var))

# plot result
grid::grid.draw(gp)
