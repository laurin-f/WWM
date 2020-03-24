#' Title
#'
#' @param y
#' @param col
#' @param group
#' @param data
#' @param timestep
#' @param plot
#' @param ylab
#'
#' @return
#' @export
#'
#' @examples
leave_NAtime_plot <- function(y="CO2",
                              col="tiefe",
                              group="Pumpstufe",
                              data,
                              timestep = 10,
                              plot=T, ylab="CO2 [ppm]"){#minutes

  data_order <- data[order(data$date),]
  data_sub <- data_order[!is.na(data_order[,group]),]
  starts<- data_sub$date[c(1,which(difftime(data_sub$date[-1],data_sub$date[-nrow(data_sub)],units = "mins") > timestep) +1)]
  ends<- data_sub$date[c(which(difftime(data_sub$date[-1],data_sub$date[-nrow(data_sub)],units = "mins") > timestep),nrow(data_sub))]

  period.df <- data.frame(starts,ends)

  data_sub$period <- NA
  for(i in 1:nrow(period.df)){
    data_sub$period[data_sub$date >= period.df$starts[i] & data_sub$date <= period.df$ends[i] ] <- i
  }

  # data_list <- apply(period.df,1 ,function(x) data_sub[data_sub$date >= x[1] & data_sub$date <= x[2],])
  # plot_list <- lapply(data_list,function(x){
  #   p <- ggplot(x)+
  #     geom_line(aes(date,x[,y],col=as.factor(x[,col])))+
  #     labs(col=col,y=y)
  #     return(p)
  #   })
  # gridExtra::grid.arrange(plot_list[[1]]+guides(col=F),plot_list[[2]],ncol=2)
  if(plot ==F){
    return(data_sub)
  }else{
    p <- ggplot(data_sub)+
      geom_line(aes(date,data_sub[,y],col=as.factor(data_sub[,col])))+
      facet_wrap(~period,scales="free_x",nrow=1)+
      labs(col=col,y=ylab)+
      theme(strip.text.x = element_blank())
    p
    return(p)
    }
}

#' Title
#'
#' @param p
#' @param data
#' @param breaks
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
adj_grob_size <- function(p, data ,breaks="10 hours", ...){
  # convert ggplot object to grob object
  p <- p+scale_x_datetime(date_breaks = breaks, ...)
  gp <- ggplotGrob(p)


  # get gtable columns corresponding to the facets (5 & 9, in this case)
  facet.columns <- gp$layout$l[grepl("panel", gp$layout$name)]

  # get the number of unique x-axis values per facet (1 & 3, in this case)

  x.var <- sapply(unique(data$period),
                  function(x) difftime(max(data$date[data$period==x]),min(data$date[data$period==x]),units="mins"))
  #oder
  #x.var <- sapply(unique(data$period),
  #                function(x) length(which(data$period==x )))


  # change the relative widths of the facet columns based on
  # how many unique x-axis values are in each facet
  gp$widths[facet.columns] <- gp$widths[facet.columns] * (x.var/sum(x.var))

  # plot result
  grid::grid.draw(gp)
}
