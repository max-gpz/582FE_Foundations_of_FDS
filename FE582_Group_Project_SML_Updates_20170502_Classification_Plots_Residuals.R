# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Residual Charts
#HIG
  #HIG - Linear
  plot.hig.err.linear = ggplot() +
    geom_line(data=chart.pred, aes(x=date, y=err.linear.hig.nl,color="No Lag"))+
    geom_line(data=chart.pred, aes(x=date, y=err.linear.hig.l1,color="Lag 1"))+
    geom_line(data=chart.pred, aes(x=date, y=err.linear.hig.l2,color="Lag 2"))+
    labs(title="Residual - HIG - Linear ",x="Date", y = "Residual Value")+
    labs(color='Legend')
  
  #HIG - Poly
  plot.hig.err.poly = ggplot() +
    geom_line(data=chart.pred, aes(x=date, y=err.poly.hig.nl,color="No Lag"))+
    geom_line(data=chart.pred, aes(x=date, y=err.poly.hig.l1,color="Lag 1"))+
    geom_line(data=chart.pred, aes(x=date, y=err.poly.hig.l2,color="Lag 2"))+
    labs(title="Residual - HIG - poly ",x="Date", y = "Residual Value")+
    labs(color='Legend')
  
  #HIG - Tree
  plot.hig.err.tree = ggplot() +
    geom_line(data=chart.pred, aes(x=date, y=err.tree.hig.nl,color="No Lag"))+
    geom_line(data=chart.pred, aes(x=date, y=err.tree.hig.l1,color="Lag 1"))+
    geom_line(data=chart.pred, aes(x=date, y=err.tree.hig.l2,color="Lag 2"))+
    labs(title="Residual - HIG - tree ",x="Date", y = "Residual Value")+
    labs(color='Legend')
  
#TRV
  #TRV - Linear
  plot.trv.err.linear = ggplot() +
    geom_line(data=chart.pred, aes(x=date, y=err.linear.trv.nl,color="No Lag"))+
    geom_line(data=chart.pred, aes(x=date, y=err.linear.trv.l1,color="Lag 1"))+
    geom_line(data=chart.pred, aes(x=date, y=err.linear.trv.l2,color="Lag 2"))+
    labs(title="Residual - TRV - Linear ",x="Date", y = "Residual Value")+
    labs(color='Legend')
  
  #TRV - Poly
  plot.trv.err.poly = ggplot() +
    geom_line(data=chart.pred, aes(x=date, y=err.poly.trv.nl,color="No Lag"))+
    geom_line(data=chart.pred, aes(x=date, y=err.poly.trv.l1,color="Lag 1"))+
    geom_line(data=chart.pred, aes(x=date, y=err.poly.trv.l2,color="Lag 2"))+
    labs(title="Residual - TRV - poly ",x="Date", y = "Residual Value")+
    labs(color='Legend')
  
  #TRV - Tree
  plot.trv.err.tree = ggplot() +
    geom_line(data=chart.pred, aes(x=date, y=err.tree.trv.nl,color="No Lag"))+
    geom_line(data=chart.pred, aes(x=date, y=err.tree.trv.l1,color="Lag 1"))+
    geom_line(data=chart.pred, aes(x=date, y=err.tree.trv.l2,color="Lag 2"))+
    labs(title="Residual - TRV - tree ",x="Date", y = "Residual Value")+
    labs(color='Legend')
  
#PGR
  #PGR - Linear
  plot.pgr.err.linear = ggplot() +
    geom_line(data=chart.pred, aes(x=date, y=err.linear.pgr.nl,color="No Lag"))+
    geom_line(data=chart.pred, aes(x=date, y=err.linear.pgr.l1,color="Lag 1"))+
    geom_line(data=chart.pred, aes(x=date, y=err.linear.pgr.l2,color="Lag 2"))+
    labs(title="Residual - PGR - Linear ",x="Date", y = "Residual Value")+
    labs(color='Legend')
  
  #PGR - Poly
  plot.pgr.err.poly = ggplot() +
    geom_line(data=chart.pred, aes(x=date, y=err.poly.pgr.nl,color="No Lag"))+
    geom_line(data=chart.pred, aes(x=date, y=err.poly.pgr.l1,color="Lag 1"))+
    geom_line(data=chart.pred, aes(x=date, y=err.poly.pgr.l2,color="Lag 2"))+
    labs(title="Residual - PGR - poly ",x="Date", y = "Residual Value")+
    labs(color='Legend')
  
  #PGR - Tree
  plot.pgr.err.tree = ggplot() +
    geom_line(data=chart.pred, aes(x=date, y=err.tree.pgr.nl,color="No Lag"))+
    geom_line(data=chart.pred, aes(x=date, y=err.tree.pgr.l1,color="Lag 1"))+
    geom_line(data=chart.pred, aes(x=date, y=err.tree.pgr.l2,color="Lag 2"))+
    labs(title="Residual - PGR - tree ",x="Date", y = "Residual Value")+
    labs(color='Legend')
  
#Show Charts
  #HIG
  plot.hig.err.linear
  plot.hig.err.poly
  plot.hig.err.tree
  
  #TRV
  plot.trv.err.linear
  plot.trv.err.poly
  plot.trv.err.tree
  
  #PGR
  plot.pgr.err.linear
  plot.pgr.err.poly
  plot.pgr.err.tree
  
  #Plot all
  multiplot( plot.hig.err.linear,
             plot.hig.err.poly,
             plot.hig.err.tree,
             cols=1)
  
  multiplot( plot.trv.err.linear,
             plot.trv.err.poly,
             plot.trv.err.tree,
             cols=1)
  
  multiplot( plot.pgr.err.linear,
             plot.pgr.err.poly,
             plot.pgr.err.tree,
             cols=1)