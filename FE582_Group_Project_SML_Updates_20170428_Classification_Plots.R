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

#HIG
  higS1 = tibble::rownames_to_column(higS,"RET") 
  higS2 = melt(higS1,id.vars = "RET")
  higS2$RET <- factor(higS2$RET, levels = c("No Lag","Lag1","Lag2"))
  
  plot.hig.class = ggplot(data=higS2,aes(x=RET,y=value, fill=variable))+
    geom_bar(stat="identity", position=position_dodge())+
    labs(title="Classification Model Success Comparison - HIG",x="Variable Considered", y = "Success Rate")+
    scale_fill_discrete(name="Model")+
    scale_fill_hue(c=45, l=100)+
    scale_fill_brewer(palette = "BuPu")
  
#TRV
  trvS1 = tibble::rownames_to_column(trvS,"RET") 
  trvS2 = melt(trvS1,id.vars = "RET")
  trvS2$RET <- factor(trvS2$RET, levels = c("No Lag","Lag1","Lag2"))
  
  plot.trv.class = ggplot(data=trvS2,aes(x=RET,y=value, fill=variable))+
    geom_bar(stat="identity", position=position_dodge())+
    labs(title="Classification Model Success Comparison - TRV",x="Variable Considered", y = "Success Rate")+
    scale_fill_discrete(name="Model")+
    scale_fill_hue(c=45, l=100)+
    scale_fill_brewer(palette = "BuPu")
  
#PGR
  pgrS1 = tibble::rownames_to_column(pgrS,"RET") 
  pgrS2 = melt(pgrS1,id.vars = "RET")
  pgrS2$RET <- factor(pgrS2$RET, levels = c("No Lag","Lag1","Lag2"))
  
  plot.pgr.class = ggplot(data=pgrS2,aes(x=RET,y=value, fill=variable))+
    geom_bar(stat="identity", position=position_dodge())+
    labs(title="Classification Model Success Comparison - PGR",x="Variable Considered", y = "Success Rate")+
    scale_fill_discrete(name="Model")+
    scale_fill_hue(c=45, l=100)+
    scale_fill_brewer(palette = "BuPu")

#Plot all
multiplot(plot.hig.class,plot.trv.class,plot.pgr.class,cols=2)

plot.hig.class
plot.trv.class
plot.pgr.class
