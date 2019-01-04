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

#Use dplyr package to create base data frame for charts
chart.pred = select(ValidationSet,date,hig_return,pgr_return,trv_return,hig_r_Lag1,hig_r_Lag2,pgr_r_Lag1,pgr_r_Lag2,trv_r_Lag1,trv_r_Lag2)

#Populate prediction results
  #HIG
    chart.pred = cbind(chart.pred,pred.linear.hig.nl,pred.linear.hig.l1,pred.linear.hig.l2)
    chart.pred = cbind(chart.pred,pred.poly.hig.nl,pred.poly.hig.l1,pred.poly.hig.l2)
    chart.pred = cbind(chart.pred,pred.tree.hig.nl,pred.tree.hig.l1,pred.tree.hig.l2)
    chart.pred = cbind(chart.pred,err.linear.hig.nl,err.linear.hig.l1,err.linear.hig.l2)
    
  #TRV
    chart.pred = cbind(chart.pred,pred.linear.trv.nl,pred.linear.trv.l1,pred.linear.trv.l2)
    chart.pred = cbind(chart.pred,pred.poly.trv.nl,pred.poly.trv.l1,pred.poly.trv.l2)
    chart.pred = cbind(chart.pred,pred.tree.trv.nl,pred.tree.trv.l1,pred.tree.trv.l2)
    chart.pred = cbind(chart.pred,err.linear.trv.nl,err.linear.trv.l1,err.linear.trv.l2)
    
  #PGR
    chart.pred = cbind(chart.pred,pred.linear.pgr.nl,pred.linear.pgr.l1,pred.linear.pgr.l2)
    chart.pred = cbind(chart.pred,pred.poly.pgr.nl,pred.poly.pgr.l1,pred.poly.pgr.l2)
    chart.pred = cbind(chart.pred,pred.tree.pgr.nl,pred.tree.pgr.l1,pred.tree.pgr.l2)
    chart.pred = cbind(chart.pred,err.linear.pgr.nl,err.linear.pgr.l1,err.linear.pgr.l2)


#Charts for non-classification methods // comparison of actuals vs. predicted
  #HIG
    #No Lag
      plot.hig.nl = ggplot() +
        geom_line(data=chart.pred, aes(x=date, y=hig_return,color="Actual"))+
        geom_line(data=chart.pred, aes(x=date, y=pred.linear.hig.nl,color="Predicted - Linear"))+
        geom_line(data=chart.pred, aes(x=date, y=pred.poly.hig.nl,color="Predicted - Polynomial"))+
        geom_line(data=chart.pred, aes(x=date, y=pred.tree.hig.nl,color="Predicted - Tree"))+
        labs(title="Actual vs. Predicted - HIG - No Lag",x="Date", y = "Return Value")+
        labs(color='Legend')
    
    #Lag 1
      plot.hig.l1 = ggplot() +
        geom_line(data=chart.pred, aes(x=date, y=hig_return,color="Actual"))+
        geom_line(data=chart.pred, aes(x=date, y=pred.linear.hig.l1,color="Predicted - Linear"))+
        geom_line(data=chart.pred, aes(x=date, y=pred.poly.hig.l1,color="Predicted - Polynomial"))+
        geom_line(data=chart.pred, aes(x=date, y=pred.tree.hig.l1,color="Predicted - Tree"))+
        labs(title="Actual vs. Predicted - HIG - Lag 1 Week",x="Date", y = "Return Value")+
        labs(color='Legend')

    #Lag 2
      plot.hig.l2 = ggplot() +
        geom_line(data=chart.pred, aes(x=date, y=hig_return,color="Actual"))+
        geom_line(data=chart.pred, aes(x=date, y=pred.linear.hig.l2,color="Predicted - Linear"))+
        geom_line(data=chart.pred, aes(x=date, y=pred.poly.hig.l2,color="Predicted - Polynomial"))+
        geom_line(data=chart.pred, aes(x=date, y=pred.tree.hig.l2,color="Predicted - Tree"))+
        labs(title="Actual vs. Predicted - HIG - Lag 2 Weeks",x="Date", y = "Return Value")+
        labs(color='Legend')
      
  #TRV
    #No Lag
      plot.trv.nl = ggplot() +
      geom_line(data=chart.pred, aes(x=date, y=trv_return,color="Actual"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.linear.trv.nl,color="Predicted - Linear"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.poly.trv.nl,color="Predicted - Polynomial"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.tree.trv.nl,color="Predicted - Tree"))+
      labs(title="Actual vs. Predicted - TRV - No Lag",x="Date", y = "Return Value")+
      labs(color='Legend')
    
    #Lag 1
      plot.trv.l1 = ggplot() +
      geom_line(data=chart.pred, aes(x=date, y=trv_return,color="Actual"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.linear.trv.l1,color="Predicted - Linear"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.poly.trv.l1,color="Predicted - Polynomial"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.tree.trv.l1,color="Predicted - Tree"))+
      labs(title="Actual vs. Predicted - TRV - Lag 1 Week",x="Date", y = "Return Value")+
      labs(color='Legend')
    
    #Lag 2
      plot.trv.l2 = ggplot() +
      geom_line(data=chart.pred, aes(x=date, y=trv_return,color="Actual"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.linear.trv.l2,color="Predicted - Linear"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.poly.trv.l2,color="Predicted - Polynomial"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.tree.trv.l2,color="Predicted - Tree"))+
      labs(title="Actual vs. Predicted - TRV - Lag 2 Weeks",x="Date", y = "Return Value")+
      labs(color='Legend')
    
  #PGR
    #No Lag
      plot.pgr.nl = ggplot() +
      geom_line(data=chart.pred, aes(x=date, y=pgr_return,color="Actual"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.linear.pgr.nl,color="Predicted - Linear"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.poly.pgr.nl,color="Predicted - Polynomial"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.tree.pgr.nl,color="Predicted - Tree"))+
      labs(title="Actual vs. Predicted - PGR - No Lag",x="Date", y = "Return Value")+
      labs(color='Legend')
    
    #Lag 1
      plot.pgr.l1 = ggplot() +
      geom_line(data=chart.pred, aes(x=date, y=pgr_return,color="Actual"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.linear.pgr.l1,color="Predicted - Linear"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.poly.pgr.l1,color="Predicted - Polynomial"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.tree.pgr.l1,color="Predicted - Tree"))+
      labs(title="Actual vs. Predicted - PGR - Lag 1 Week",x="Date", y = "Return Value")+
      labs(color='Legend')
    
    #Lag 2
      plot.pgr.l2 = ggplot() +
      geom_line(data=chart.pred, aes(x=date, y=pgr_return,color="Actual"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.linear.pgr.l2,color="Predicted - Linear"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.poly.pgr.l2,color="Predicted - Polynomial"))+
      geom_line(data=chart.pred, aes(x=date, y=pred.tree.pgr.l2,color="Predicted - Tree"))+
      labs(title="Actual vs. Predicted - PGR - Lag 2 Weeks",x="Date", y = "Return Value")+
      labs(color='Legend')
      
#Show Charts
      plot.hig.nl
      plot.hig.l1
      plot.hig.l2
      
      plot.trv.nl
      plot.trv.l1
      plot.trv.l2
      
      plot.pgr.nl
      plot.pgr.l1
      plot.pgr.l2
      
#Plot all
multiplot( plot.hig.nl,
           plot.hig.l1,
           plot.hig.l2,cols=1)

multiplot( plot.trv.nl,
           plot.trv.l1,
           plot.trv.l2,cols=1)

multiplot( plot.pgr.nl,
           plot.pgr.l1,
           plot.pgr.l2,cols=1)
      
multiplot( plot.hig.nl,
           plot.hig.l1,
           plot.hig.l2,
           
           plot.trv.nl,
           plot.trv.l1,
           plot.trv.l2,
           
           plot.pgr.nl,
           plot.pgr.l1,
           plot.pgr.l2,
           cols=3)
