library(  drc)
?ryegrass

## Displaying the data set
head(ryegrass,n=2)

## Fitting a four-parameter Weibull model (type 2)
ryegrass.m1 <- drm(rootl ~ conc, data = ryegrass, fct = W2.4())

## Displaying a summary of the model fit
summary(ryegrass.m1)

## Plotting the fitted curve together with the original data
plot(ryegrass.m1)

## Fitting a four-parameter Weibull model (type 1)
ryegrass.m2 <- drm(rootl ~ conc, data = ryegrass, fct = W1.4())
plot(ryegrass.m2)

## Fitting a four-parameter log-logistic model
## with user-defined parameter names
ryegrass.m3 <- drm(rootl ~ conc, data = ryegrass, 
                   fct = LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
summary(ryegrass.m3)


###########################
## Comparing log-logistic and Weibull models
## (Figure 2 in Ritz (2009))
ryegrass.m0 <- drm(rootl ~ conc, data = ryegrass, fct = LL.4())
ryegrass.m1 <- drm(rootl ~ conc, data = ryegrass, fct = W1.4())
ryegrass.m2 <- drm(rootl ~ conc, data = ryegrass, fct = W2.4())

plot(ryegrass.m0, broken=TRUE, xlab="Dose (mM)", ylab="Root length (cm)", lwd=2, 
     cex=1.2, cex.axis=1.2, cex.lab=1.2)
plot(ryegrass.m1, add=TRUE, broken=TRUE, lty=2, lwd=2)
plot(ryegrass.m2, add=TRUE, broken=TRUE, lty=3, lwd=2)

arrows(3, 7.5, 1.4, 7.5, 0.15, lwd=2)
text(3,7.5, "Weibull-2", pos=4, cex=1.2)

arrows(2.5, 0.9, 5.7, 0.9, 0.15, lwd=2)
text(3,0.9, "Weibull-1", pos=2, cex=1.2)



edLL<-data.frame(ED(ryegrass.m0,c(10,50,90),interval="delta"),ll="Log-logistic")
edW1<-data.frame(ED(ryegrass.m1,c(10,50,90),interval="delta"),ll="Weibull1")
edW2<-data.frame(ED(ryegrass.m2,c(10,50,90),interval="delta"),ll="Weibull2")
CompED<-rbind(edLL,edW1,edW2)
library(ggplot2)
###################
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
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
#####################

p1 <- ggplot(data=CombED[c(1,4,7),], aes(x=ll, y=Estimate))+
 geom_bar(stat="identity", fill="lightgreen", colour="black")+
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.1) +
  ylab("ED10")+
  xlab("")
p2 <- ggplot(data=CombED[c(2,5,8),], aes(x=ll, y=Estimate))+
  geom_bar(stat="identity", fill="lightgreen", colour="black")+
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.1) +
  ylab("ED50")+
  xlab("")

p3 <- ggplot(data=CombED[c(3,6,9),], aes(x=ll, y=Estimate))+
geom_bar(stat="identity", fill="lightgreen", colour="black")+
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=0.1) +
  ylab("ED90")+
  xlab("Sigmoid four parameter model")
x11()
multiplot(p1,p2,p3)
multiplot(p1, p2, cols=2)



CombED<-rbind(edLL,edW1,edW2)
comped(CombED[c(1,4),1],CombED[c(1,4),2],log=F,operator = "-")
comped(CombED[c(1,7),1],CombED[c(1,7),2],log=F,operator = "-")
comped(CombED[c(3,6),1],CombED[c(3,6),2],log=F,operator = "-")
comped(CombED[c(6,9),1],CombED[c(6,9),2],log=F,operator = "-")

comped(CombED[c(3,9),1],CombED[c(3,9),2],log=F,operator = "-")
comped(CombED[c(6,9),1],CombED[c(6,9),2],log=F,operator = "-")
