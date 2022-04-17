plotMoustache = function( codaSamples, datFrm , yName="y" , xName="x" ) {
  mcmcMat = as.matrix(codaSamples,chains=TRUE)
  chainLength = NROW( mcmcMat )
  y = datFrm[,yName]
  x = as.numeric(as.factor(datFrm[,xName]))
  xlevels = levels(as.factor(datFrm[,xName]))
  # Display data with posterior predictive distributions
  #openGraph(width=min(10,1.25*length(xlevels)),height=5)
  par(mar=c(3,3,2,0.5)) # number of margin lines: bottom,left,top,right
  par(mgp=c(1.75,0.5,0)) # which margin lines to use for labels
  plot(-1,0, 
       xlim=c(0.1,length(xlevels)+0.1) , 
       xlab=xName , xaxt="n" , ylab=yName ,
       ylim=c(min(y)-0.2*(max(y)-min(y)),max(y)+0.2*(max(y)-min(y))) , 
       main="Data with Posterior Predictive Distrib.")
  axis( 1 , at=1:length(xlevels) , tick=FALSE , lab=xlevels )
  for ( xidx in 1:length(xlevels) ) {
    xPlotVal = xidx 
    yVals = y[ x==xidx ]
    points( rep(xPlotVal,length(yVals))+runif(length(yVals),-0.05,0.05) , 
            yVals , pch=1 , cex=1.5 , col="red" )
    chainSub = round(seq(1,chainLength,length=20))
    for ( chnIdx in chainSub ) {
      m = mcmcMat[chnIdx,paste("m[",xidx,"]",sep="")]
      s = mcmcMat[chnIdx,paste("ySigma",sep="")]
      nu = 1000 # effectively normal instead of mcmcMat[chnIdx,"nu"]
      tlim = qt( c(0.025,0.975) , df=nu )
      yl = m+tlim[1]*s
      yh = m+tlim[2]*s
      ycomb=seq(yl,yh,length=201)
      #ynorm = dnorm(ycomb,mean=m,sd=s)
      #ynorm = 0.67*ynorm/max(ynorm)
      yt = dt( (ycomb-m)/s , df=nu )
      yt = 0.67*yt/max(yt)
      lines( xPlotVal-yt , ycomb , col="skyblue" ) 
    }
  }
}
