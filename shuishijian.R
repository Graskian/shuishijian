# Library with functions for general data set manipulation and visualization, with special focus on time series
# Version 1.00, October 19th 2018
# Copyright Mathias Blicher Bjerregård

# ================================================ #
# ======== SPECIAL DATA LOADING FUNCTIONS ======== #
# ================================================ #

# Load rain data from DMI "Alle haendelser i CSV-format" via svk.dmi.dk
loadRain <- function(file,sep=";",format="%Y-%m-%d %H:%M",skip=1){
  
  rain <- read.csv(file,sep=sep,skip=skip,header=F)[,1:2]
  rain$V1 <- as.POSIXct(rain$V1,format=format)
  rain$V2 <- as.numeric(gsub(",",".",rain$V2))
  colnames(rain) <- c("TimeStamp","Rainfall")
  return(rain)
  
}

# ================================================ #
# ========= GENERAL DATASET MANIPULATION ========= #
# ================================================ #

# Column-number search. Returns column number given a data set and column name
cn <- function(x,a){
  
  which(colnames(x)==a)
  
}

# Convert a vector of time stamps quickly. List of possibilites will be expanded
convTime <- function(x,format){
  
  x <- switch(format,
              hofor = as.POSIXct(x,format="%d-%m-%Y %H:%M:%S"),
              standard = as.POSIXct(x,format="%Y-%m-%d %H:%M"),
              NA)
  return(x)
  
}

# Convert time in data set, where the time label is TimeStamp. Calls the convTime functions
convTime.df <- function(x,format){
  
  x$TimeStamp <- convTime(x$TimeStamp,format)
  return(x)
  
}

# Add boundaries to time series equal to zeros
addBounds <- function(x,time){

  df <- x[1:2,]
  df$TimeStamp <- time
  df[,2] <- 0
  
  rbind(df[1,],x,df[2,])
  
}

# Subset according to time interval. Requires time index vector to be denoted TimeStamp
timeSubset <- function(x,time){
  
  x <- x[(x$TimeStamp>=time[1] & x$TimeStamp<=time[2]),]
  
}

# Switch timezones of time vector. Supports UTC, CET and CEST
tzSwitch <- function(x,from,to){
  
  timezones <- c("UTC","CET","CEST")
  x + (which(timezones==to) - which(timezones==from))*3600
  
}

# Switch timezone in a data set, where the time label is TimeStamp. Calls the tzSwitch functions
tzSwitch.df <- function(x,from,to){
  
  x$TimeStamp <- tzSwitch(x$TimeStamp,from,to)
  return(x)
  
}

# ================================================ #
# ============ PLOT-RELATED FUNCTIONS ============ #
# ================================================ #

# Variant of the famous zoomPlot function
# Requires independent variable to have the name "TimeStamp" and be in POSIXct format
zoomTime <- function(x,c=2,time=NULL,format="simple",xlab="Time",ylab="Value",xres="day",col=1,
                     ylim=NULL,margins=NULL,las="V",xtext.coord=5.8,cex.x=1,cex.y=1,cex.main=1.2,cex.axis=1){
  
  # Select time range
  if(is.null(time)){
    time <- range(x$TimeStamp)
  }
  
  if(format == "simple"){
    time <- as.POSIXct(time,format="%Y-%m-%d")
  }else if(format == "min"){
    time <- as.POSIXct(time,format="%Y-%m-%d %H:%M")
  }
  
  # Limit data to selected time range
  x <- timeSubset(x,time)
  
  # Select plot range for dependent variable
  if(is.null(ylim)){
    ylim <- range(x[,c],na.rm=T)
  }
  
  # X-label orientation
  las <- switch(las,H=1,V=2)
  
  # Plot margins
  if(is.null(margins)){
    par(mar=c(7,4,4,2)+0.1)
  }else if(is.character(margins)){
    par(mar=c(5,4,4,2)+0.1)
  }else{
    par(mar=margins)
  }
  
  # Plot time series
  plot(0,type='n',xlim=time,ylim=ylim,main=colnames(x)[c],xlab="",ylab=ylab,xaxt="n",cex.main=cex.main)
  grid()
  lines(x$TimeStamp,x[,c],type="l",lwd=2,col=col)
  
  if(xres == "4h"){
    axis.POSIXct(1,at=seq(time[1],time[2],by=3600*4),format="%b-%d %H:00",las=las,cex.axis=0.85)
    mtext(xlab,side=1,line=xtext.coord,cex=cex.x)
  }else if(xres=="n"){
    axis.POSIXct(1,at=seq(time[1],time[2],by="days"),format="\n",las=las,cex.axis=0.85)
  }else{
    axis.POSIXct(1,at=seq(time[1],time[2],by=xres),format="%b %d",las=las,cex.axis=0.85)
    mtext(xlab,side=1,line=xtext.coord,cex=cex.x)
  }
  par(mar=c(5,4,4,2)+0.1)
  
}

# Add timeseries curves to existing plot. Default color = black
addSeries <- function(x,c,col=1){
  
  lines(x$TimeStamp,x[,c],lwd=2,col=col)
  
}

# General input-output plot
inoutPlot <- function(yout,xin,time,cols=c(2,4),margins=c(1.5,4,4,1)+0.1,xtext.coord=3,cex.x=0.75,cex.main=1.2){
  
  layout(matrix(c(1,1,1,1,1,2,2,2,2,2,2),ncol=1))
  zoomTime(yout,time=time,xres="n",col=cols[1],margins=margins,cex.main=cex.main)
  margins <- margins + c(xtext.coord+0.5,0,0,0)
  zoomTime(xin,time=time,col=cols[2],las="H",margins=margins,xtext.coord = xtext.coord,cex.x=cex.x,cex.main=cex.main)
  
}

# Specific input-output plot
inoutPlot.df <- function(yout,xin,time,cy=2,cx=2,cols=c(2,4),margins=c(1.5,4,4,1)+0.1,xtext.coord=3,cex.x=0.75,cex.main=1.2){
  
  # Select series
  if(is.character(cy)){
    yout <- yout[,c(1,cn(yout,cy))]
  }
  else{
    yout <- yout[,c(1,cy)]
  }
  if(is.character(cx)){
    xin <- xin[,c(1,cn(xin,cx))]
  }
  else{
    xin <- xin[,c(1,cx)]
  }
  
  # Input-output plot
  inoutPlot(yout,xin,time,cols,margins,xtext.coord,cex.x,cex.main)
  
}