# read various calibration runs and compute a hydrograph

rm(list=ls())

source("read_ROutput.r") # download these from repository
source("read_RInput.r")

# --------- Settings -----------
make.png <- TRUE 
calibPath <- "PATH_TO_CALIBRATOR_OUTPUT" # should end in rhessys/output
session <- 3
plotRuns <- c('73') # which iterations to plot
startdate <- as.Date(paste('2009','10','1',sep="/")) # period to plot
enddate <- as.Date(paste('2010','9','30',sep="/"))
streamObsfile <- "STREAM_OBS_FILE" # assumes it's formatted for rhessys input already
precipObsfile <- "PATH_TO_PRECIP_FILE" # should end in .rain

for (i in plotRuns) {

# Read in vars from various files
# basin variables
	runDir <- paste("SESSION_",session,"_world_ITR_",i,sep="")
	infile <- paste(calibPath,runDir,"rhessys_basin.daily",sep="/")
	streamdf <- readBasinVar(infile,"streamflow")
	streamflow <- streamdf$streamflow
	moddates <- streamdf$dates
	precipdf <- readBasinVar(infile,"precip")
	precip <- precipdf$precip
# observed streamflow
	ob.streamflowdf <- readRCInputVar(streamObsfile)
	ob.streamflow.dates <- ob.streamflowdf$dates
	ob.streamflow <- ob.streamflowdf$myvar	
	
# Select fields only in time period defined above
	d <- moddates[moddates >= startdate & moddates <= enddate]
	p <- subset(precip, moddates >= startdate & moddates <= enddate)
	s <- subset(streamflow, moddates >= startdate & moddates <= enddate)
	ob.s <- subset(ob.streamflow, ob.streamflow.dates >= startdate & ob.streamflow.dates <= enddate)
	
# plot
	if ( make.png == T ) {
	imagename <- paste("hydrograph","_S",session,"_I",i,".png",sep='')
	if ( file.exists(imagename)) file.remove(imagename)
	png(imagename,width=900,height=600)
	}
	
	ypmax = max(p)+(0.5*max(p))
	par(mar=c(5,4,4,5)+.1)
	barplot(p,col='blue',border = NA,yaxt="n",ylim=c(ypmax,0)) # precip
	axis(4,cex.axis=1.5)
	mtext("Precip [mm/day]", side=4, line=3,cex=1.5)
	titletext <- paste("Session ",session," Iteration ",i," Obs streamflow (black), modeled streamflow (green), & precip (blue)",sep="")
	title(titletext)
	par(new=TRUE)
	ysmax = max(ob.s,s)+(0.25*max(ob.s,s))
	plot(d,ob.s,type="l",lwd=2,col='black',ylab="Streamflow [mm/day]",xlab="",xaxt='n',ylim=c(0,ysmax),cex.lab=1.5,cex.axis=1.5) # streamflow obs
	lines(d,s,col='darkgreen',lwd=2,xlab="",xaxt='n',ylim=c(0,ysmax),cex.lab=1,cex.axis=1.5) # streamflow

	dates2 <- seq.Date(from=d[1],by='month',to=d[length(d)])
	axis.Date(side=1,at=dates2,labels=format(dates2,'%y'),tick=F,mgp=c(3,2,1),cex.axis=1.5)
	axis.Date(side=1,at=dates2,labels= substring(format(dates2,'%b'),1,1),tick=F,cex.axis=1.5)
	
# almost done...
	dev.off()
	
}

cat("Adios!\n")
