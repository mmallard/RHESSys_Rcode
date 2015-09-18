# reads various calibration runs and makes mass balance plots (assumes streamflow is in calibrator version 2 format)
library(OIdata)
library(ggplot2)
library(gridExtra)
library(gridBase)

rm(list=ls())

# available in repository
source("read_ROutput.r")
source("read_RInput.r")
source("read_CalibratorV2input.r")

# ------- Settings ---------
name <- "Testbed Creek" # designate a name for this set of runs
make.png <- TRUE
calibPath <- "PATH_TO_CALIBRATOR_OUTPUT" # should end in rhessys/output
session <- 2
plotRuns <- c('106') # which iterations to plot?
startdate <- as.Date(paste('2001','10','1',sep="/")) # period of which to plot
enddate <- as.Date(paste('2005','9','30',sep="/"))
streamObsfile <- "STREAM_OBS_FILE" # assumes it's formatted for rhessys input already
precipObsfile <- "PATH_TO_PRECIP_FILE" # should end in .rain

for (i in plotRuns) {

# Read in vars from various files
# basin variables
	runDir <- paste("SESSION_",session,"_world_ITR_",i,sep="")
	runPath <- paste(calibPath,runDir,sep="/")
	infile <- paste(runPath,"rhessys_basin.daily",sep="/")
	streamdf <- readBasinVar(infile,"streamflow")
	streamflow <- streamdf$streamflow
	moddates <- streamdf$dates
	precipdf <- readBasinVar(infile,"precip")
	precip <- precipdf$precip
	evapdf <- readBasinVar(infile,"evap")
	evap <- evapdf$evap
	transdf <- readBasinVar(infile,"trans")
	trans <- transdf$trans
# observed streamflow
	ob.streamflowdf <- readRC2InputVar(streamObsfile,"streamflow_mm")
	ob.streamflow.dates <- ob.streamflowdf$dates
	ob.streamflow <- ob.streamflowdf$streamflow_mm	
# read parameter set from .out file
	outfile <- list.files(path=runPath,pattern="*.out",full.names=TRUE)
	parmline <- read.table(outfile,skip=1,nrow=1)
	parmset <- paste("m=",parmline$V25," k=",parmline$V26," depth=",parmline$V27," gw1=",parmline$V32," gw2=",parmline$V33,sep="")
# read observed precip 
	ob.precipdf <- readRCInputVar(precipObsfile)
	ob.p.dates <- ob.precipdf$dates
	ob.p <- ob.precipdf$myvar * 1000.0 # convert units from m to mm
	
# Select fields only in time period defined above
	d <- moddates[moddates >= startdate & moddates <= enddate]
	p <- subset(precip, moddates >= startdate & moddates <= enddate)
	e <- subset(evap, moddates >= startdate & moddates <= enddate)
	tr <- subset(trans, moddates >= startdate & moddates <= enddate)
	s <- subset(streamflow, moddates >= startdate & moddates <= enddate)
	ob.s <- subset(ob.streamflow, ob.streamflow.dates >= startdate & ob.streamflow.dates <= enddate)
	ob.p <- subset(ob.p, ob.p.dates >= startdate & ob.p.dates <= enddate)
	
# accumulate
	p.acc <- sum(p)
	s.acc <- sum(s)
	e.acc <- sum(e)
	tr.acc <- sum(tr)
	ob.s.acc <- sum(ob.s)
	ob.p.acc <- sum(ob.p)
	ob.e.acc <- ob.p.acc - ob.s.acc # "observed" evap
	et.acc <- e.acc + tr.acc
	
# compute runoff ratios
	mratio <- s.acc/p.acc
	oratio <- ob.s.acc/ob.p.acc
	
# make table object for plotting later
	shorten <- function(x) format(round(x, 2), nsmall=2)
	vals <- matrix(c(shorten(ob.p.acc),shorten(ob.s.acc),shorten(ob.e.acc),shorten(oratio),shorten(p.acc),shorten(s.acc),shorten(et.acc),shorten(mratio)),nrow=4,ncol=2)
	colnames(vals) <- c('Observed','Modeled')
	rownames(vals) <- c('Precip (mm)','Streamflow (mm)','ET (mm)', 'Runoff ratio')
	tab <- as.table(vals)

# create streamflow array with no zero values, for use when deriving & plotting cdf
	noz.s <- s
	noz.s[noz.s==0] <- min(s[s>0]) # all zero's reassigned to data's min value
	noz.ob.s <- ob.s
	noz.ob.s[noz.ob.s==0] <- min(ob.s[ob.s>0])	

# log streamflow
	log.s <- log(s)
	log.ob.s <- log(ob.s)
	
# cdf of streamflows (no zero's version of streamflow)
	s.cdf <- ecdf(noz.s)
	ob.s.cdf <- ecdf(noz.ob.s)

# plot
	if ( make.png == T ) { 
	iname <- gsub(" ", "", name, fixed = TRUE) # take out whitespace
	imagename <- paste("massBalance_",iname,"_S",session,"_I",i,".png",sep='') #image filename
	if ( file.exists(imagename)) file.remove(imagename)
	png(imagename,width=900,height=600)
	}
	par(mfrow=c(2,2)) # 4 panel, 2 x 2
	options(scipen=999) # don't use scientific notation

# top left panel
	ypmax = max(p)+(0.5*max(p))
	par(mar=c(5,4,4,5)+.1)
	barplot(p,col='blue',border = NA,yaxt="n",ylim=c(ypmax,0)) # precip
	axis(4,cex.axis=1.5)
	mtext("Precip [mm/day]", side=4, line=3,cex=1.5)
	par(new=TRUE)
	ysmax = max(ob.s,s)+(0.25*max(ob.s,s))
	plot(d,ob.s,type="l",lwd=2,col='black',ylab="Streamflow [mm/day]",xlab="",xaxt='n',ylim=c(0,ysmax),cex.lab=1.5,cex.axis=1) # streamflow obs
	lines(d,s,col='darkgreen',lwd=2,xlab="",xaxt='n',ylim=c(0,ysmax),cex.lab=1.5,cex.axis=1) # streamflow
	dates2 <- seq.Date(from=d[1],by='month',to=d[length(d)])
	axis.Date(side=1,at=dates2,labels=format(dates2,'%y'),tick=F,mgp=c(3,2,1),cex.axis=1.5)
	axis.Date(side=1,at=dates2,labels= substring(format(dates2,'%b'),1,1),tick=F,cex.axis=1.5)
# top right
	plot(d,ob.s,log="y", type="l", col="black", ylab='',xaxt='n',xlab='') 
	lines(d,s,col="darkgreen")
	dates2 <- seq.Date(from=d[1],by='month',to=d[length(d)])
	axis.Date(side=1,at=dates2,labels=format(dates2,'%y'),tick=F,mgp=c(3,2,1),cex.axis=1.5)
	axis.Date(side=1,at=dates2,labels= substring(format(dates2,'%b'),1,1),tick=F,cex.axis=1.5)
# bottom left
	ob.cdf.vals <- knots(ob.s.cdf)
	s.cdf.vals  <- knots(s.cdf)
	cdfmin <- min(s.cdf.vals, ob.cdf.vals)
	cdfmax <- max(s.cdf.vals, ob.cdf.vals)
	plot(ob.s.cdf,col='black',ylab='',xlab='',log="x",xlim=c(cdfmin,cdfmax),main='')
	lines(s.cdf, col='darkgreen')
# bottom right, done with ggplot
	plot.new()              
	vps <- baseViewports()
	pushViewport(vps$figure) ##   I am in the space of the autocorrelation plot
	vp1 <-plotViewport(c(1.8,1,0,1)) ## create new vp with margins, you play with this values 
	tabplot <- qplot(1:10, 1:10, geom = "blank") + 
	theme_bw() +
	theme(line = element_blank(),
	text = element_blank()) +
	annotation_custom(grob = tableGrob(tab,
	# change font sizes:
	gpar.coltext = gpar(cex = 1.2),
	gpar.rowtext = gpar(cex = 1.2)),
	xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
	print(tabplot,vp = vp1)
# almost done...
	titletext <- paste("Session ",session," Iteration ",i,", ",startdate," to ",enddate,", Black (obs), Model (green), Precip (blue)",sep="")
	subtext <- parmset
	title(main=titletext, sub=subtext, outer=TRUE, line=-2,cex.main=1.5,cex.sub=1.5)
	dev.off()
	
}

cat("Adios!\n")
