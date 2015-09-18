# read various calibration runs & obs, compute daily Nash Sutcliffe Efficiency
# IMPORTANT: hydroGOF package needed to run this code.  
# to install: install.packages("hydroGOF")
# you can install this in your personal library if installing on kittyhawk

rm(list=ls())

library("hydroGOF")

source("read_Routput.r")
source("read_Rinput.r")

# what run is this?
name <- "Cane Creek"

make.outfile <- TRUE
session <- 3
totalRunNum <- 500
sampleNum <- 500 # helpful for testingi subset of runs

# apply thrheshold to NOT INCLUDE low flows
appthresh <- FALSE # turn this filtering on/off
lowthresh <- 0.1

# which runs to analyze
if (totalRunNum > sampleNum) {               	# if only analyzing a subset of your runs
	run.seq <- seq(from=1,to=sampleNum,by=1) 	#  chose first few runs
} else if (totalRunNum == sampleNum) {		 	# if examining all runs
	run.seq <- seq(from=1,to=totalRunNum,by=1)  #  go through them in order
} else {
	stop("sample is bigger than total # of runs, check your settings...")
}
	
# time period
startdate <- as.Date(paste('2009','10','1',sep="/")) # period of which to calc mass balance
enddate <- as.Date(paste('2010','9','30',sep="/"))

# where are all these files?
calibPath <- "PATH_TO_CALIBRATOR_OUTPUT" # should end in rhessys/output
streamObsfile <- "STREAM_OBS_FILE" # assume it's formatted for rhessys input already

# Read in observed streamflow
runDir <- paste("SESSION_",session,"_world_ITR_1",sep="")
runPath <- paste(calibPath,runDir,sep="/")
infile <- paste(runPath,"rhessys_basin.daily",sep="/")
ob.streamflowdf <- readRCInputVar(streamObsfile)
ob.streamflow.dates <- ob.streamflowdf$dates
ob.streamflow <- ob.streamflowdf$myvar	
	
# Select fields only in time period defined above
ob.s <- subset(ob.streamflow, ob.streamflow.dates >= startdate & ob.streamflow.dates <= enddate)

# apply threshold if appthres is on
if (appthresh) {
	backup.ob.s <- ob.s # keep copy of original
	ob.s[ob.s<lowthresh] <- NA 
}

# no-zeros version of obs streamflow for log NSE calc
noz.ob.s <- ob.s
noz.ob.s[noz.ob.s==0] <- NA # all zero's reassigned to NA
log.ob.s <- log(noz.ob.s)

nse <- rep(NA, sampleNum) # empty arrays for use later
log.nse <- rep(NA, sampleNum)

for (i in run.seq) {

# for the bored person watching the terminal window...
	cat("Analyzing run ",i," of ",sampleNum,"\n")
	
# Read in modeled streamflow
	runDir <- paste("SESSION_",session,"_world_ITR_",i,sep="")
	runPath <- paste(calibPath,runDir,sep="/")
	infile <- paste(runPath,"rhessys_basin.daily",sep="/")
	streamdf <- readBasinVar(infile,"streamflow")
	streamflow <- streamdf$streamflow
	moddates <- streamdf$dates
	
# Select fields only in time period defined above
	s <- subset(streamflow, moddates >= startdate & moddates <= enddate)

# apply threshold if appthres is on
if (appthresh) {
	backup.s <- s # keep copy of original
	s[s<lowthresh] <- NA 
}
	
# for log NSE, eliminate zeros
	noz.s <- s
	noz.s[noz.s==0] <- NA # all zero's reassigned to NA
	log.s <- log(noz.s) 

# calculate arithmetic NSE and log NSE
	nse[i] <- NSE(s,ob.s,na.rm=TRUE)
	log.nse[i] <- NSE(log.s, log.ob.s, na.rm=TRUE)
	
}

# report results
cat("Summary of NSE results: \n")
summ <- summary(nse)
print(summ)
cat("Summary of log NSE results: \n")
summ <- summary(log.nse)
print(summ)

if ( make.outfile == TRUE ) { 
	iname <- gsub(" ", "", name, fixed = TRUE) # take out whitespace
	outfilename <- paste("nse_",iname,"_sample",sampleNum,"of",totalRunNum,".csv",sep='') #image filename
	if (appthresh) outfilename <- paste("nse_",iname,"_maskedBelow",lowthresh,"_sample",sampleNum,"of",totalRunNum,".csv",sep='')
	if ( file.exists(outfilename)) file.remove(outfilename)
# make table object 
	vals <- matrix(c(run.seq,nse,log.nse),nrow=sampleNum,ncol=3)
	colnames(vals) <- c(',Run','NSE','logNSE')
	rownames(vals) <- seq(1:sampleNum)
	tab <- as.table(vals)
# write out table
	write.table(tab,file=outfilename,sep=",",quote=FALSE)
}
