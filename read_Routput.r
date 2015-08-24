# M. Mallard, August 2015
# function to read in rhessys_basin.daily output files
# Called with the file name and variable name (see headers at the top of rhessys_basin.daily file)
# Will return dataframe with 2 columns ("dates" and name of variable that was retrieved)

# Takes filename and string variable varname.  varname MUST match one of the headers in the basin.daily file 
readBasinVar <- function(filename,varname) {
	if (!file.exists(infile)) {
		cat("File name ",infile,"\n")
		stop("File does not exist \n")
	}
	cat("Reading ",infile,"\n")
	basin <- read.csv(file=infile, sep="", header=TRUE)
	bnames <- names(basin)
	numcol <- match(varname,bnames) # match header names found in file with the varname requested
	if (is.na(numcol)) {
		cat("Variable name is not in the file. \n Must match one of the following:")
		print(bnames)
		stop("\n")
	}
	len <- length(basin$day)
	startdd <- basin$day[1]
	startmm <- basin$month[1]
	startyy <- basin$year[1]
	startdate <- as.Date(paste(startyy,startmm,startdd,sep="/"))
	dates <- seq.Date(from=startdate,by='day',length.out=len) # sequence of dates corresponding with variable
	newdf <- cbind.data.frame(dates, varname=basin[numcol])	
	return(newdf)
}
