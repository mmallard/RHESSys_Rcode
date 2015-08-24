# M. Mallard, August 2015
# function to read in text files used as input to rhessys 
# Will read precip or streamflow files formatted like this...
# year month day hour
# value1
# value2
# ...
# Returns dataframe object with 2 columns ("dates" and "myvar") 

readRCInputVar <- function(thisfile) { # Takes filename
	if (!file.exists(thisfile)) stop("File does not exist \n")
	cat("Reading ",thisfile,"\n")
	vardf <- read.csv(file=thisfile, header=FALSE, skip=1) # skip date and read single column of values below
	myvar <- vardf$V1 
	datedf <- read.csv(file=thisfile,header=FALSE,nrows=1) # read only the line containing the date
	startdate <- as.Date(levels(datedf$V1[1]),"%Y %m %d 1")
	len <- length(vardf$V1)
	dates <- seq.Date(from=startdate,by='day',length.out=len) # make sequence of dates corresponding values from file
	newdf <- cbind.data.frame(dates, myvar)	
	return(newdf)
}
