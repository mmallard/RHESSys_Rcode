# function to read in text files used as input to rhessys calibrator version 2.0
# example - precip or streamflow files formatted like this...
# datetime,streamflow_mm,precip_mm
#1/1/2001,0.195174943,0
#1/2/2001,0.183694064,0
#1/3/2001,0.183694064,0
#1/4/2001,0.183694064,0
# ...
# Returns dataframe with 2 columns ("dates" and column with variable name that was requested)
# ASSUMES daily inputs are in file

readRC2InputVar <- function(infile,varname) { # Takes filename & var name (either precip_mm or streamflow_mm)
    if (!file.exists(infile)) {
      cat("File name ",infile,"\n")
      stop("File does not exist \n")
    }
    cat("Reading ",infile,"\n")
    filedf <- read.csv(file=infile, sep=",", header=TRUE)
    vnames <- names(filedf)
    numcol <- match(varname,vnames) # match column headers in this file with variable that was requested
    if (is.na(numcol)) { 
      cat("Variable name is not in the file. \n Must match one of the following:")
      print(vnames)
      stop("\n")
    }
    len <- length(filedf$datetime)
	  startdatedf <- read.csv(file=infile,header=FALSE,nrows=1,skip=1)
	  startdate <- as.Date(startdatedf$V1, format = "%m/%d/%Y")
    dates <- seq.Date(from=startdate,by='day',length.out=len) # sequence of dates corresponding with variable
    newdf <- cbind.data.frame(dates, varname=filedf[numcol])
    return(newdf)
}

