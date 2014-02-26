#! /usr/bin/Rscript

	#REMINDER: from R command-prompt run source("this/file/loc")

	#both of these do not seem to work as expected:
	#read.table("dataFile.dat", fileEncoding="latin1", header=TRUE, row.names = 1, sep = " ")
	#zz <- read.csv("dataFile.csv", strip.white = TRUE)

	#this also does not work as expected:
	#read.table("beer.dat", fileEncoding="latin1", header=FALSE, row.names = 1, sep = "\n")

	#but this works:
	zz.df <- read.csv("beer.dat",strip.white=TRUE, header=TRUE, stringsAsFactors=FALSE)	#sample dataframe


	#for sample:
	zz.ts =  ts(data=zz.df,frequency=12,start=c(1956,1),end=c(1995,5))	#there is some guestimation on the dates here, b/c i'm being lazy
	#notes on method here: http://faculty.washington.edu/ezivot/econ424/Working%20with%20Time%20Series%20Data%20in%20R.pdf

	#for autoSense data:
		#NOTE:   zz.df[28] = heart rate variance (according to ls(zz.df))
#	zz.ts =  ts(data=zz.df[28],frequency=83,start=c(0,0),end=c(1,18)) # for time shown in hrs 
#	zz.ts =  ts(data=zz.df[28],deltat=1,start=0,end=83) # f=525600/yr = for time shown in mins
#	zz.ts =  ts(data=zz.df[28],deltat=1/10,start=0,end=8.3) # 8 'tens of minutes'

	#general methods hereon followed from: http://maths-people.anu.edu.au/~johnm/courses/r/ASC2008/pdf/Rtimeseries-ohp.pdf	
	plot(zz.ts)
	dev.new()
	plot(spectrum(zz.ts,method="ar"))
	dev.new()
	#if(periodic) #for now, comment out if not enough periods observed or "not periodic"?
		plot(decompose(zz.ts))
		dev.new()
		plot(stl(zz.ts,s.window="periodic"))
		dev.new()
		library('forecast') # must be installed using install.packages('forecast') 
		#ALSO: this lib only works on R v2.15.1+ (I spent two hours on this; wheee)
		# details and publication for this package: http://robjhyndman.com/software/forecast/
		plot(forecast(zz.ts))

