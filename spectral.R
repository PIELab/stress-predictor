#! /usr/bin/Rscript

	#REMINDER: from R command-prompt run source("this/file/loc")

	print('reading data from table')
	zz.temp  <- read.table("data/IEratio4day.csv",header=FALSE,sep=',',colClasses="numeric")
	zz.df    <- t(zz.temp)#transpose the (flipped matlab csv)

	print('setting up data as time series object')
	#whole dataset:
	zz.ts <-  ts( data=zz.df, deltat=1/(24*60), start=c(0,6*60), end=c(4,6*60) ) # 1sample/min t in hrs	(4days = 96hrs, +6hr shift forward)

	zz.train <- ts( data=zz.timeSeries[1:4320]   , deltat=1/(60*24), start=c(0,6*60) ,end=c(3,6*60) )
	zz.test  <- ts( data=zz.timeSeries[4320:5761], deltat=1/(60*24), start=c(3,6*60) ,end=c(4,6*60) )

	print('plotting data')
	#general methods hereon followed from: http://maths-people.anu.edu.au/~johnm/courses/r/ASC2008/pdf/Rtimeseries-ohp.pdf	
	plot(zz.train, main='input data')
	print('plotting spectral analysis')
	dev.new()
	zz.spect <- spectrum(zz.train,method='ar')
	plot(zz.spect,main='spectral')
	print('plotting spectral decomposition')
	dev.new()
	plot(decompose(zz.train))
	print('plotting stl')
	dev.new()
	plot(stl(zz.train,s.window="periodic"),main='stl decomp')
	print('plotting data forecast')
	dev.new()
	library('forecast') # must be installed using install.packages('forecast') 
	#ALSO: this lib only works on R v2.15.1+ (I spent two hours on this; wheee)
	# details and publication for this package: http://robjhyndman.com/software/forecast/
	zz.pred <- forecast(zz.train)
	plot(zz.pred)
	dev.new()
	plot(zz.pred,xlim=c(3.2,4.2),main='prediction vs real data')
	points(zz.test,type='l',col='red',lwd=1)
	points(zz.train,type='l',col='green',lwd=1)
		
		

