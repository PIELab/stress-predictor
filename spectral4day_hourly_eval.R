#! /usr/bin/Rscript

	# ARIMA forecasting on IE ratio data using hours as the 'natural unit', then compare forecast to actual data

	#REMINDER: from R command-prompt run source("this/file/loc")

	print('reading data from table')
	zz.temp  <- read.table("data/IEratio4day.csv",header=FALSE,sep=',',colClasses="numeric")
	zz.df    <- t(zz.temp)#transpose the (flipped matlab csv)

	print('setting up data as time series object')
	#whole dataset:
	zz.ts <-  ts(data=zz.df,deltat=1/60,start=6,end=102) # 1sample/min t in hrs	(4days = 96hrs, +6hr shift forward)

	zz.train <- ts(data=zz.timeSeries[1:5640]   ,deltat=1/60, start=6 ,end=100 )
	zz.test  <- ts(data=zz.timeSeries[5641:5761],deltat=1/60, start=c(100,1),end=102)

	print('plotting data')
	#general methods hereon followed from: http://maths-people.anu.edu.au/~johnm/courses/r/ASC2008/pdf/Rtimeseries-ohp.pdf	
	plot(zz.train, main='input data')
	print('plotting spectral analysis')
	dev.new()
	zz.spect <- spectrum(zz.train,method='ar')
	plot(zz.spect,main='spectral',xlab='frequency [1/hour]')
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
	plot(zz.pred,xlim=c(99.5,102),main='prediction vs real data',ylab='IE ratio',xlab='time [hrs]')
	points(zz.test,type='l',col='red',lwd=2)
	points(zz.train,type='l',col='green',lwd=2)

	legend(99.5,8, # places a legend at the appropriate place 
	c("training data","test data","forecast"), # puts text in the legend 
	lty=c(1,1,1), # gives the legend appropriate symbols (lines)
	lwd=c(2,2,2),col=c("green","red","blue")) # gives the legend lines the correct color and width

