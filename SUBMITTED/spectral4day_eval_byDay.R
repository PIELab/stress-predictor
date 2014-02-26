#! /usr/bin/Rscript

	# this script perfomrs forecasting on IE ratio data using days as the 'natural unit'
	print('reading data from table')
	zz.temp  <- read.table("data/IEratio4day.csv",header=FALSE,sep=',',colClasses="numeric")
	zz.df    <- t(zz.temp)#transpose the (flipped matlab csv)

	print('setting up data as time series object')
	#load the dataset:
	zz.tsRAW <-  ts( data=zz.df, deltat=1/(24*60), start=c(0,6*60), end=c(4,6*60) ) # 1sample/min t in hrs	(4days = 96hrs, +6hr shift forward)

	#split into train & test sets
	zz.train <- ts( data=zz.timeSeries[1:4320]   , deltat=1/(60*24), start=c(0,6*60) ,end=c(3,6*60) )
	zz.test  <- ts( data=zz.timeSeries[4320:5761], deltat=1/(60*24), start=c(3,6*60) ,end=c(4,6*60) )

	print('plotting data')
	plot(zz.train, main='input data')
	print('plotting spectral analysis')
	dev.new()
	zz.spect <- spectrum(zz.train,method='ar')
	plot(zz.spect,main='spectral',xlab='frequency [1/day]')
	print('plotting time series decomposition')
	dev.new()
	plot(decompose(zz.train))
	print('plotting stl decomposition')
	dev.new()
	plot(stl(zz.train,s.window="periodic"),main='stl decomp')
	print('plotting data forecast')
	dev.new()
	library('forecast') 
	zz.pred <- forecast(zz.train)
	plot(zz.pred)
	dev.new()	#closer view
	plot(zz.pred,xlim=c(3.2,3.5),main='prediction vs real data',ylab='IE ratio',xlab='time [days]')
	points(zz.test,type='l',col='red',lwd=1)
	points(zz.train,type='l',col='green',lwd=1)
	legend(3.2,8, # places a legend at the appropriate place 
	c("training data","test data","forecast"), # puts text in the legend 
	lty=c(1,1,1), # gives the legend appropriate symbols (lines)
	lwd=c(2,2,2),col=c("green","red","blue")) # gives the legend lines the correct color and width

	dev.new()	#wider view
	plot(zz.pred,xlim=c(3.2,4.2),main='prediction vs real data',ylab='IE ratio',xlab='time [days]')
	points(zz.test,type='l',col='red',lwd=1)
	points(zz.train,type='l',col='green',lwd=1)
	legend(3.2,8, # places a legend at the appropriate place 
	c("training data","test data","forecast"), # puts text in the legend 
	lty=c(1,1,1), # gives the legend appropriate symbols (lines)
	lwd=c(2,2,2),col=c("green","red","blue")) # gives the legend lines the correct color and width

	
		#REMINDER: from R command-prompt run source("this/file/loc")	

