#! /usr/bin/Rscript
	# read dataframe from file
	zz.df  <- read.csv("data/autoSenseSample.csv",strip.white=TRUE,header=TRUE,stringsAsFactors=FALSE)

	# select time-series object from dataframe
	zz.ts =  ts(data=zz.df[28],deltat=1/10,start=0,end=8.3) # 8 'tens of minutes'

	# plot input data
	plot(zz.ts,main='input data',xlab='time [10min]',ylab='HRV')
	
	# plot spectral graph
	dev.new()
	plot(spectrum(zz.ts,method="ar"),ylab='frequency[1/10min]')
	
	# plot time series decomposition components
	dev.new()
	plot(decompose(zz.ts))
	dev.new()
	plot(stl(zz.ts,s.window="periodic"))
	
	# predict on the data using the forecast package
	dev.new()
	library('forecast') # must be installed using install.packages('forecast') 
	#ALSO: this lib only works on R v2.15.1+ (I spent two hours learning this)
	# details and publication for this package: http://robjhyndman.com/software/forecast/
	plot(forecast(zz.ts),xlab='time [10mins]')

