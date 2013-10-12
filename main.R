#! /usr/bin/Rscript
	# this is a spectral analysis including exponential smoothing prediction
	# which treats 1 day as an entire 'seasonal cycle'. 



	# === CONFIGURATION ===
	PARTICIPANT_N <- 18	#particpant Number
	# NOTE: if the PARTICIPANT_N you choose does not work, check the PRE-DEFINED VALUE SETS below

	# === OPTIONAL COFIGURATION ITEMS: ===
	MISSING_DATA <- TRUE # TRUE if you want to check for missing rows. if FALSE, script will not check for them.

	#these are for adjusting the focus of the evaluation graph
	ZOOM.start <- 20
	ZOOM.end   <- 22.1

	DELTA_T = 1/(24*60) #once per minute, assumes cycle is 1 day

	# === PRE-DEFINED VALUE SETS FOR CONVENIENCE ===
	if(PARTICIPANT_N == 12){
		# name of data file
		fname <- "data/Participant_12_memphis_study_1/logsax.csv" 

		print(cat("const values for participant number",par_N,"undefined. quitting."))
		q()
		MISSING_DATA <- FALSE

#		# first time in dataset = ???
#		START.mon<- 0
#		START.day<- 0
#		START.hr <- 0
#		START.m  <- 0
		START.val<- 0 
#		# last time in the dataset = ???
#		END.mon  <- 0
#		END.day  <- 0
#		END.hr   <- 0
#		END.m    <- 0
#		END.val  <- END.day - START.day + ( END.hr*60 + END.m)/(1440)
#
#		# desired location of training/test set split in data
#		# NOTE: you will want to chose these approximately 2 cycles (days) before the end of the data
		SPLIT.index <- 4000 # "V4000",13
#		SPLIT.day   <- 0
#		SPLIT.hr    <- 0
#		SPLIT.m     <- 0
#		SPLIT.val   <- SPLIT.day - START.day + ( SPLIT.hr*60 + SPLIT.m )/(1440)

		# the column of interest in the csv file (yes, Variance is mispelled)
		INDEX_OF_INTEREST <- "V1"	

	}else if(PARTICIPANT_N == 18){
		# name of data file
		fname <- "data/Participants_18_memphis_study_2/all-groups_with_NAs.csv" 

		# first time in dataset = 2/7/2012 0:00
		START.mon<- 2
		START.day<- 7
		START.hr <- 0
		START.m  <- 0
		START.val<- 0 + (START.hr*60+START.m)/(1440)
		# last time in the dataset = 2/29/2012 0:19
		END.mon  <- 2
		END.day  <- 29
		END.hr   <- 0
		END.m    <- 19
		END.val  <- END.day - START.day + ( END.hr*60 + END.m)/(1440)

		# desired location of training/test set split in data
		# NOTE: you will want to chose these approximately 2 cycles (days) before the end of the data
		SPLIT.index <- 28820 # "28820","2/27/2012 0:19"
		SPLIT.day   <- 27
		SPLIT.hr    <- 0
		SPLIT.m     <- 19
		SPLIT.val   <- SPLIT.day - START.day + ( SPLIT.hr*60 + SPLIT.m )/(1440)

		# the column of interest in the csv file (yes, Variance is mispelled)
		INDEX_OF_INTEREST <- "Varience.of.RR.Interval"	

		ZOOM.start <- 20
		ZOOM.end   <- 22.1
	} else {
		print(cat("const values for participant number",par_N,"undefined. quitting."))
		q()
	}


	# === END OF DATA-SPECIFIC VALUES, BEGIN PROGRAM CODE ===

	print(cat("loading data from ",fname))
	zz.df  <- read.csv(fname,strip.white=TRUE,header=TRUE,stringsAsFactors=FALSE)

	print("available columns:")
	print(colnames(zz.df))
	# select time-series object from dataframe (use http://stat.ethz.ch/R-manual/R-patched/library/stats/html/ts.html for ref)
	print(cat("selected column: ",INDEX_OF_INTEREST))

	if (MISSING_DATA) {
		source('insert_missing_data.R')
		print(cat("saving the new csv file with missing data added to ", paste(fname,"_feature",INDEX_OF_INTEREST,".csv",sep="") ) )
		write.csv(zz.df,paste(fname,"_feature",INDEX_OF_INTEREST,".csv",sep=""))
	}

	print(cat("creating time-series object from minute ",START.val," to ",END.val,". #samples=",length(zz.df[INDEX_OF_INTEREST]) ))
	zz.ts <- ts(data=zz.df[[INDEX_OF_INTEREST]],deltat=DELTA_T,start=START.val)#,end=END.val)

	print("splitting into training & test sets")
	zz.train <- ts(data=zz.ts[1:SPLIT.index]            ,deltat=DELTA_T, start=START.val  )
	zz.test  <- ts(data=zz.ts[SPLIT.index:length(zz.ts)],deltat=DELTA_T, start=SPLIT.val)

	print("plotting data...")
	plot(zz.ts,main='input data',xlab='time [??]',ylab='HRV')

	#print ("ensuring dataframe is numeric ts")
	#zz.ts <- as.ts(as.numeric(zz.ts),deltat=DELTA_T,start=START.val)
	
	print("plotting spectral graph..")
	dev.new()
	plot(spectrum(as.numeric(zz.ts),method="ar"),ylab='frequency[1/??]')
	
	print("plotting time series decomposition components")
	dev.new()
	plot(decompose(zz.ts))
	dev.new()
	plot(stl(zz.ts,s.window="periodic"))
	
	print("predicting on the data using the forecast package")
	dev.new()
	library('forecast') # must be installed using install.packages('forecast') 
	#ALSO: this lib only works on R v2.15.1+ (I spent two hours learning this)
	# details and publication for this package: http://robjhyndman.com/software/forecast/
	zz.pred <- forecast(zz.train)
	plot(zz.pred,xlab='time [???]')

	print("plotting prediction evaluation")
	dev.new()
	plot(zz.pred,xlim=c(ZOOM.start,ZOOM.end),main='prediction vs real data',ylab='IE ratio',xlab='time [????]')
	points(zz.train,type='l',col='green',lwd=2)
	points(zz.test,type='l',col='red',lwd=2)

	legend(99.5,8, # places a legend at the appropriate place 
	c("training data","test data","forecast"), # puts text in the legend 
	lty=c(1,1,1), # gives the legend appropriate symbols (lines)
	lwd=c(2,2,2),col=c("green","red","blue")) # gives the legend lines the correct color and width

