#! /usr/bin/Rscript
	#this is a spectral analysis including exponential smoothing prediction of P12's data

	# read dataframe from file
	fname <- "data/Participant_12_memphis_study_1/a1.csv" 

	# == for treating 1 day as an entire 'seasonal cycle'.
	# start = 2/7/2012 0:00
	START.mon<- 2
	START.day<- 6
	START.hr <- 16
	START.m  <- 30
	START.val<- START.hr*60+START.m
	# end = 2/29/2012 0:19
	END.mon  <- 2
	END.day  <- 6
	END.hr   <- 23
	END.m    <- 19
	END.val  <- END.hr*60 + END.m

	#location of training/test set split in data
	SPLIT.index <- 100 # 06-Feb-2012 19:38:19
	SPLIT.val   <- 19*50 + 38

	INDEX_OF_INTEREST <- "Varience.of.RR.Interval"	#the column of interest in the csv file (yes, Variance is mispelled)

	#=== END OF DATA-SPECIFIC VALUES ===

	print(cat("loading data from ",fname))
	zz.df  <- read.csv(fname,strip.white=TRUE,header=TRUE,stringsAsFactors=FALSE)


	print("available columns:")
	print(colnames(zz.df))
	# select time-series object from dataframe (use http://stat.ethz.ch/R-manual/R-patched/library/stats/html/ts.html for ref)
	indexOfItem <- "Varience.of.RR.Interval"	#the column of interest in the csv file (yes, Variance is mispelled)
	print(cat("selected column: ",indexOfItem))


	print("adding NA values for missing rows") 

	insertRow <- function(existingDF, newrow, r) {
	  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
	  existingDF[r,] <- newrow
	  existingDF
	}

	formatTime <- function(year,mon,day,hr,min){
		if ( min < 10 ) mmm <- cat('0',m) else mmm <- min #add padding 0 to minutes
		formattedTimeString <- cat(mon,'/',day,'/',year,' ',hr,':',min)
		formattedTimeString
	}

	# NOTE: START.mon MUST = END.mon for this
	d <- START.day
	hr<- START.hr
	m <- START.m
	i <- 1
	rowsInserted <- 0
	while (i < length(zz.df[1]) ){
		currentTimeStr <- formatTime('2012',START.mon,d,hr,m)
		print( cat(currentTimeStr,'=?=',row["time"]) )
		while ( currentTimeStr != row["time"]){ # if next minute is not in the data
			# col[0] = "time", the rest are NA TODO:add col[1] = "timestamp" as well
			NA_VALUE <- "NA"
			newRow <- c( currentTimeStr,rep(NA_VALUE,length(zz.df)-1) )
			print( paste("adding row ",newRow,". new length=",length(zz.df[1]),sep="") )
			insertRow(zz.df,newRow,i) # add the row
			rowsInserted <- rowsInserted+1

			# move to the next minute
			m <- m + 1
			if ( m >= 60 ){ # 60m = 1hr
				hr <- hr + 1
				m  <- 0
				if ( hr >= 24 ){ #24hr = 1d
					d <- d+1
					hr <- 0
			}
			#update current time string
			currentTimeStr <- formatTime('2012',START.mon,d,hr,m)
		}
		i <- i+1
	}
	print (cat(rowsInserted," NA rows inserted."))

	DELTA_T = 1/(24*60)
	print(cat("creating time-series object from minute ",START.val," to ",END.val,". #samples=",length(zz.df[indexOfItem]) ))
	zz.ts <- ts(data=zz.df[[indexOfItem]],deltat=DELTA_T,start=START.val)#,end=END.val)

	print("splitting into training & test sets")
	zz.train <- ts(data=zz.ts[1:SPLIT.index]           ,deltat=DELTA_T, start=START.val  )
	zz.test  <- ts(data=zz.ts[SPLIT.index:length(zz.ts)],deltat=DELTA_T, start=SPLIT.val+1)

	print("plotting data...")
	plot(zz.ts,main='input data',xlab='time [min]',ylab='HRV')
	
	print("plotting spectral graph..")
	dev.new()
	plot(spectrum(zz.ts,method="ar"),ylab='frequency[1/min]')
	
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
	plot(zz.pred,xlab='time [mins?]')

	print("plotting prediction evaluation")
	dev.new()
	plot(zz.pred,xlim=c(99.5,102),main='prediction vs real data',ylab='IE ratio',xlab='time [hrs]')
	points(zz.test,type='l',col='red',lwd=2)
	points(zz.train,type='l',col='green',lwd=2)

	legend(99.5,8, # places a legend at the appropriate place 
	c("training data","test data","forecast"), # puts text in the legend 
	lty=c(1,1,1), # gives the legend appropriate symbols (lines)
	lwd=c(2,2,2),col=c("green","red","blue")) # gives the legend lines the correct color and width

