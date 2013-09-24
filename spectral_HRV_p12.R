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

	print(cat("loading data from ",fname))
	zz.df  <- read.csv(fname,strip.white=TRUE,header=TRUE,stringsAsFactors=FALSE)


	print("available columns:")
	print(colnames(zz.df))
	# select time-series object from dataframe (use http://stat.ethz.ch/R-manual/R-patched/library/stats/html/ts.html for ref)
	indexOfItem <- "Varience.of.RR.Interval"	#the column of interest in the csv file (yes, Variance is mispelled)
	print(cat("selected column: ",indexOfItem))

	print(cat("creating time-series object from minute ",START.val," to ",END.val,". #samples=",length(zz.df[indexOfItem]) ))

	# add NA values for missing times 
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
		#2/29/2012 0:03
		currentTimeStr <- formatTime('2012',START.mon,d,hr,m)
		print( cat(currentTimeStr,'=?=',row["time"]) )
		while ( currentTimeStr != row["time"]){ # if next minute is not in the data
			# col[0] = "time", the rest are NA TODO:add col[1] = "timestamp" as well
			NA_VALUE <- "NA"
			newRow <- c( currentTimeStr,rep(NA_VALUE,length(zz.df)-1) )
			print( cat("adding row ",newRow) )
			insertRow(zz.df,newRow,i) # add the row
			rowsInserted <- rowsInserted+1

			# move to the next minute
			m <- m + 1
			if ( m >= 60 ){ # 60m = 1hr
				hr <- hr + 1
				if ( hr >= 24 ){ #24hr = 1d
					d <- d+1
					hr<- 0
				m  <- 0
				}
			}
			#update current time string
			currentTimeStr <- formatTime('2012',START.mon,d,hr,m)
		}
		i <- i+1
	}
	print (cat(rowsInserted," NA rows inserted."))

	zz.ts <- ts(data=zz.df[[indexOfItem]],deltat=1/(24*60),start=START.val)#,end=END.val)

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
	plot(forecast(zz.ts),xlab='time [mins]')

	#TODO: add eval

