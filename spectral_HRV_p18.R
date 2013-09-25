#! /usr/bin/Rscript
	#this is a spectral analysis including exponential smoothing prediction of P12's data

	# read dataframe from file
	fname <- "data/Participants_18_memphis_study_2/all-groups_with_NAs.csv" 

	# == for treating 1 day as an entire 'seasonal cycle'.
	# start = 2/7/2012 0:00
	START.mon<- 2
	START.day<- 7
	START.hr <- 0
	START.m  <- 0
	START.val<- 0 + (START.hr*60+START.m)/(1440)
	# end = 2/29/2012 0:19
	END.mon  <- 2
	END.day  <- 29
	END.hr   <- 0
	END.m    <- 19
	END.val  <- END.day - START.day + ( END.hr*60 + END.m)/(1440)

	#location of training/test set split in data
	# NOTE: you will want to chose these approximately 2 cycles (days) before the end of the data
	SPLIT.index <- 28820 # "28820","2/27/2012 0:19"
	SPLIT.day   <- 27
	SPLIT.hr    <- 0
	SPLIT.m     <- 19
	SPLIT.val   <- SPLIT.day - START.day + ( SPLIT.hr*60 + SPLIT.m )/(1440)

	INDEX_OF_INTEREST <- "Varience.of.RR.Interval"	#the column of interest in the csv file (yes, Variance is mispelled)

	#these are for adjusting the focus of the evaluation graph
	ZOOM.start <- 20
	ZOOM.end   <- 22.1

	DELTA_T = 1/(24*60) #once per minute, assumes cycle is 1 day

	#=== END OF DATA-SPECIFIC VALUES ===

	print(cat("loading data from ",fname))
	zz.df  <- read.csv(fname,strip.white=TRUE,header=TRUE,stringsAsFactors=FALSE)


	print("available columns:")
	print(colnames(zz.df))
	# select time-series object from dataframe (use http://stat.ethz.ch/R-manual/R-patched/library/stats/html/ts.html for ref)
	print(cat("selected column: ",INDEX_OF_INTEREST))


	print("adding NA values for missing rows") 

	# insert newrow into the given dataframe at index r and return new dataframe
	insertRow <- function(existingDF, newrow, r) {
	  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
	  existingDF[r,] <- newrow
	  existingDF
	}

	# return a formatted time string using yr mon day hr min
	formatTime <- function(year,mon,day,hr,min){
		if ( min < 10 ) {mmm <- paste('0',m,sep="")} else {mmm <- min} #add padding 0 to minutes
		formattedTimeString <- paste(mon,'/',day,'/',year,' ',hr,':',mmm,sep="")
		formattedTimeString
	}

	# list structure to use list[a,b,c,,d,] <- func() things. see here for more info:https://stat.ethz.ch/pipermail/r-help/2004-June/053343.html
	list <- structure(NA,class="result")
	"[<-.result" <- function(x,...,value) {
		args <- as.list(match.call())
		args <- args[-c(1:2,length(args))]
		length(value) <- length(args)
		for(i in seq(along=args)) {
		  a <- args[[i]]
		  if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
		}
		x
	}

	# return the next day, hr, and min values
	nextMinute <- function(d,hr,m){
		# move to the next minute
		m <- m + 1
		if ( m >= 60 ){ # 60m = 1hr
			hr <- hr + 1
			m  <- 0
			if ( hr >= 24 ){ #24hr = 1d
				d <- d+1
				hr <- 0
			}
		}
		c(d,hr,m)
	}

	# NOTE: START.mon MUST = END.mon for this
	d <- START.day
	hr<- START.hr
	m <- START.m
	ind <- 1 #index
	rowsInserted <- 0 #count of total inserted rows
	while (ind < length(zz.df[[1]]) ){
		#2/29/2012 0:03
		currentTimeStr <- formatTime('2012',START.mon,d,hr,m)
		cat( '\r',currentTimeStr,'=?=',zz.df[["time"]][ind] )
		while ( !is.na(zz.df[["time"]][ind]) && currentTimeStr != zz.df[["time"]][ind]){ # if not at end of file && next minute is not in the data
			# col[0] = "time", the rest are NA TODO:add col[1] = "timestamp" as well
			NA_VALUE <- 0
			newRow <- c( currentTimeStr,rep(NA_VALUE,length(zz.df)-1) )
			cat( "\radding row at",newRow[1],"new length=",length(zz.df[[1]]),"current index=",ind,"filling until:",zz.df[["time"]][ind] )
			zz.df <- insertRow(zz.df,newRow,ind) # add the row
			ind <- ind+1 #move forward b/c of inserted row
			rowsInserted <- rowsInserted+1

			list[d,hr,m] <- nextMinute(d,hr,m) # move to the next minute
			#update current time string
			currentTimeStr <- formatTime('2012',START.mon,d,hr,m)
		}
		ind <- ind+1
		list[d,hr,m] <- nextMinute(d,hr,m)# move to the next minute
		
		#update current time string
		currentTimeStr <- formatTime('2012',START.mon,d,hr,m)
	}
	print (cat(rowsInserted," NA rows inserted. Resulting sample size:",length(zz.df[[1]]) ))

# NOTE: use something like write.csv(zz.df,paste(fname,"_feature",INDEX_OF_INTEREST,".csv",sep=""))
#       to save the dataframe

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

