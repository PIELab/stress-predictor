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
