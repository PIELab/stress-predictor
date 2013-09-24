#! /usr/bin/Rscript
	args <- commandArgs(TRUE)	# arguments passed to script (unused here)

	pois.HMM.lalphabeta <- function (x ,m , lambda , gamma , delta = NULL ){
		if ( is.null ( delta ) ) delta <- solve ( t ( diag ( m ) - gamma +1) , rep (1 , m ) )
			n <- length ( x )
			lalpha <- lbeta <- matrix ( NA ,m , n )
			allprobs <- outer (x , lambda , dpois )
			foo <- delta * allprobs [1 ,]
			sumfoo <- sum ( foo )
			lscale <- log ( sumfoo )
			foo <- foo / sumfoo
			lalpha [ ,1] <- log ( foo ) + lscale
			for ( i in 2: n ) {
			foo <- foo %*% gamma * allprobs [i ,]
			sumfoo <- sum ( foo )
			lscale <- lscale + log ( sumfoo )
			foo <- foo / sumfoo
			lalpha [ , i ] <- log ( foo ) + lscale
		}
		lbeta [ , n ] <- rep (0 , m )
		foo <- rep (1/ m , m )
		lscale <- log ( m )
		for ( i in (n -1) :1) {
			foo <- gamma %*%( allprobs [ i +1 ,]* foo )
			lbeta [ , i ] <- log ( foo ) + lscale
			sumfoo <- sum ( foo )
			foo <- foo / sumfoo
			lscale <- lscale + log ( sumfoo )
		}
		list ( la = lalpha , lb = lbeta )
	}


#function definition from p245 Hidden Markov Models for Time Series An Introduction Using R by Walter Zucchini et al
	pois.HMM.state_prediction <-
	    function (x ,m , lambda , gamma , delta = NULL , H =1 ,...) {
		if ( is.null ( delta ) ) delta <- solve ( t ( diag ( m ) - gamma +1) , rep (1 , m ) )
		n   <- length ( x )
		fb  <- pois.HMM.lalphabeta (x ,m , lambda , gamma , delta = delta )
		la  <- fb$la
		c   <- max ( la [ , n ])
		llk <- c + log ( sum ( exp ( la [ , n ] - c ) ) )
		statepreds <- matrix ( NA , ncol =H , nrow = m )
		foo1 <- exp ( la [ , n ] - llk )
		foo2 <- diag ( m )
		for ( i in 1: H ) {
			foo2 <- foo2 %*% gamma
			statepreds [ , i ] <- foo1 %*% foo2
		}
		statepreds
	}

	#inputs:
	x <- 1
	m  <- 1
	lambda<-1
	gamma<-1

	#run function
	pred <- pois.HMM.state_prediction(x,m,lambda,gamma)
