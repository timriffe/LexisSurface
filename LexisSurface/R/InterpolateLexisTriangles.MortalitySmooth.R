InterpolateLexisTriangles.MortalitySmooth <-
function(Dx, Exp, years, ages, grid.int = .1, lambda = 0){
	require(MortalitySmooth)
	# impute widths, assume 1 if missing
	age.widths <- rep(1,nrow(Dx))
	
	# impute ages if missing
	if (missing(ages)){
		ages <- as.numeric(rownames(Dx))
		if (is.null(ages)){
			ages <- cumsum(age.widths)-age.widths
		}
	}
	# impute years if missing
	if (missing(years)){
		years <- as.numeric(colnames(Dx))
		if (is.null(years)){
			years <- seq(from=1900,by=1,length=ncol(Dx))
			cat("\nyears not specified, assumed single years, starting in 1900; age labels may be off.\n")
		}
	}
	# default lambdas (smoothness parameter. smaller = rougher, larger = smoother)
	if (missing(lambda)){
		lambda <- c(.1,.1)
	}
	if (length(lambda)==1){
		lambda <- c(lambda,lambda)
	}
	# gs is for grid size, i.e. how many new small grids equal one big one
	gs <- 1/grid.int
	
	# fit is the Mort2Dsmooth object, passed to
	fit <- Mort2Dsmooth(x=ages, y=years, Z=Dx, offset=log(Exp),method=3,lambda=c(.1,.1))
	# compute log-death rates for each calendar month and calendar ages
	newyears <- seq(years[1], years[length(years)], by=grid.int)
	newages <- seq(ages[1], ages[length(ages)], by=grid.int)
	newdata <- list(x=newages, y=newyears)
	z.kji <- predict(fit, newdata=newdata)
	
	
	LTr <- UTr <- matrix(0,ncol=gs,nrow=gs)
	for (i in 1:gs){
		LTr[1:(gs-i+1),(gs-i+1)] <- c(rep(1,gs-i),.5)
		UTr[(gs-i+1):gs,(gs-i+1)] <- c(.5,rep(1,i-1))
	}
	ages.tri <- ages[-length(ages)]
	years.tri  <- years[-length(years)]
	# we skim down to exactly the dimensions within which we have full information, and label 
	z.sel <- z.kji[1:(length(ages.tri)*gs),1:(length(years.tri)*gs)]
	
	# now, we need 2 overlapping matrices UT = upper triangle, LT = lower triangle.
	# likewise, we determine colors as we go from a custom color table for the values
	# present in the surface. Since each triangle has 1 color, the color matrices have
	# the same dimensions.
	UTVals <- LTVals <- matrix(nrow=length(ages.tri),ncol=length(years.tri))
	denom <- sum(UTr)
	# we move a gs x gs sized box and average the values for triangles
	for (n in 1:length(years.tri)){
		for (m in 1:length(ages.tri)){	
			# 1) select a gs*gs cell of the selected matrix & select the triangle values- determine
			# appropriate average value.
			UTVals[m,n] <- sum(z.sel[(gs*(m-1)+1):(gs*m),(gs*(n-1)+1):(gs*n)] * UTr)/denom
			# repeat for lower triangle
			LTVals[m,n] <- sum(z.sel[(gs*(m-1)+1):(gs*m),(gs*(n-1)+1):(gs*n)] * LTr)/denom
		}
	}
	UTVals <- exp(UTVals)
	LTVals <- exp(LTVals)
	rownames(UTVals) <- rownames(LTVals) <- ages.tri
	colnames(UTVals) <- colnames(LTVals) <- years.tri
	return(list(LowerTriangles=LTVals,UpperTriangles=UTVals))
}

