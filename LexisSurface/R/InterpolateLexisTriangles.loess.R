InterpolateLexisTriangles.loess <-
function(mxmat, years, ages, age.widths, grid.int = .1, span = .003, ax){
	if (missing(span) || is.null(span)) { 
		span <- grid.int*2 
	}
	if (sum(as.numeric(colnames(mxmat)))>sum(as.numeric(rownames(mxmat)))){
		mxmat <- t(mxmat)
	}
	# impute widths, assume 1 if missing
	if (missing(age.widths)){
		age.widths <- rep(1,ncol(mxmat))
		cat("\nage.widths not specified, assuming 1 year age groups\n")
	}
	# impute ages if missing
	if (missing(ages)){
		ages <- as.numeric(colnames(mxmat))
		if (is.null(ages)){
			ages <- cumsum(age.widths)-age.widths
		}
	}
	# impute years if missing
	if (missing(years)){
		years <- as.numeric(rownames(mxmat))
		if (is.null(years)){
			years <- seq(from=1900,by=1,length=nrow(mxmat))
			cat("\nyears not specified, assumed single years, starting in 1900; age labels may be off.\n")
		}
	}
	
	# impute ax if missing 
	if ((missing(ax) || is.null(ax))) {
		ax <- c(.1*age.widths[1],.5*age.widths[2:length(age.widths)])
		if (ax[1] < grid.int){ 
			ax[1] <- 0 
		} else {
			ax[1] <- grid.int*floor(ax[1]/grid.int)
		}
	}
	
	ages.mid <- ages+ax
	gs <- 1/grid.int
	# put ages, years and z in long form:
	x 	<- c(rep(ages.mid,length(years)))
	y 	<- c(sort(rep(years,length(ages.mid))))

	# for some reason
	# loess.predict() only takes vectors named x and y as arguments. It's really strange. I've done testing
	# and confirmed that they must be called x and y. It also doesn't help to specify them as columns in a 
	# matrix 
	zlong <- c(t(mxmat))
	zlong[zlong==0] <- NA 
	# here we make the loess object
	z.loess <- stats:::loess(log(zlong) ~ x * y,span=span,na.action="na.omit") # this is where the long 'x' and 'y' enter
	# and we need new finer coordinates:
	ynew <- seq(from=min(years),to=max(years),by=grid.int)
	xnew <- seq(from=min(ages.mid),to=max(ages.mid),by=grid.int)
	NewGridMar <- list(x=xnew,y=ynew)
	# interpolate the loess object to intermittent points to get a smooth surface
	z.kji <- stats:::predict(z.loess,newdata=expand.grid(NewGridMar))
	# we fill in 0 with a linear extrapolation (no big deal since it's
	# just 1 grid.int usually. a very small delta makes the linear assumption OK	
	
	mx0 <- function(mxvals,ax1,grid.int){
		xvals <- seq(from=ax1,to=(grid.int*length(mxvals)+ax1-grid.int),by=grid.int)
		Line <- lm(mxvals~xvals)
		xnew <- seq(from=0, to = ax1-grid.int,by=grid.int)
		log.mx.out <- xnew*Line$coef[2]+Line$coef[1]
		return(log.mx.out)
	}
	
	if (ax[1]/grid.int > 0){
		newrows <- ceiling(ax[1]/grid.int)
		z.kji <- rbind(matrix(nrow=newrows,ncol=ncol(z.kji)),z.kji)
		z.kji[1:newrows,] <- apply(as.matrix(z.kji[(newrows+1):(newrows+6),]),2,mx0,ax1=ax[1],grid.int=grid.int)
	}		
	# Upper and Lower triangle selection matrices
	# upper and lower might seem backwards. They are produced on the assumption that age data go down when in the 
	# matrix, but are plotted upward. gs <-10. also note, the cohort lines change direction!!
	# draw picturess too see that it makes sense. gs <- 10
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

