\name{InterpolateLexisTriangles.loess}
\alias{InterpolateLexisTriangles.loess}
\title{
A function to break mortality hazards from Lexis squares into Lexis triangles  using a smoothing technique.
}
\description{
This function takes a matrix of Mx values in Lexis squares and splits them into upper and lower Lexis triangles by using a loess smoother function. This works by smoothing the entire surface and predicting its value at very small intervals, then averaging over this tiny grid to arrive at individual upper and lower triangle values.
}
\usage{
InterpolateLexisTriangles.loess(mxmat, years, ages, age.widths, grid.int = 0.1, span = 0.003, ax)
}

\arguments{
  \item{mxmat}{
a matrix of central death rates (Mx) in single years and single ages. This needs to be organized with years in columns and ages in rows. label the columns and rows with the years and ages! If extracting from the HMD, this is the item "Mx_1x1"
}
  \item{years}{
vector containing the years covered by the data. These should match the column names of \code{mxmat}.
}
  \item{ages}{
vector containing the ages covered by the data. These should match the row names of \code{mxmat}.
}
  \item{age.widths}{
vector containing the age interval widths (redundant, sorry). Assumed \code{rep(1,nrow(mxmat))} if missing.
}
  \item{grid.int}{
optional. The interval at which \code{predict.loess()} should calculate the smoothed surface. This needs to be a single number evenly divisible into 1, and it will be used in both the x and y directions. default = .1; yields results similar to .2. Smaller values will use large amounts of memory.
}
  \item{span}{
optional. The smoothing parameter used by \code{loess()}. Default = 0.003. Results are better with as low a value as the data will allow. More years and ages allow for smaller \code{span}.
}
  \item{ax}{
optional. The average years lived within an age interval by those dying within that interval, for each age. Default uses interval midpoints (.5), except the first year of life, which is assumed .1. Values smaller than .1 entail very large matrices and may overwhelm your system memory.
}
}
\details{
The loess-smoothing method works by first smoothing the 2d surface made of Lexis square (\code{mxmat}), then calling the \code{predict.loess()} function to predict the surface values on a finer grid of ages and years, say at intervals of .1 (default). Each age-year is now a 10x10 matrix. To derive an upper triangle Mx estimate, the function takes the arithmetic average of the elements of this 10x10 matrix corresponding to the upper triangle (with the diagonal weighted as .5 for both triangles), and vice versa for the lower triangle.
}
\value{
A list with two items
\item{LowerTriangles}{a matrix of the Mx values in lower triangles with years in columns and ages in rows.}
\item{UpperTriangles}{a matrix of the Mx values in upper triangles with years in columns and ages in rows.}
}
\author{
Tim Riffe
}
\note{
Even with \code{span} set as small as your data will allow, results may be too smooth. This is a matter of judgement, and results may be compared with those from other interpolation functions.
}
\section{Warning }{Just use this function to manipulate data for plotting it as a Lexis surface. This is not a serious proposal for how to split Mx values into triangles!, and results should not be published.} 

\seealso{
Three other methods included in this package: \code{\link{InterpolateLexisTriangles.simple}}, \code{\link{InterpolateLexisTriangles.MortalitySmooth}}, \code{\link{InterpolateLexisTriangles.Dxprop}}
}

\examples{
library(LexisSurface)
data(Mortality)
mxmat <- Mortality$mxmat
Mx.loess.002 <- InterpolateLexisTriangles.loess(mxmat,age.widths=rep(1,100),span=.002)
Mx.loess.01 <- InterpolateLexisTriangles.loess(mxmat,age.widths=rep(1,100),span=.01)
is.list(Mx.loess.002) 	# TRUE
names(Mx.loess.002) 	# "LowerTriangles" "UpperTriangles"
# let's get some better colors:
mymxcols <- grDevices:::colorRampPalette(c("aquamarine","green","yellow","orange","red","purple4"),space = "rgb")
# plot (takes a minute)
col.ref <- LexisColorRefTable(log(Dxmat/Expmat),colorramp=mymxcols,ncolors=200)
windows(height=7,width=12)
par(mfrow=c(1,2))
PlotLexisTriangles(Mx.loess.002$UpperTriangles,Mx.loess.002$LowerTriangles,col.ref=col.ref)
title("loess smoothed, span = .002")
PlotLexisTriangles(Mx.loess.01$UpperTriangles,Mx.loess.01$LowerTriangles,col.ref=col.ref)
title("loess smoothed, span = .01")
# notice a big difference. lower span = better definition.

## The function is currently defined as
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
}

