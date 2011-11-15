\name{InterpolateLexisTriangles.MortalitySmooth}
\alias{InterpolateLexisTriangles.MortalitySmooth}
\title{
A function to break mortality hazards from Lexis squares into Lexis triangles using a smoothing technique.
}
\description{
This function takes two matrices of death counts and population exposures and splits them into upper and lower Lexis triangles of Mx values by using the \code{Mort2Dsmooth}. This works by smoothing the entire surface and predicting its value at very small intervals, then averaging over this tiny grid to arrive at individual upper and lower triangle values.
}
\usage{
InterpolateLexisTriangles.MortalitySmooth(Dx, Exp, years, ages, grid.int = 0.1, lambda = 0)
}
\arguments{
  \item{Dx}{
matrix of death counts with years in columns and ages in rows. Label the columns and rows with the years and ages! If extracting from the HMD, this is the item "Deaths_1x1"
}
  \item{Exp}{ 
matrix of population exposures with years in columns and ages in rows. Label the columns and rows with the years and ages! If extracting from the HMD, this is the item "Exposures_1x1"
}
  \item{years}{
vector containing the years covered by the data. These should match the column names of \code{Dx} and \code{Exp}.
}
  \item{ages}{
vector containing the ages covered by the data. These should match the row names of \code{Dx} and \code{Exp}.
}
  \item{grid.int}{
optional. The interval at which \code{predict.Mort2Dsmooth()} should calculate the smoothed surface. This needs to be a single number evenly divisible into 1, and it will be used in both the x and y directions. default = .1; yields results similar to .2. Smaller values will use large amounts of memory.
}
  \item{lambda}{
optional. The smoothing parameter used by \code{Mort2Dsmooth()}. Default = 0.
}
}
\details{
The MortalitySmooth method works by using p-splines to smooth the mortality surface, using log(expsure) as an offset, then calling the \code{predict.Mort2Dsmooth()} function to predict the surface values on a finer grid of ages and years, say at intervals of .1 (default). Each age-year is now a 10x10 matrix. To derive an upper triangle Mx estimate, the function takes the arithmetic average of the elements of this 10x10 matrix corresponding to the upper triangle (with the diagonal weighted as .5 for both triangles), and vice versa for the lower triangle.
}
\value{
A list with two items
\item{LowerTriangles}{a matrix of the Mx values in lower triangles with years in columns and ages in rows.}
\item{UpperTriangles}{a matrix of the Mx values in upper triangles with years in columns and ages in rows.}
}
\references{
Carlo G Camarda (2009) MortalitySmooth: Smoothing Poisson counts with P-splines, R package version 1.0. available at: http://CRAN.R-project.org/package=MortalitySmooth
}
\author{
Tim Riffe
}
\note{
Even with \code{lambda} set to 0, results may be too smooth. This is a matter of judgment, and results may be compared with those from other interpolation functions. Then function may return an error if the data contain too many NAs, as is sometimes the case in the upper ages of HMD data up to age 110+. In that case, take a smaller age range.
}

\section{Warning }{Just use this function to manipulate data for plotting it as a Lexis surface. This is not a serious proposal for how to split Mx values into triangles!, and results should not be published.
} 
\seealso{
Three other methods included in this package: \code{\link{InterpolateLexisTriangles.simple}}, \code{\link{InterpolateLexisTriangles.loess}}, \code{\link{InterpolateLexisTriangles.Dxprop}}
}\examples{
library(LexisSurface)
# do install.packages("MortalitySmooth") if necessary
library(MortalitySmooth)

# read in data:
Dxmat <- Mortality$Dxmat
Expmat <- Mortality$Expmat

# dimensions must match
all.equal(dim(Dxmat),dim(Expmat)) #TRUE

# years across columns, ages down rows
# output format for HMDget(bla bla, format=4)
Dxmat[1:5,1:5]
Expmat[1:5,1:5]

# split to triangles
Mx.MS.0 <- InterpolateLexisTriangles.MortalitySmooth(Dx=Dxmat,Exp=Expmat,lambda=0)
Mx.MS.5 <- InterpolateLexisTriangles.MortalitySmooth(Dx=Dxmat,Exp=Expmat,lambda=5)

# some better colors than the default:
mymxcols <- grDevices:::colorRampPalette(c("aquamarine","green","yellow","orange","red","purple4"),space = "rgb")
# we want exactly comparable colors
col.ref <- LexisColorRefTable(log(Dxmat/Expmat),colorramp=mymxcols,ncolors=200)

# plot
windows(height=7,width=12)
par(mfrow=c(1,2),mar=c(4,2,4,6))
PlotLexisTriangles(Mx.MS.0$UpperTriangles, Mx.MS.0$LowerTriangles, col.ref = col.ref,
		leg.side="right",leg.coords=c(2000,10,2003,90))
title("p-spline smoothed, lambda = 0")
PlotLexisTriangles(Mx.MS.5$UpperTriangles, Mx.MS.5$LowerTriangles, col.ref = col.ref,legend = FALSE)
title("p-spline smoothed, lambda = 5")
## The function is currently defined as
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
}
