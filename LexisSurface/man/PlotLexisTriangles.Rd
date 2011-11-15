\name{PlotLexisTriangles}
\alias{PlotLexisTriangles}
\title{
plot mortality surfaces of logged Mx values as Lexis triangles!
}
\description{
If you have data for upper and lower triangles of a Lexis surface, these can be plotted as such, rather than using a gridded surface. A color strip legend is also placed. Lexis reference lines are optionally plotted as well.
}
\usage{
PlotLexisTriangles(UpperTriangles, LowerTriangles, col.ref, colorramp, ncolors, log = TRUE, axes = TRUE, gen.labs = TRUE, lex.lines = TRUE, lex.line.col = "#A3A3A350", lex.line.int = 10, new = TRUE, legend = TRUE, leg.side = "bottom", leg.coords, leg.abs.labs = TRUE, leg.log.labs = TRUE, ...)
}
\arguments{
  \item{UpperTriangles}{
matrix of the upper triangle values, with years across columns and ages going down rows.
}
  \item{LowerTriangles}{
matrix of the lower triangle values, with years across columns and ages going down rows.
}
  \item{col.ref}{
(optional) color reference table, such as that produced by \code{LexisColorRefTable}. Otherwise one is made internally.
}
  \item{colorramp}{
(optional) color ramp function of the kind returned by \code{grDevices:::colorRampPalette()}. If none is supplied, the default colors are produced by the function \code{grDevices:::colorRampPalette(c("#53D837","yellow","orange","red"),space = "rgb",bias=1.2)}, which is a gradient from green to yellow to orange to red. Including more colors between (that do not match this gradient) would increase the contrast of plots. If \code{col.ref} has been specified, then you do not need to specify \code{colorramp}.
}
  \item{ncolors}{
number of color levels that the function should interpolate. The colors from \code{colorramp(ncolors)} will be assigned to evenly space intervals from the minimum to the maximum of the triangle values given (evenly spaced over the range of their log, if \code{lof=TRUE}).
}
  \item{log}{
logical. Default = TRUE. Should the triangle values be logged before plotting? This is typical for mortality hazard surfaces. There is no automatic detection for whether values have been logged. If you specify Mx data, and \code{log=FALSE}, then the plot will still work, but it may all be the same color. 
}
  \item{axes}{
logical. Default = TRUE. Should axes labels be placed on the bottom and left axes for years and ages, respectively? If so, these are in 10 year intervals.
}
  \item{gen.labs}{
logical. Default = TRUE. In the case that the youngest age is not 0, should generation labels be placed on the top axes, corresponding with the Lexis cohort reference lines? If the lowest age is 0, then this argument is ignored, and year labels on the bottom are simply rotated 45 degrees so that they serve for both years and generations.
}
  \item{lex.lines}{
logical. Default = TRUE. Should Lexis triangle lines be plotted on top of the surface (at wide intervals) as a reference?
}
  \item{lex.line.col}{
color for Lexis triangle lines. Default = "#A3A3A350", a grey color at 50\% opacity.
}
  \item{lex.line.int}{
interval size in years (ages) for Lexis triangle reference lines. Default = 10 (big triangles). 5 also looks good.
}
  \item{new}{
logical. Default = TRUE. Should a new plot be started, erasing the prior window (if open) (TRUE), or do you just want to add Lexis triangles to a plot that has already been started (FALSE). if \code{FALSE}, be sure to set the x limits and y limits ahead of time to match the year and age ranges of the data.
}
  \item{legend}{
logical. Default = TRUE. Should a color strip legend be added to the margins of the plot?
}
  \item{leg.side}{
which margin should the legend be placed in? One of \code{c("bottom", "left", "top", "right")} or \code{c(1, 2, 3, 4)}, respectively. Default = \code{"bottom"}.
}
  \item{leg.coords}{
(optional) a vector of the form \code{c(xleft, ybottom, xright, ytop)} specifying the sides of the color strip. x coordinates will be in years, and y coordinates are in ages, both on the same scale as the plot. If you are having difficulty placing the legend in a good spot (sorry), then you can execute \code{par(mar=c(bla,bla,bla,bla))} prior to running the \code{PlotLexisTriangles()} command to make some extra space. 
}
  \item{leg.abs.labs}{
logical. Default = TRUE. Should axis ticks and labels by placed for absolute Mx values? (only relevant if \code{log=TRUE}).
}
  \item{leg.log.labs}{
logical. Default = TRUE. Should axis ticks and labels by placed for logged Mx values (if \code{log=TRUE})? if \code{log=FALSE}, and \code{leg.log.labs} is true, then legend ticks and labels will be spaced \code{pretty}-like over the (not) logged data range.
}
  \item{\dots}{
additional parameters to be passed to the initial call to \code{plot()}. Only recognized if \code{new=TRUE}. Other parameters can be set by using \code{par()} prior to calling this function.
}
}
\details{
For more control over plotting parameters, you can open a plot window prior to calling this function and set its parameters (possibly specifying \code{new=FALSE}). Just be sure to specify \code{asp=1} for a 1:1 aspect ratio on the plot. This is important for Lexis plots, as it guarantees a 45 degree angle for cohort lines.
}
\value{
Nothing is returned. A Lexis surface plot is plotted in a graphics device.
}
\author{
Tim Riffe
}
\note{
There is no automatic detection of whether data have been logged prior to plotting, so be sure to be careful specifying this argument. If you are comparing different surfaces of the same variable, then you can specify a common color table to use, with the function \code{LexisColorRefTable()}, and pass this to \code{LexisSurface()} using the argument \code{col.ref}. Any R color ramp can be used, custom or default.

If you want to have more control over the plot than can be had by passing parameters to \code{plot()} via the \code{...} argument, first open a graphics device and set \code{par()} arguments, then open a blank plot window with \code{plot(NULL,type="n",xlim=c(your year range),ylim=c(your age range), etc)} prior to calling this function, and specify \code{new = FALSE} in the arguments.
}

\seealso{
See Also the large number of R functions for plotting matrix or gridded surfaces, such as \code{image} in base or \code{image.plot} in the "fields" package.
}
\examples{
library(LexisSurface)
# use install.packages("MortalitySmooth") if necessary
library(MortalitySmooth)
data(Mortality)
# First some mortality examples:
# read in data:
mxmat 		<- Mortality$mxmat
Dxmat 		<- Mortality$Dxmat
Expmat 		<- Mortality$Expmat
lexismat 	<- Mortality$lexismat
# the initial Mx values in Lexis squares:
mxmat[1:6,1:6]
# Deaths in Lexis square, used by InterpolateLexisTriangles.MortalitySmooth:
Dxmat[1:6,1:6]
# Expsoures in Lexis square, used by InterpolateLexisTriangles.MortalitySmooth:
Expmat[1:6,1:6]
# Death counts in Lexis triangles, used by InterpolateLexisTriangles.Dxprop()
lexismat[1:6,1:6]
# (be attentive to the format of lexismat if you intend to use this method with data from a different source)
dim(lexismat) 	# lexismat has twice as many rows since there are 2 triangles per square.
dim(mxmat)		# you may need to shave off a row or two if you take straight from HMD

# we'll demonsrate splitting them with the 4 (questionable and untested) methods provided:
Mx.loess.002 <- InterpolateLexisTriangles.loess(mxmat,age.widths=rep(1,100),span=.002)
Mx.MortSmth.0 <- InterpolateLexisTriangles.MortalitySmooth(Dxmat,Expmat,lambda=0)
Mx.prop <- InterpolateLexisTriangles.Dxprop(mxmat,Deaths_lexis=lexismat)
Mx.simple <- InterpolateLexisTriangles.simple(mxmat,k=3) # different results with k=4

# some better colors than the default:
mymxcols <- grDevices:::colorRampPalette(c("aquamarine","green","yellow","orange","red","purple4"),space = "rgb")
# now, since we want to compare, we use a common color reference table
# and specify it each time.
col.ref <- LexisColorRefTable(log(mxmat),colorramp=mymxcols,ncolors=200)

# now we plot:
windows(height=7,width=12)
par(mfrow=c(1,2),mar=c(3,3,3,6))
PlotLexisTriangles(Mx.loess.002[[2]],Mx.loess.002[[1]],col.ref=col.ref,
		leg.side="right",leg.coords=c(2000,10,2003,90),
		main = "loess smoothed, with as little smoothing as possible")
PlotLexisTriangles(Mx.MortSmth.0[[2]],Mx.MortSmth.0[[1]],col.ref=col.ref,
		main = "p-spline, with smoothing parameter = 0",legend=FALSE)


# compare simple with triangles split using HMD death triangles
windows(height=7,width=12)
par(mfrow=c(1,2),mar=c(3,3,3,6))
PlotLexisTriangles(Mx.simple[[2]],Mx.simple[[1]],col.ref=col.ref,
		leg.side="right",leg.coords=c(2000,10,2003,90),
		main="simple averaging, k = 3")
PlotLexisTriangles(Mx.prop[[2]],Mx.prop[[1]],col.ref=col.ref,
		main="split using death triangles from HMD",legend=FALSE)



# some other options:
graphics.off()
par(mar=c(3,6,3,3))
PlotLexisTriangles(Mx.prop$UpperTriangles[1:20,50:70],Mx.prop$LowerTriangles[1:20,50:70],
		col.ref=col.ref,lex.line.int = 5,axes=FALSE,main="young males, Sweden 1949-1970 (HMD)",
		leg.side=2,leg.coords=c(1946,2,1947,17),leg.log.labs=FALSE)
axis(1,pos=0,at=seq(par("xaxp")[1],par("xaxp")[2],5),padj=-.7)
axis(2,pos=1949,at=seq(0,20,5),labels=seq(0,20,5))

# don't care about comparing? for more definition from the current plot, let it make its own color table:
# keep colorramp:
graphics.off()
par(mar=c(3,6,3,3))
PlotLexisTriangles(Mx.prop$UpperTriangles[1:20,50:70],Mx.prop$LowerTriangles[1:20,50:70],
		colorramp=mymxcols,lex.line.int = 5,axes=FALSE,main="young males, Sweden 1949-1970 (HMD)",
		leg.side=2,leg.coords=c(1946,2,1947,17),leg.log.labs=FALSE)
axis(1,pos=0,at=seq(par("xaxp")[1],par("xaxp")[2],5),padj=-.7)
axis(2,pos=1949,at=seq(0,20,5),labels=seq(0,20,5))

# closer look at Spanish flu
# we specify col.ref, making this a simple zoom-in of the larger plot
# with the exact same colors
graphics.off()
windows(height=10,width=5)
par(mar=c(4,4,4,4))
PlotLexisTriangles(Mx.prop[[2]][1:40,17:21],Mx.prop[[1]][1:40,17:21],
		col.ref=col.ref,lex.lines=FALSE,axes=FALSE,leg.side="left",
		leg.coords=c(1910,5,1911,35),
		main="Spanish flu, Swedish males, HMD")
axis(2,pos=1916,at=seq(0,40,5),labels=F)
text(1916,seq(0,40,5),seq(0,40,5),pos=2)
# label some aspects:
arrows(1916,-5,1918,0,xpd=TRUE)
arrows(1921,-5,1919,0,xpd=TRUE)
text(1916,-5,"1918",pos=2,xpd=TRUE)
text(1921,-5,"1919",pos=4,xpd=TRUE)
arrows(1922,32,1919.25,33.75,xpd=TRUE)
text(1921,32,"Effects visible",pos=4,xpd=TRUE)
text(1921,30.5,"visible into the",pos=4,xpd=TRUE)
text(1921,29,"1st 1/2 of 1919",pos=4,xpd=TRUE)

#######
# again, if we don't care about comparing with other plots:
# here we just specify the color ramp, and colors are spread over
# the range of data in the present plot.
# closer look at Spanish flu
graphics.off()
windows(height=10,width=5)
par(mar=c(4,4,4,4))
PlotLexisTriangles(Mx.prop[[2]][1:40,17:21],Mx.prop[[1]][1:40,17:21],
		colorramp=mymxcols,lex.lines=FALSE,axes=FALSE,leg.side="left",
		leg.coords=c(1910,5,1911,35),
		main="Spanish flu, Swedish males, HMD")
axis(2,pos=1916,at=seq(0,40,5),labels=F)
text(1916,seq(0,40,5),seq(0,40,5),pos=2)
# label some aspects:
arrows(1916,-5,1918,0,xpd=TRUE)
arrows(1921,-5,1919,0,xpd=TRUE)
text(1916,-5,"1918",pos=2,xpd=TRUE)
text(1921,-5,"1919",pos=4,xpd=TRUE)
arrows(1922,32,1919.25,33.75,xpd=TRUE)
text(1921,32,"Effects visible",pos=4,xpd=TRUE)
text(1921,30.5,"visible into the",pos=4,xpd=TRUE)
text(1921,29,"1st 1/2 of 1919",pos=4,xpd=TRUE)


#######################################
# a non-logged surface:
# here Fertility rates have been made using births and exposures in the HMD,
# already both available in Lexis triangles:
data(Fertility)
mxFxcols <- grDevices:::colorRampPalette(c("white","blue","green","yellow","orange"),space = "rgb")
graphics.off()
windows(height=6,width=10)
# specify log = FALSE, and the data will be plotted as-is
PlotLexisTriangles(Fertility$UpperTriangles,Fertility$LowerTriangles,colorramp=mxFxcols,
		leg.side="bottom",main="APC fertility rates, Sweden 1891-2008 (HFD)",log=F)
# notice we don't have to specify anything special for the legend labels, per the defaults when
# log=F
# from what I understand there really was a baby-boom in 1920... it's not likely a data error.

## The function is currently defined as
function(UpperTriangles, LowerTriangles, col.ref, colorramp, ncolors, log = TRUE, 
		axes = TRUE, gen.labs=TRUE, lex.lines = TRUE, lex.line.col = "#A3A3A350", lex.line.int = 10, new = TRUE, 
		legend = TRUE, leg.side="bottom", leg.coords, leg.abs.labs = TRUE, leg.log.labs = TRUE, ...){
	
	##################################################################################
	# simple checks
	##################################################################################
	# if both are specified, we have proper triangles, need to make sure compatible
	if (missing(UpperTriangles)==FALSE & missing(LowerTriangles)==FALSE){
		if (any(dim(UpperTriangles) != dim(LowerTriangles))){
			stop("UpperTrianlges and LowerTriangles inputs must have the same dimensions")
		}
		if (any(colnames(UpperTriangles) != colnames(LowerTriangles))){
			stop("UpperTrianlges and LowerTriangles inputs must cover the same years")
		}
		if (any(rownames(UpperTriangles) != rownames(LowerTriangles))){
			stop("UpperTrianlges and LowerTriangles inputs must cover the same years")
		}
	}
	##################################################################################
	# if one or the other is specified, we duplicate to have two triangle objects (redundant)
	# that way it also plots lexis squares.
	if (missing(UpperTriangles)==FALSE & missing(LowerTriangles)==TRUE){
		LowerTriangles <- UpperTriangles
	}
	if (missing(LowerTriangles)==FALSE & missing(UpperTriangles)==TRUE){
		UpperTriangles <- LowerTriangles
	}
	##################################################################################
	# if not logged, we log it, simple criterion to check: non-logged mortality data are positive
	if (log == TRUE){
		LowerTriangles <- log(LowerTriangles)
		UpperTriangles <- log(UpperTriangles)
	}
	##################################################################################	
	# make sure no infinites (in case non-logged was 0)
	remove.infinite <- function(x){
		if (length(which(is.infinite(x))) > 0){
			x[is.infinite(x)] <- NA
		}
		return(x)
	}
	
	UpperTriangles <- remove.infinite(UpperTriangles)
	LowerTriangles <- remove.infinite(LowerTriangles)
	
	##################################################################################
	# get info for plotting
	##################################################################################
	# these are used to determine dimensions, ticks, labels
	years.tri 	<- as.numeric(colnames(LowerTriangles))
	ages.tri 	<- as.numeric(rownames(LowerTriangles))
	
	##################################################################################
	# define default col.ref if missing
	if (missing(col.ref)){
		if (missing(ncolors)){
			ncolors <- 200
		}
		if (missing(colorramp)){
			colorramp <- grDevices:::colorRampPalette(c("#53D837", "yellow", "orange", "red"), space = "rgb", bias = 1.2)
		}
		# we specify log = FALSE in either case
		col.ref <- LexisColorRefTable(mxvals = c(c(LowerTriangles), c(UpperTriangles)), colorramp = colorramp, ncolors = ncolors)
	}
	##################################################################################
	# make color matrices, a color for each triangle, stored in 2 objects
	UTCols <- LTCols <- matrix(nrow = length(ages.tri), ncol = length(years.tri))
	# assign color to each triangle in UpperTriangels and Lower Triangles
	for (i in 1:length(years.tri)){
		for (j in 1:length(ages.tri)){
			# 2) which color corresponds with the minimum residual?- insert into color matrix
			Res <- ((col.ref[, 2] - UpperTriangles[j, i])^2)^.5
			if (all(is.na(Res) == FALSE)) {
				UTCols[j,i] <- col.ref[which.min(Res), 1]
			} else { UTCols[j, i] <- "transparent"}
			# repeat for lower triangle
			Res <- ((col.ref[, 2]-LowerTriangles[j, i])^2)^.5
			if (all(is.na(Res) == FALSE)) {
				LTCols[j, i] <- col.ref[which.min(Res),1]
			} else { LTCols[j, i] <- "transparent"}
		}
	}
	
	##################################################################################
	# the basic plotting code:
	##################################################################################
	# if new==FALSE, then just polygons are plotted, assuming a device is already active and has the 
	# appropriate limits
	if (new==TRUE){
		plot(NULL, type = "n", xlim = range(years.tri), ylim = range(ages.tri), asp = 1, axes = FALSE, xlab="", ylab="", ...)
	}
	#plot(NULL, type = "n", xlim = range(years.tri), ylim = range(ages.tri), asp = 1, axes = FALSE, xlab="", ylab="")
	##################################################################################
	# each polygon draws a triangle whose fill color comes from the corresponding color matrix LTcols or UTcols
	for (n in 1:length(years.tri)){
		for (m in 1:length(ages.tri)){
			# upper triangle
			polygon(x = c(years.tri[n], years.tri[n] + 1, years.tri[n]), y = c(ages.tri[m], ages.tri[m] + 1,ages.tri[m] + 1), border = NA, col = UTCols[m, n])
			# lower triangle
			polygon(x = c(years.tri[n], years.tri[n] + 1, years.tri[n]+1), y = c(ages.tri[m], ages.tri[m], ages.tri[m] + 1), border = NA, col = LTCols[m, n])
		}
	}
	##################################################################################
	# optional lexis lines plotting, every "lex.line.int" ages/years. default = 10; 5 also good
	if (lex.lines==T){
		# lex.line.col="pink"
		# lex.line.int=10
		# triangle size can be set in argument "lex.line.int"
		ageticks <- ages.tri[ages.tri \%\% lex.line.int == 0]
		yearticks <- years.tri[years.tri \%\% lex.line.int == 0]
		rect(min(years.tri), min(ages.tri), max(years.tri) + 1, max(ages.tri) + 1, lwd = .5, border = lex.line.col)
		# vertical
		segments(yearticks, rep(min(ages.tri),length(yearticks)), yearticks,rep(max(ages.tri) + 1,length(yearticks)), lwd = .5, col = lex.line.col)
		# horizontal
		segments(rep(min(years.tri), length(ageticks)), ageticks,rep(max(years.tri) + 1, length(ageticks)), ageticks,lwd = .5,col = lex.line.col)
		# cohort lines, rough but effective
		Nclines <- length(ageticks)+length(yearticks)+2
		cstart <- max(yearticks)
		for (i in 1:100){
			x <- ((cstart-(i-1)*lex.line.int)+100):((cstart-(i-1)*lex.line.int)+500)
			y <- 0:(length(x)-1)
			xy <- cbind(x,y)
			xy <- xy[(xy[,1] < min(years.tri))==FALSE,]
			if (length(xy) > 2){
				xy <- xy[(xy[,1] > (max(years.tri)+1))==FALSE,]
			}
			if (length(xy) > 2){
				xy <- xy[(xy[,2] < min(ages.tri))==FALSE,]
			}
			if (length(xy) > 2){
				xy <- xy[(xy[,2] > (max(ages.tri)+1))==FALSE,]
			}
			if (length(xy) > 2){
				segments(xy[1,1],xy[1,2],xy[nrow(xy),1],xy[nrow(xy),2], lwd = .5, col = lex.line.col)
			}
		}		
	}
	##################################################################################
	# optional axes plotting, every 10 ages, years.
	ageticks <- ages.tri[ages.tri \%\% 10 == 0]
	yearticks <- years.tri[years.tri \%\% 10 == 0]
	if (axes == TRUE){
		rect(min(years.tri), min(ages.tri), max(years.tri) + 1, max(ages.tri) + 1)
		if (min(ages.tri)==0){
			axis(1, pos = min(ages.tri), at = yearticks, tck = -0.01, labels = FALSE)
			text(yearticks-3, min(ages.tri) - 2, yearticks, srt = 45, pos = 1, xpd = TRUE)
		} else {
			axis(1,pos = min(ages.tri))
			if (gen.labs == TRUE){
				# not thoroughly tested... placement might be off is some cases. err
				segments(pretty(yearticks)+((max(ages.tri)+1)-max(ageticks)),(max(ages.tri)+1),(pretty(yearticks)+.5)+((max(ages.tri)+1)-max(ageticks)),(max(ages.tri)+1.5))
				text(pretty(yearticks)+((max(ages.tri)+1)-max(ageticks)),(max(ages.tri)+1.5),pretty(yearticks)-max(ages.tri)-1+((max(ages.tri)+1)-max(ageticks)),pos=3)
			}
		}
		
		axis(2, pos=min(years.tri), at=ageticks, labels = FALSE, tck = -0.01)
		text(min(years.tri), ageticks, ageticks, pos=2, xpd = TRUE)
	}
	if (legend==TRUE){
		LexisLegend(col.ref = col.ref, abs.labs = leg.abs.labs, log.labs = leg.log.labs, side = leg.side, coords = leg.coords, log = log)
	}
  }
}
\keyword{ hplot }

