\name{LexisColorRefTable}
\alias{LexisColorRefTable}
\title{
function to produce a color reference table used internally by \code{PlotLexisTriangles} to decide the color for each upper and lower triangle.
}
\description{
This function is used internally by \code{PlotLexisTriangles()} for plotting Lexis surfaces, but it is made available separately so that the same color reference table may be supplied to different calls to \code{PlotLexisTriangles()}, making colors comparable between different Lexis surfaces from different populations. The object produced by this function is also an argument in \code{LexisLegend()}.
}
\usage{
LexisColorRefTable(mxvals, colorramp, ncolors = 200)
}

\arguments{
  \item{mxvals}{
the Mx values for which colors are requested. The function does no internal logging of data, so if the legend ought to reflect a demographic surface that has been logged (like a hazard surface), then you should log \code{mxvals} in the argument. Unlike the name suggests, this argument could be any vector of values, logged or not, Mx or not.
}
  \item{colorramp}{
a color ramp function of the kind returned by \code{grDevices:::colorRampPalette()}. If none is supplied, the default colors are produced by the function \code{grDevices:::colorRampPalette(c("#53D837","yellow","orange","red"),space = "rgb",bias=1.2)}, which is a gradient from green to yellow to orange to red. Including more colors between (that do not match this gradient) would increase the contrast of plots. See examples for other color ramps.
}
  \item{ncolors}{
number of colors that the function should interpolate. Default = 200. The colors from \code{colorramp(ncolors)} will be assigned to evenly space intervals from the minimum to the maximum of the logged Mx values.
}
}
\details{
Colors are assigned to individual Lexis triangles in \code{PlotLexisTriangles()} by choosing the color that minimizes the difference from the Mx value in question. The table therefore does not directly represent intervals, but rather central interval values. In the legend arguments, in the case of \code{log=TRUE}, the legend will take the second column to find \code{pretty} logged values and labels for ticks, and the third to find absolute ticks and labels. If in legend specification \code{log=FALSE}, then only values from the second column are taken, and the third column is ignored.
}
\value{
a matrix with 3 columns and \code{ncolors} rows. The first column are the colors from \code{colorramp(ncolors)}. The second column are evenly spaced values from the minimum to the maximum of code{mxvals}. The third column consists in the \code{exp()} of the second column. 
}
\author{
Tim Riffe
}
\examples{
library(LexisSurface)
data(Mortality)
# some mx values
mxmat <- Mortality$mxmat

# basic code, since ncolors defaults to 200 and colorramp has a default
col.ref <- LexisColorRefTable(log(mxmat))
# if you make your own, it needs this form:
head(col.ref)

plot(NULL,type="n",xlim=c(0,10),ylim=c(0,200),axes=FALSE,
		xlab="",ylab="",main="this is roughly how the color strip legend uses col.ref")
for (i in 1:200){
	rect(0,(i-1),10,i,col=col.ref[i,1],border="transparent")
}
ys <- seq(20,200,20)
axis(2,pos=0,at=ys,labels=round(col.ref[ys,2],digits=2))
axis(4,pos=10,at=ys,labels=round(col.ref[ys,3],digits=4))
# except the legend function would give pretty axis ticks


# you can also experiment with your own color ramp.
# I'll welcome any suggestions to incude as prefab options in the package.
# haven't thought much on this
mymxcols <- grDevices:::colorRampPalette(c("green","yellow","blue","red","white"),space = "rgb")
# (PlotLexisTriangles also plots normal lexis square data)
PlotLexisTriangles(mxmat,colorramp=mymxcols)
title("Defining your own color ramp to use")

## The function is currently defined as
function(mxvals, colorramp, ncolors=200){
	# make sure values enter as logged
	mxvals[is.infinite(mxvals)] <- NA
	mxrange <- range(mxvals,na.rm=T)
	
	# default color ramp
	if (missing(colorramp)){
		colorramp <- grDevices:::colorRampPalette(c("#53D837","yellow","orange","red"),space = "rgb",bias=1.2)
	}
	
	# define reference table
	col.ref <- data.frame(matrix(0,nrow=ncolors,ncol=3))
	# 1) 200 evenly spaced colors over whole range of possible colors
	col.ref[,1] <- colorramp(ncolors)		
	# 2) 200 evenly spaced log mx values, from min to max within selected matrix
	col.ref[,2] <- seq(from=mxrange[1],to=mxrange[2],length=ncolors)
	# 3) the real mx values that correspond with the logged values in column 2
	col.ref[,3] <- exp(col.ref[,2])
	colnames(col.ref) <- c("color","log mx","mx")
	return(col.ref)
  }
}
