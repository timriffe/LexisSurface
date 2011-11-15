\name{LexisLegend}
\alias{LexisLegend}
\title{
produces a color-strip legend used internally by \code{PlotLexisTriangles}
}
\description{
This function is used internally by \code{PlotLexisTriangles()} for plotting Lexis mortality surfaces, but it is made available separately so that this kind of legend can be added to the margin of any plot.
}
\usage{
LexisLegend(col.ref, abs.labs = TRUE, log.labs = TRUE, side = "bottom", coords, log = TRUE)
}

\arguments{
  \item{col.ref}{
A color reference table of the form produced by \code{LexisColorRefTable()}: A matrix with 3 columns and a row for each color. The first column are the colors from \code{colorramp(ncolors)}. The second column are evenly spaced values from the minimum to the maximum of the range of the argument \code{mxvals} specified to make \code{col.ref}. The third column is the \code{exp()} of the second column. if mortality data have been logged for plotting, the the second column represents a logged scale and the third column represents the  corresponding absolute values. 
}
  \item{abs.labs}{
logical. Default = TRUE. Should axis ticks and labels be placed on the left/bottom (depending if vertical or horizontal) of the color strip representing the 3rd column of col.ref? In the case of logged surface values, this would mean axis values for the original unlogged data. In the case of a not-logged surface, this argument is ignored.
}
  \item{log.labs}{
logical. Default = TRUE. Should axis ticks and labels be placed on the right/bottom (depending if vertical or horizontal) of the color strip representing the 2nd column of col.ref? In the case of logged surface values, this would mean axis values for the logged data specified to \code{LexisColorRefTable()}. In the case of a not-logged surface (\code{log=FALSE}), this argument is a misnomer, and \code{pretty} ticks will be found for the data in the second column.
}
  \item{side}{
which margin should the legend be placed in? One of \code{c("bottom", "left", "top", "right")} or \code{c(1, 2, 3, 4)}, respectively. Default = \code{"bottom"}. There are default coordinates for these that sort-of adjust for the data, but the default placement is not always good, in which case you should also specify the argument \code{coords} (below). 
}
  \item{coords}{
(optional) a vector of the form \code{c(xleft, ybottom, xright, ytop)} specifying the sides of the color strip. x coordinates will be in years, continuing out from the plot itself, and y coordinates are in ages, on the same scale as the plot itself. If you are having difficulty placing the legend in a good spot (sorry), then you can execute \code{par(mar=c(bla,bla,bla,bla))} prior to running the \code{PlotLexisTriangles()} command to make some extra space.  
}
  \item{log}{
logical. Default = TRUE. Have the data been logged. In this case the default is to put ticks on both sides of the color strip indicating how to interpret the colors on a natural log scale and how to interpret them on an absolute scale. If \code{log=FALSE} then only values from the 2nd column of \code{col.ref} are taken to find \code{pretty} ticks and labels.
}
}
\details{
The legend is placed roughly in the middle of the margin, running nearly the entire length of the plotted Lexis surface. Exact sizing and placement must be specified with the argument \code{coords}.
}
\value{
a legend is added to the plot. Nothing is returned.
}
\author{
Tim Riffe
}
\examples{
library(LexisSurface)
data(Mortality)
data(Fertility)
# some mx values
mxmat <- Mortality$mxmat
Fxupper <- Fertility$UpperTriangles
Fxlower <- Fertility$LowerTriangles

# a funky color ramp
mymxcols <- grDevices:::colorRampPalette(c("white","green","blue","purple","hotpink"),space = "rgb")
col.ref <- LexisColorRefTable(log(mxmat),colorramp=mymxcols)
# (PlotLexisTriangles also plots normal lexis square data)

# here the defult right side placement is terrible:
PlotLexisTriangles(mxmat,col.ref=col.ref,leg.side="right",main="Some legend options")

# re-specify with leg.coords (passed to legend arguments as a'coords')
PlotLexisTriangles(mxmat,col.ref=col.ref,leg.side="right",leg.coords=c(2000,10,2003,90),
		main="Some legend options")
# on the bottom, no log labels:
LexisLegend(col.ref=col.ref,side="bottom",log.labs=FALSE,coords=c(1910,-14,1980,-11))
# pretty labels still not perfect- overlap on left...

mxFxcols <- grDevices:::colorRampPalette(c("white","blue","green","yellow","orange"),space = "rgb")
graphics.off()
windows(height=6,width=10)
# specify log = FALSE
PlotLexisTriangles(Fxupper,Fxlower,colorramp=mxFxcols,leg.side="bottom",
		main="a legend for not-logged values",log=F)
# change coords
graphics.off()
windows(height=6,width=10)
PlotLexisTriangles(Fxupper,Fxlower,colorramp=mxFxcols,leg.side="bottom",
		leg.coords=c(1911,-2,1996,0),log=F,main="a legend for not-logged values")


## The function is currently defined as
function(col.ref, abs.labs = TRUE, log.labs = TRUE, side = "bottom", coords, log = TRUE){
	# basic parameters:
	Plt <- par("plt") # plot region as fraction fo figure
	Xaxp <- par("xaxp")[1:2] # default tick extremes x
	Yaxp <- par("yaxp")[1:2] # default tick extremes y
	N <- nrow(col.ref)
	# where to put pretty marks for log values:
	logvals <-  pretty(col.ref[,2],n=5)[pretty(col.ref[,2],n=5) > min(col.ref[,2]) &  pretty(col.ref[,2],n=5) < max(col.ref[,2])]
	# absolute values are a bit tougher
	if (log == TRUE){
		absvals <- c()
		abslabs <- (pretty(log(col.ref[,3],base=10),n=4))
		abslabs <- 10^abslabs[(abslabs/.5) \%\% 2 == 0]
		abslabs <- abslabs[(abslabs > 1) == FALSE]	
		for (i in 1:length(abslabs)){
			absvals <- c(absvals,col.ref[,3][which.min(abs(col.ref[,3]-abslabs[i]))])
		}
		if (max(absvals)<.9){
			if (abslabs[length(abslabs)]==abslabs[(length(abslabs)-1)]){
				abslabs <- abslabs[-length(abslabs)]
				absvals <- absvals[-length(absvals)]
			}
		}
		if (min(absvals)/min(abslabs)>2){
			abslabs[1] <- round(min(absvals),digits=(length(absvals)-1))
		}
		# hopefully the abolute labels will come out pretty!
	}
	# decide left right or bottom: 
	if (missing(side)){
		side <- c("left","right","bottom","top")[which.max(c(Plt[1],(1-Plt[2]),Plt[3],(1-Plt[4])))]
	}
	
	# each side needs to use diff coordinate defaults
	if (side == "bottom" || side == 1){
		coords <- if (missing(coords)){
			c(Xaxp[1],Yaxp[1]-.2*diff(Yaxp),Xaxp[2],Yaxp[1]-.15*diff(Yaxp))
		} else {coords}
		xall <- seq(coords[1],coords[3],length=(N+1))
		xleft <- xall[1:N]
		xright <- xall[2:(N+1)]
		ybottom <- coords[2]
		ytop <- coords[4]
		# plot color strip
		for (i in 1:N){
			rect(xleft[i], ybottom, xright[i], ytop,border="transparent",col=col.ref[i,1],xpd=T)
		}
		# match values to current plot
		xs <- (xleft+xright)/2
		# plot log ticks and labels
		if (log.labs==TRUE){
			for (i in 1:length(logvals)){
				xpos <- xs[which.min(abs(col.ref[,2]-logvals[i]))]
				segments(xpos,ytop,xpos,ytop+.3*(ytop-ybottom),xpd=TRUE)
				text(xpos,ytop+.3*(ytop-ybottom),logvals[i],xpd=TRUE,cex=.8,adj=c(0,-.3))
			}
		}
		# plot absolute ticks and labels
		if (log == TRUE){
			if (abs.labs==TRUE){
				for (i in 1:length(absvals)){
					xpos <- xs[which.min(abs(col.ref[,3]-absvals[i]))]
					segments(xpos,ybottom-.3*(ytop-ybottom),xpos,ybottom,xpd=TRUE)
					text(xpos,ybottom-.3*(ytop-ybottom),abslabs[i],xpd=TRUE,cex=.8,adj=c(0,1))
				}
			}
			if (log.labs==TRUE){
				text(min(xleft),ytop+.2*(ytop-ybottom),"ln",pos=2,xpd=TRUE)
			}
			text(min(xleft),ybottom-.2*(ytop-ybottom),"absolute",pos=2,xpd=TRUE)
		}
	
	}
	
	if (side == "top" || side == 3){
		
		coords <- if (missing(coords)){
					c(Xaxp[1],Yaxp[2]+.08*diff(Yaxp),Xaxp[2],Yaxp[2]+.13*diff(Yaxp))
				} else {coords}
		xall <- seq(coords[1],coords[3],length=(N+1))
		xleft <- xall[1:N]
		xright <- xall[2:(N+1)]
		ybottom <- coords[2]
		ytop <- coords[4]
		# plot color strip
		for (i in 1:N){
			rect(xleft[i], ybottom, xright[i], ytop,border="transparent",col=col.ref[i,1],xpd=T)
		}
		# match values to current plot
		xs <- (xleft+xright)/2
		# plot log ticks and labels
		if (log.labs==TRUE){
			for (i in 1:length(logvals)){
				xpos <- xs[which.min(abs(col.ref[,2]-logvals[i]))]
				segments(xpos,ytop,xpos,ytop+.3*(ytop-ybottom),xpd=TRUE)
				text(xpos,ytop+.3*(ytop-ybottom),logvals[i],xpd=TRUE,cex=.8,adj=c(0,-.3))
			}
			
		}
		# plot absolute ticks and labels
		if (log == TRUE){
			if (abs.labs==TRUE){
				for (i in 1:length(absvals)){
					xpos <- xs[which.min(abs(col.ref[,3]-absvals[i]))]
					segments(xpos,ybottom-.3*(ytop-ybottom),xpos,ybottom,xpd=TRUE)
					text(xpos,ybottom-.3*(ytop-ybottom),abslabs[i],xpd=TRUE,cex=.8,adj=c(0,1))
				}
			}
			if (log.labs==TRUE){
				text(min(xleft),ytop+.2*(ytop-ybottom),"ln",pos=2,xpd=TRUE)
			}
			text(min(xleft),ybottom-.2*(ytop-ybottom),"absolute",pos=2,xpd=TRUE)
		}
	}
	
	if (side=="left" || side == 2){
		
		coords <- if (missing(coords)){
				  	c(Xaxp[1]-.1*diff(Xaxp),Yaxp[1],Xaxp[1]-.05*diff(Xaxp),Yaxp[2])
					} else {coords}

		left <- coords[1]
		right <- coords[3]
		yall <- seq(coords[2],coords[4],length=(N+1))
		ybottom <- yall[1:N]
		ytop <- yall[2:(N+1)]

		# plot color strip
		for (i in 1:N){
			rect(left, ybottom[i], right, ytop[i],border="transparent",col=col.ref[i,1],xpd=T)
		}
		# match values to current plot
		ys <- (ybottom+ytop)/2
		# plot log ticks and labels
	
		if (log.labs==TRUE){
			for (i in 1:length(logvals)){
				ypos <- ys[which.min(abs(col.ref[,2]-logvals[i]))]
				segments(right,ypos,right-.3*(left-right),ypos,xpd=TRUE)
				text(right-.3*(left-right),ypos,logvals[i],xpd=TRUE,cex=.8,adj=c(-.3,.2))
			}
		}
		# plot absolute ticks and labels
		if (log == TRUE){
			if (abs.labs==TRUE){
				for (i in 1:length(absvals)){
					ypos <- ys[which.min(abs(col.ref[,3]-absvals[i]))]
					segments(left,ypos,left+.3*(left-right),ypos,xpd=TRUE)
					text(left+.3*(left-right),ypos,abslabs[i],xpd=TRUE,cex=.8,adj=c(1,.2))
				}
			}
			if (log.labs==TRUE){
				text(right-(left-right),min(ys),"ln",pos=1,xpd=TRUE)
			}
			text(left+(left-right),min(ys)-diff(Yaxp)/30,"abs",pos=1,xpd=TRUE)
		}
	}
	
	if (side=="right" || side == 4){
		
		coords <- if (missing(coords)){
					c(Xaxp[2]+.05*diff(Xaxp),Yaxp[1],Xaxp[2]+.1*diff(Xaxp),Yaxp[2])
				} else {coords}
		
		left <- coords[1]
		right <- coords[3]
		yall <- seq(coords[2],coords[4],length=(N+1))
		ybottom <- yall[1:N]
		ytop <- yall[2:(N+1)]
		# plot color strip
		for (i in 1:N){
			rect(left, ybottom[i], right, ytop[i],border="transparent",col=col.ref[i,1],xpd=T)
		}
		# match values to current plot
		ys <- (ybottom+ytop)/2
		# plot log ticks and labels
		if (log.labs==TRUE){
			for (i in 1:length(logvals)){
				ypos <- ys[which.min(abs(col.ref[,2]-logvals[i]))]
				segments(right,ypos,right-.3*(left-right),ypos,xpd=TRUE)
				text(right-.3*(left-right),ypos,logvals[i],xpd=TRUE,cex=.8,adj=c(-.3,.2))
			}
		}
		# plot absolute ticks and labels
		if (log == TRUE){	
			if (abs.labs==TRUE){
				for (i in 1:length(absvals)){
					ypos <- ys[which.min(abs(col.ref[,3]-absvals[i]))]
					segments(left,ypos,left+.3*(left-right),ypos,xpd=TRUE)
					text(left+.3*(left-right),ypos,abslabs[i],xpd=TRUE,cex=.8,adj=c(1,.2))
				}
			}
			if (log.labs==TRUE){
				text(right-(left-right),min(ys),"ln",pos=1,xpd=TRUE)
			}
			text(left+(left-right),min(ys)-diff(Yaxp)/30,"abs",pos=1,xpd=TRUE)
		}
	}

	}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
