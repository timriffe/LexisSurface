\name{LexisSurface-package}
\alias{LexisSurface-package}
\alias{LexisSurface}
\docType{package}
\title{
plot logged Lexis mortality surfaces as triangles!}
\description{
This package contains a function for plotting logged mortality surfaces consisting in Lexis triangles. There are also functions for four ad-hoc methods of splitting Lexis squares into Lexis triangles.}
\details{
\tabular{ll}{
Package: \tab LexisSurface\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2011-07-28\cr
License: \tab General Public\cr
LazyLoad: \tab yes\cr
}
}
\author{
Tim Riffe

Maintainer: Tim Riffe <tim.riffe@gmail.com>
}
\references{
Human Mortality Database. University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded on 28-7-2012).

Human Fertility Database. Max Planck Institute for Demographic Research (Germany) and Vienna Institute of Demography (Austria). Available at www.humanfertility.org (data downloaded on 28-7-2012).
}
\keyword{ package }
\keyword{ hplot }

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

}
