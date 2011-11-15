\name{InterpolateLexisTriangles.Dxprop}
\alias{InterpolateLexisTriangles.Dxprop}

\title{
A function to break mortality hazards from Lexis squares into Lexis triangles using information on deaths in Lexis triangles.
}
\description{
If you have death counts data organized into Lexis triangles (as available from the Human Mortality Database), this function splits M(x) data into Lexis triangles by simply using the known proportions of deaths in upper and lower triangles. This is an imperfect method because exposures do not adjust, but it provides good detail for surface visualization.
}
\usage{
InterpolateLexisTriangles.Dxprop(mxmat, Deaths_lexis)
}

\arguments{
  \item{mxmat}{
a matrix of central death rates (Mx) in single years and single ages. This needs to be organized with years in columns and ages in rows. If extracting from the HMD, this is the item "Mx_1x1"
}
  \item{Deaths_lexis}{
a matrix of death counts by Lexis triangles. Must be organized with years in columns and ages in rows, cohorts nested within ages with lower triangles first, followed by upper triangles. Be careful that this doesn't include triangles from open age groups, as these are lone triangles. This matrix should have exactly twice the number of rows as \code{mxmat}. If extracting from the HMD, this is the item "Deaths_lexis"
}
}
\details{
The Mx squares are split such that the ratio of upper to lower triangles is equal to the ratio of upper to lower triangles in the Lexis deaths input, and the the arithmetic average of the Mx triangles is equal to the Mx input Mx square value. Since there is no adjustment for exposure between the triangles that make up a square, this method will tend to overestimate Mx in lower triangles and underestimate Mx in upper triangles.
}
\value{
A list with two items
\item{LowerTriangles}{a matrix of the Mx values in lower triangles with years in columns and ages in rows.}
\item{UpperTriangles}{a matrix of the Mx values in upper triangles with years in columns and ages in rows.}

}
\references{
Human Mortality Database. University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de. 
}
\author{
Tim Riffe
}
\note{
This function is in development and may change in the future to take exposure in Lexis squares as an argument in order to provide a better estimate.
}

\section{Warning }{Just use this function to manipulate data for plotting it as a Lexis surface. This is not a serious proposal for how to split Mx values into triangles!, and results should not be published.} 

\seealso{
Three other methods included in this package: \code{\link{InterpolateLexisTriangles.simple}}, \code{\link{InterpolateLexisTriangles.MortalitySmooth}}, \code{\link{InterpolateLexisTriangles.loess}}
}

\examples{
library(LexisSurface)
data(Mortality)
lexismat 	<- Mortality$lexismat
mxmat 		<- Mortality$mxmat

# compare dimensions:
dim(mxmat)
dim(lexismat)
# notice that lexismat has twice as many rows as mxmat.
# be careful when selecting data from the HMD not to include open age groups (lone triangles)


# years across columns, ages down rows
lexismat[1:5,1:5]
mxmat[1:5,1:5]

# convert to Triangle Mx:
Mx.prop <- InterpolateLexisTriangles.Dxprop(mxmat,Deaths_lexis=lexismat)
is.list(Mx.prop) 	# TRUE
names(Mx.prop) 		# LowerTriangles UpperTriangles

# output is a list with 2 elements, a matrix for upper and a matrix for lower Mx triangles
LowerTriangles <- Mx.prop$LowerTriangles
UpperTriangles <- Mx.prop$UpperTriangles

# same basic format
LowerTriangles[1:5,1:5]
UpperTriangles[1:5,1:5]

# take a look
mymxcols <- grDevices:::colorRampPalette(c("aquamarine","green","yellow","orange","red","purple4"),space = "rgb")
PlotLexisTriangles(UpperTriangles,LowerTriangles,main="SWE Mortality Surface, 1900-1989 (HMD)",colorramp=mymxcols,leg.side="right",leg.coords=c(2000,10,2003,90))

## The function is currently defined as
function(mxmat, Deaths_lexis){
	Dim <- dim(mxmat)
	# define upper and lower triangle matrices
	UTVals <- LTVals <- matrix(nrow=Dim[1],ncol=Dim[2])
	# 
	# now we take upper 2 * (upper Dx / (upper Dx+lower Dx)) as the Mx upper tri adjustment of the lexis square
	# and 				2 * (lower Dx / (upper Dx+lower Dx)) as the Mx lower tri adjustment of the lexis square
	# multiply by 2 because Mx is a hazard. 
	# The arithmetic avg of the two triangles is equal the original Mx
	for (m in 1:Dim[1]){
		for (n in 1:Dim[2]){
			UTVals[m,n] <- 2*mxmat[m,n]*(Deaths_lexis[(m*2),n]/sum(Deaths_lexis[(m*2-1):(m*2),n]))
			LTVals[m,n] <- 2*mxmat[m,n]*(Deaths_lexis[(m*2-1),n]/sum(Deaths_lexis[(m*2-1):(m*2),n]))
		}
	}
	
	rownames(UTVals) <- rownames(LTVals) <- rownames(mxmat)
	colnames(UTVals) <- colnames(LTVals) <- colnames(mxmat)
	return(list(LowerTriangles=LTVals,UpperTriangles=UTVals))
  }
}
