\name{InterpolateLexisTriangles.simple}
\alias{InterpolateLexisTriangles.simple}

\title{
A function to break mortality hazards from Lexis squares into Lexis triangles an averaging technique.
}
\description{
the input matrix of Mx values is first expanded into a matrix \code{k} times larger in both directions. Each inner element is then replaced with the average of its neighbors in the un-averaged matrix. Triangles are then deduced by averaging the corresponding values in each kxk matrix representing an age/year.
}
\usage{
InterpolateLexisTriangles.simple(mxmat, k)
}

\arguments{
  \item{mxmat}{
a matrix of central death rates (Mx) in single years and single ages. This needs to be organized with years in columns and ages in rows. label the columns and rows with the years and ages! If extracting from the HMD, this is the item "Mx_1x1"
}
  \item{k}{
a whole number representing the expansion factor values higher than 5 make little sense. Good results can be obtained by comparing 2,3,4.
}
}
\details{
each cell of \code{mxmat} is replaced with a kxk matrix of cells with the same value. In a second expanded matrix, each non-edge cell value consists in a weighted average of itself and its 8 (king) neighbors in the original expanded matrix, where the weights are: 2, for the central value, 1 for period, cohort, and age neighbors and .5 for the two remaining off-corners. This becomes the expanded averaged matrix. To derive an upper triangle Mx estimate, the function takes the arithmetic average of the elements of each kxk sub-matrix corresponding to the upper triangle (1 in the cells only in the upper triangle, .5 on the diagonal and 0 in the cells only in the lower triangle), and vice versa for the lower triangle.
}
\value{
A list with two items
\item{LowerTriangles}{a matrix of the Mx values in lower triangles with years in columns and ages in rows.}
\item{UpperTriangles}{a matrix of the Mx values in upper triangles with years in columns and ages in rows.}
}
\author{
Tim Riffe
}
\section{Warning }{Just use this function to manipulate data for plotting it as a Lexis surface. This is not a serious proposal for how to split Mx values into triangles!, and results should not be published.
} 
\seealso{
Three other methods included in this package: \code{\link{InterpolateLexisTriangles.MortalitySmooth}}, \code{\link{InterpolateLexisTriangles.loess}}, \code{\link{InterpolateLexisTriangles.Dxprop}}
}
\examples{
# examples for simple splitting method
library(LexisSurface)
data(Mortality)
# read in data:
mxmat <- Mortality$mxmat
# years across columns, ages down rows
mxmat[1:5,1:5]

# where k is how big we expand the input matrix
# averaging is always with direct neighbors, so bigger k = less effect of averaging
Mx.simple2 <- InterpolateLexisTriangles.simple(mxmat,k = 2)
Mx.simple6 <- InterpolateLexisTriangles.simple(mxmat,k = 6)
is.list(Mx.simple2)
names(Mx.simple2);names(Mx.simple5)
# some better colors than the default:
mymxcols <- grDevices:::colorRampPalette(c("aquamarine","green","yellow","orange","red","purple4"),space = "rgb")
#since we want to compare, we use a common color reference table:
col.ref <- LexisColorRefTable(log(mxmat),ncolors=200)

# plot
windows(height=7,width=12)
par(mfrow=c(1,2),mar=c(4,2,4,6))
PlotLexisTriangles(Mx.simple2$UpperTriangles, Mx.simple2$LowerTriangles, col.ref = col.ref,
		leg.side = "right", leg.coords = c(2000, 10, 2003, 90))
title("neighbor-averaged, k=2")
PlotLexisTriangles(Mx.simple6$UpperTriangles, Mx.simple6$LowerTriangles, col.ref = col.ref, legend = FALSE)
title("neighbor-averaged, k=6")
# notice how the plot on the right is more granular/pixely: this is because the neighbor averaging
# was less effective due to a small grid (due to a larger expansion). k=3 or k=4 are usually good
# you may wish to try fiddling with the weights (w) inside the function, although this argument can't
# be passed on at this time. for instance, the two values of .5 could be replaced with 0 (and you'd have
# to adjust the denominator as well to add up properly...)
## The function is currently defined as
function(mxmat, k){
	
	Dim <- dim(mxmat)
	
	# internal function part 1
	expandmxmat <- function(mxmat,k=2){
		dim1 <- dim(mxmat)
		dim2 <- k*dim1
		mxmatk <- matrix(nrow=dim2[1],ncol=dim2[2])
		for (m in 1:dim1[1]){
			for (n in 1:dim1[2]){
				mxmatk[(m*k-k+1):(m*k),(n*k-k+1):(n*k)] <- mxmat[m,n]
			}
		}
		return(mxmatk)
	}
	
	# internal function part 2, weighted averaging
	avgmat <- function(mxmatk){
		mxmatk2 <- mxmatk
		for (m in 2:(nrow(mxmatk)-1)){
			for (n in 2:(ncol(mxmatk)-1)){
				mxmatk2[m,n] <- weighted.mean(mxmatk[(m-1):(m+1),(n-1):(n+1)],w=c(1,1,.5,1,2,1,.5,1,1))
			}
		}
		return(mxmatk2)
	}
	
	# we make a fine grid, granular at first
	mxmatk <- expandmxmat(mxmat=mxmat,k=k)
	
	# now we take a weighted average of neighboring cells, 
	# giving more weight to central value, period and cohort, less value to others
	mxmatk <- avgmat(mxmatk)

	# Define Upper and Lower triangle selection matrices
	# upper and lower might seem backwards. They are produced on the assumption that age data go down when in the 
	# matrix, but are plotted upward.also note, the cohort lines change direction!!
	# draw picturess too see that it makes sense.
	
	LTr <- UTr <- matrix(0,ncol=k,nrow=k)
	for (i in 1:k){
		LTr[1:(k-i+1),(k-i+1)] <- c(rep(1,k-i),.5)
		UTr[(k-i+1):k,(k-i+1)] <- c(.5,rep(1,i-1))
	}
	
	# define upper and lower triangle matrices
	UTVals <- LTVals <- matrix(nrow=Dim[1],ncol=Dim[2])
	denom <- sum(UTr)
	# we move a gs x gs sized box and average the values for triangles
	for (m in 1:Dim[1]){
		for (n in 1:Dim[2]){	
			# 1) select a gs*gs cell of the selected matrix & select the triangle values- determine
			# appropriate average value.
			UTVals[m,n] <- sum(mxmatk[(k*(m-1)+1):(k*m),(k*(n-1)+1):(k*n)] * UTr)/denom
			# repeat for lower triangle
			LTVals[m,n] <- sum(mxmatk[(k*(m-1)+1):(k*m),(k*(n-1)+1):(k*n)] * LTr)/denom
		}
	}
	rownames(UTVals) <- rownames(LTVals) <- rownames(mxmat)
	colnames(UTVals) <- colnames(LTVals) <- colnames(mxmat)
	return(list(LowerTriangles=LTVals,UpperTriangles=UTVals))
  }
}
