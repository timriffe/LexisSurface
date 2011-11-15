InterpolateLexisTriangles.simple <-
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

