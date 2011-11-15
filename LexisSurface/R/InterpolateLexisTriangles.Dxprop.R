InterpolateLexisTriangles.Dxprop <-
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

