LexisColorRefTable <-
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

