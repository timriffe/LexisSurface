PlotLexisTriangles <-
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
		ageticks <- ages.tri[ages.tri %% lex.line.int == 0]
		yearticks <- years.tri[years.tri %% lex.line.int == 0]
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
	ageticks <- ages.tri[ages.tri %% 10 == 0]
	yearticks <- years.tri[years.tri %% 10 == 0]
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

