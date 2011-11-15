LexisLegend <-
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
		abslabs <- 10^abslabs[(abslabs/.5) %% 2 == 0]
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

