#' This function is a hack of the official vioplot function 
#' to allow multiple colors of the violines.
#' @name vioplot
#' @docType methods
#' @description the official vioplot function patched to allow different colors for the different violines.
#' @param datas a list with all data to plot
#' @param range see orig vioplot funciton default= 1.5
#' @param h see orig vioplot funciton default= NULL
#' @param ylim see orig vioplot funciton default= NULL
#' @param names see orig vioplot funciton default= NULL
#' @param horizontal please look up in the original vioplot documentation
#' @param col please look up in the original vioplot documentation
#' @param border please look up in the original vioplot documentation
#' @param lty please look up in the original vioplot documentation
#' @param lwd please look up in the original vioplot documentation
#' @param rectCol please look up in the original vioplot documentation
#' @param colMed please look up in the original vioplot documentation
#' @param pchMed please look up in the original vioplot documentation
#' @param at please look up in the original vioplot documentation
#' @param add please look up in the original vioplot documentation
#' @param wex please look up in the original vioplot documentation
#' @param drawRect please look up in the original vioplot documentation
#' @param main please look up in the original vioplot documentation
#' @param cex.axis please look up in the original vioplot documentation
#' @param neg please look up in the original vioplot documentation
#' @title description of function vioplot
setGeneric('vioplot', ## Name
		function ( datas, range = 1.5, h = NULL, ylim = NULL, names = NULL, 
				horizontal = FALSE, col = 'magenta', border = 'black', lty = 1, 
				lwd = 1, rectCol = 'black', colMed = 'white', pchMed = 19, 
				at, add = FALSE, wex = 1, drawRect = TRUE, main=NULL, cex.axis=1, neg=NULL) { ## Argumente der generischen Funktion
			standardGeneric('vioplot') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)

#' @rdname vioplot
setMethod('vioplot', signature = c ('list'),
		definition = function (datas, range = 1.5, h = NULL, ylim = NULL, names = NULL, 
		horizontal = FALSE, col = 'magenta', border = 'black', lty = 1, 
		lwd = 1, rectCol = 'black', colMed = 'white', pchMed = 19, 
		at, add = FALSE, wex = 1, drawRect = TRUE, main=NULL, cex.axis=1, neg=NULL) 
{
	n <- length(datas)
	if (missing(at)) 
		at <- 1:n
	upper <- vector(mode = 'numeric', length = n)
	lower <- vector(mode = 'numeric', length = n)
	q1 <- vector(mode = 'numeric', length = n)
	q3 <- vector(mode = 'numeric', length = n)
	med <- vector(mode = 'numeric', length = n)
	base <- vector(mode = 'list', length = n)
	height <- vector(mode = 'list', length = n)
	baserange <- c(Inf, -Inf)
	args <- list(display = 'none')
	if (!(is.null(h))) 
		args <- c(args, h = h)
	names.2 <- NULL
	for (i in 1:n) {
		data <- datas[[i]][ is.na(datas[[i]]) ==F ]
		if ( ! is.null(neg)) {
			names.2 <- c ( names.2, paste( length(which( datas[[i]] != neg )),"/",length(data),sep='') )
		}else {
			names.2 <- c ( names.2, paste( length(data),"/",length(datas[[i]]),sep='') )
		}
		if ( length(data) == 0) {
			data <- c(0)
		}
		data.min <- min(data)
		data.max <- max(data)
		
		if ( data.min == data.max ) {
			next;
		}
		q1[i] <- quantile(data, 0.25)
		q3[i] <- quantile(data, 0.75)
		med[i] <- median(data)
		
		iqd <- q3[i] - q1[i]
		upper[i] <- min(q3[i] + range * iqd, data.max)
		lower[i] <- max(q1[i] - range * iqd, data.min)
		est.xlim <- c(min(lower[i], data.min), max(upper[i], 
						data.max))
		smout <- do.call('sm.density', c(list(data, xlim = est.xlim), 
						args))
		hscale <- 0.4/max(smout$estimate) * wex
		base[[i]] <- smout$eval.points
		height[[i]] <- smout$estimate * hscale
		t <- range(base[[i]])
		baserange[1] <- min(baserange[1], t[1])
		baserange[2] <- max(baserange[2], t[2])
	}
	if (!add) {
		xlim <- if (n == 1) 
					at + c(-0.5, 0.5)
				else range(at) + min(diff(at))/2 * c(-1, 1)
		if (is.null(ylim)) {
			ylim <- baserange
		}
	}
	if ( ! is.null(names)) {
		label <- names
	}
	else if (is.null(names.2)) {
		label <- 1:n
	}
	else {
		label <- names.2
	}
	boxwidth <- 0.05 * wex
	if ( length( col ) == 1 ){
		col = rep(col,n)
	}
	if ( length(col ) < n ) {
		stop(paste(length(col),'colors are too view to color',n,'data sets'))
	} 
	if (!add) 
		plot.new()
	if (!horizontal) {
		if (!add) {
			plot.window(xlim = xlim, ylim = ylim)
			axis(2, cex.axis=cex.axis)
			axis(1, at = at, labels = label, cex.axis=cex.axis)
		}
		box()
		for (i in 1:n) {
			polygon(c(at[i] - height[[i]], rev(at[i] + height[[i]])), 
					c(base[[i]], rev(base[[i]])), col = col[i], border = border, 
					lty = lty, lwd = lwd)
			if (drawRect) {
				lines(at[c(i, i)], c(lower[i], upper[i]), lwd = lwd, 
						lty = lty)
				rect(at[i] - boxwidth/2, q1[i], at[i] + boxwidth/2, 
						q3[i], col = rectCol)
				points(at[i], med[i], pch = pchMed, col = colMed)
			}
			else{
				lines(at[c(i, i)], c(lower[i], upper[i]), lwd = lwd, 
						lty = lty)
				lines(c(at[i]- boxwidth/2, at[i] + boxwidth/2), c(lower[i], lower[i]), lwd = lwd, 
						lty = lty)
				lines( c(at[i]- boxwidth/2, at[i] + boxwidth/2), c(upper[i], upper[i]), lwd = lwd, 
						lty = lty)
				points(at[i], med[i], pch = pchMed, col = colMed, cex=2)
			}
		}
		
	}
	else {
		if (!add) {
			plot.window(xlim = ylim, ylim = xlim)
			axis(1,cex.axis =cex.axis)
			axis(2, at = at, labels = label, cex.axis=cex.axis)
		}
		box()
		for (i in 1:n) {
			polygon(c(base[[i]], rev(base[[i]])), c(at[i] - height[[i]], 
							rev(at[i] + height[[i]])), col = col[i], border = border, 
					lty = lty, lwd = lwd)
			if (drawRect) {
				lines(c(lower[i], upper[i]), at[c(i, i)], lwd = lwd, 
						lty = lty)
				rect(q1[i], at[i] - boxwidth/2, q3[i], at[i] + 
								boxwidth/2, col = rectCol)
				points(med[i], at[i], pch = pchMed, col = colMed)
			}
		}
	}
	if ( ! is.null(main) ){
		title( main, cex.main = 2)
	}
	invisible(list(upper = upper, lower = lower, median = med, 
					q1 = q1, q3 = q3))
}

)

