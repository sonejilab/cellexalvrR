
#' Using kmeans to group the drc data
#' for the usability of the log file the genes need to be ordered.
#' The heatmap needs to show these clusters of genes. And to identify the right number of clusters I need and elbow analysis.
#' https://www.icsi.berkeley.edu/icsi/node/4806
#' Finding a Kneedle in a Haystack: Detecting Knee Points in System Behavior
#' Satopaa, V.., Albrecht J., Irwin D., & Raghavan B., 2010
#' @name optGroupCountKmeans
#' @docType methods
#' @description calculate the optimal amount of kmeans grooups for a data matrix
#' @param dat the data matrix
#' @param k number of clusters to testdefault=1:20
#' @title identify the optimal numer of clusters for a matrix
#' @export 
#if ( ! isGeneric('optGroupCountKmeans') ){
setGeneric('optGroupCountKmeans', ## Name
	function ( dat, k=1:20 ) { 
		standardGeneric('optGroupCountKmeans')
	}
)
#}


#' @rdname optGroupCountKmeans
setMethod('optGroupCountKmeans', signature = c ('matrix'),
	definition = function ( dat, k=1:20 ) {


	## this needs to be reproducible!!
	set.seed(123581347)
	points = unlist(lapply( k, function(i) {  #total within-cluster sum of square (WSS)
		sum(stats::kmeans( dat, i )$withinss)
	} ) )
	## create a linear function between start: 1;points[1] and end: length(points);points[length(points)]
	slope <- diff(c(points[1], points[length(points)] ))/diff(c(1,length(points)))
	intercept <- points[1]-slope
	f = function(x) { x * slope + intercept }
	der = unlist(lapply( 1:length(points) ,function(x) { points[x] - f(x) }))
	der = der- min(der)
	## And find the max length of this value
	## here more groups is likely better than less
	max ( which(der < max(der) / 1e+10) )
} )