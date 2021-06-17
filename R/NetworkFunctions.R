#' @name cormat2df
#' @docType methods
#' @description  Converts a pairwise correlation matrix for data.frame
#' @param cors A square matrix of pairwise measures
#' @title convert correlation matrix to data frame (intern)
#' @keywords network construction
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('cormat2df', ## Name
	function (cors) {
		standardGeneric('cormat2df')
	}
)
#}


#' @rdname cormat2df
setMethod('cormat2df', signature = c ('matrix'),definition = function (cors) {

    ut <- upper.tri(cors)
    df.out <- data.frame(
        row = rownames(cors)[row(cors)[ut]],
        column = rownames(cors)[col(cors)[ut]],
        cor  = (cors)[ut]
    )
    df.out
})

#' This is the R function that calculates the TF networks shown in the VR environment.
#' 
#' @name make.cellexalvr.network
#' @docType methods
#' @description  Creates a network from selected groups for selected genes
#' @param cellexalObj cellexalvrR object
#' @param cellidfile a CellexalVR selection file
#' @param outpath the outpath
#' @param cutoff.ggm The cutoff for the correlation (default = 0.1)
#' @param exprFract which fraction of cells needs to express a gene to be included in the analysis (default 0.01)
#' @param top.n.inter get only the n top interations default=130
#' @param method the used method c( "rho.p","pcor" )
#' @title VR exposed function to create networks
#' @keywords network construction
#' @export 
#if ( ! isGeneric('make.cellexalvr.network') ){
setGeneric('make.cellexalvr.network', ## Name
	function (cellexalObj, cellidfile,outpath, cutoff.ggm=0.1,
        exprFract = 0.1, top.n.inter=130,method=c("rho.p","pcor")) {
		standardGeneric('make.cellexalvr.network')
	}
)
#}


#' @rdname make.cellexalvr.network
setMethod('make.cellexalvr.network', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, cellidfile,outpath, cutoff.ggm=0.1, 
        exprFract = 0.1, top.n.inter=130,method=c("rho.p","pcor")) {

		if ( !file.exists(outpath)) {
			dir.create( outpath ,  recursive = TRUE)
		}

	datadir <- cellexalObj@outpath
    #data <- cellexalObj@data
	cellexalObj <- userGrouping(cellexalObj, cellidfile) #function definition in file 'userGrouping.R'

	message( paste( "the cellexal object drc names:", paste( collapse= ", ", names(cellexalObj@drc))))
	
	#checkVRfiles( cellexalObj, datadir)
	## cut loc to only include TFs
	if ( is.na( match('TFs', colnames(cellexalObj@meta.gene)))) {
		cellexalObj = useInbuiltGOIlists(cellexalObj, 'TFs') #function definition in file 'useInbuiltGOIlists.R'
	}

	loc <- onlyGOIs( cellexalObj, 'TFs' ) #function definition in file 'onlyGOIs.R'

	## kick the not groupoed samples out of the loc object
	loc <- reduceTo (loc, what='col', to=colnames(cellexalObj@data)[- #function definition in file 'reduceTo.R'
							which(is.na(cellexalObj@userGroups[,cellexalObj@usedObj$lastGroup]))
			] )
    if ( exprFract > 1) {
        exprFract = .1
    }
    message( paste("We have", nrow(loc@data), "genes remaining after TF and fracExpr cuts") )
    if ( nrow(loc@data) < 10 ) {
        return ( make.cellexalvr.network( cellexalObj, cellidfile,outpath, cutoff.ggm / 10, exprFract, top.n.inter,method ) )
    }
    ## at some time we had a problem in the creeation of order column names:
    possible = c( paste(cellexalObj@usedObj$lastGroup, c(' order','.order'), sep=""))
    gname = possible[which(!is.na(match(possible, colnames(loc@userGroups))))]
	loc <- reorderSamples ( loc, gname ) #function definition in file 'reorder.obj.R'

	info <- groupingInfo( loc ) #function definition in file 'groupingInfo.R'
	if ( info@drc == 'unknown' || is.null( info@drc) ) {
		## just assume the user selected from graph 1
		## better than breaking
		info@drc = names(loc@drc)[1]
	}
	grps <- as.vector(unique(info@grouping))

    data <- loc@data
    req.graph <- info@drc

    grp.tabs <- NULL
    avg.drc.coods <- NULL
    #layout.tabs <- NULL
    if ( length(method) == 2) {
        method="rho.p"
    }
    if(method=="rho.p"){

        for(i in 1:length(grps)){

            message(paste("Making network",i))

            rq.cells <- as.vector(colnames(data)[which(loc@userGroups[,info@gname] ==grps[i])])

            if ( length(rq.cells) < 10 ) {
                message(paste("not enough cell in group",  grps[i], "(", length(rq.cells),")" ) )
                if(interactive()) { browser() }
                next
            }
            ## now remove all 'rarely expressed' genes
            sub.d <- data[, rq.cells ]
            min = exprFract * length( rq.cells )
            if ( min < 3){
                min = 3
            }
            OK = which( FastWilcoxTest::ColNotZero( Matrix::t(sub.d) ) >= min )
            sub.d <- sub.d[OK,]
            
            if ( nrow( sub.d) < 2 ) {
                message("less than two genes expressed in the group - next")
                next
            }
            cor.mat <- propr::perb(as.matrix(t(sub.d)))@matrix
            cor.mat.flt <- cormat2df(cor.mat) #function definition in file 'NetworkFunctions.R'
            cor.mat.ord <-  cor.mat.flt[rev(order(cor.mat.flt[,3])),]
            
            cor.cut <- quantile(cor.mat.ord[,3],0.99)
            if ( cor.cut == 1) {
                cor.cut = 1-1e-4
            }
            cor.cut.low <- quantile(cor.mat.ord[,3],0.01)

            hi.prop <- length(which(cor.mat.ord[,3] > cor.cut)) / nrow(cor.mat.ord)
            lo.prop <- length(which(cor.mat.ord[,3] < cor.cut.low))/ nrow(cor.mat.ord)


            loc.top.n.inter = top.n.inter
            if ( loc.top.n.inter > nrow(cor.mat.ord) ){
                loc.top.n.inter = round(nrow(cor.mat.ord) / 10 )
            }

            hi.num <- round(loc.top.n.inter*(hi.prop/(hi.prop+lo.prop)))
            lo.num <- loc.top.n.inter-hi.num

            cor.mat.req <- cor.mat.ord[ c(1:hi.num, (nrow(cor.mat.ord)-lo.num):nrow(cor.mat.ord)),]

            net <- cbind(cor.mat.req[,c(3,1,2)],0,0,0)
            colnames(net) <- c("pcor","node1","node2","pval","qval","prob")
            if ( is.null(rownames(cellexalObj@drc[[req.graph]])) ) {
                rownames(cellexalObj@drc[[req.graph]]) =colnames( cellexalObj@data )
            }
            avg.drc.coods <- rbind(avg.drc.coods, c(apply(cellexalObj@drc[[req.graph]][rq.cells,1:3],2,mean),info@col[i]))

             if(nrow(cor.mat.req)>0){

                key1 <- paste(net[,2],net[,3],sep="")
                key2 <- paste(net[,3],net[,2],sep="")

                grp.tabs <- rbind(grp.tabs,cbind(net,info@col[i],key1,key2))

            }else{next}
        }
    
    }

    if(method=="pcor"){

        for(i in 1:length(grps)){

            message(paste("Making network",i))

            rq.cells <- as.vector(colnames(data)[which(loc@userGroups[,info@gname] ==grps[i])])

            if ( length(rq.cells) < 10 ) {
                message(paste("not enough cell in group",  grps[i], "(", length(rq.cells),")" ) )
                if(interactive()) { browser() }
                next
            }
            ## now remove all 'rarely expressed' genes
            sub.d <- data[, rq.cells ]
            min = exprFract * length( rq.cells )
            if ( min < 5){
                min = 5
            }

            OK = which( FastWilcoxTest::ColNotZero( Matrix::t(sub.d) ) >= min )
            sub.d <- sub.d[OK,]
            
            if ( nrow( sub.d) < 2 ) {
                message("less than two genes expressed in the group - next")
                next
            }

            inferred.pcor <- GeneNet::ggm.estimate.pcor(as.matrix(Matrix::t(sub.d)),method="static")
            test.results <- GeneNet::network.test.edges(inferred.pcor,plot=F)
            net <- GeneNet::extract.network(test.results, cutoff.ggm = cutoff.ggm )

            if(nrow(net)>top.n.inter){
                net <- net[1:top.n.inter,]
            }
            avg.drc.coods <- rbind(avg.drc.coods, c(apply(cellexalObj@drc[[req.graph]][rq.cells,],2,mean),info@col[i]))

            if(nrow(net)>0){

                net[,2] <- rownames(sub.d)[net[,2]]
                net[,3] <- rownames(sub.d)[net[,3]]

                key1 <- paste(net[,2],net[,3],sep="")
                key2 <- paste(net[,3],net[,2],sep="")

                grp.tabs <- rbind(grp.tabs,cbind(net,info@col[i],key1,key2))

            }else{next}

        }
    }
	message( paste( "the cellexal object drc names:", paste( collapse= ", ", names(cellexalObj@drc))))
    # browser()
    # # check if this plot makes sense:
    # rgl::plot3d( cellexalObj@drc[[req.graph]][which( is.na(cellexalObj@userGroups[,info@gname])),], col='gray80')
    # OK = which( ! is.na(cellexalObj@userGroups[,info@gname]))
    # rgl::points3d( cellexalObj@drc[[req.graph]][ OK, ], col= cellexalObj@colors[[ info@gname ]] [ cellexalObj@userGroups[OK,info@gname ]] )
    # rgl::points3d( avg.drc.coods[,1:3], col=avg.drc.coods[,4], size=5 )
    # # looks good! 2020/03/11
    if(nrow(grp.tabs)==0){
        message("There are no networks to see here.")
        invisible(cellexalObj)
    }else{
    utils::write.table(grp.tabs,file.path( outpath,"Networks.nwk"),row.names=F,col.names=T,quote=F,sep="\t",eol="\r\n")
    utils::write.table(cbind(avg.drc.coods,req.graph),file.path( outpath,"NwkCentroids.cnt"),row.names=F,col.names=F,quote=F,sep="\t",eol="\r\n")
    #utils::write.table(layout.tabs,file.path(outpath,"NwkLayouts.lay"),row.names=T,col.names=F,quote=F,sep="\t",eol="\r\n")
    if ( ! is.null(cellexalObj@usedObj$sessionName) ) {
        ## write some info to the session.
        ## how many networks have we already saved?
        cellexalObj = logNetwork( cellexalObj, grouping=info@gname )
    }
	invisible(cellexalObj)
    }
} )



#' @rdname make.cellexalvr.network
setMethod('make.cellexalvr.network', signature = c ('character'),
		definition = function (cellexalObj, cellidfile,outpath, cutoff.ggm=0.1, exprFract = 0.1, top.n.inter=130,method=c("rho.p","pcor")) {
			cellexalObj2 <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			make.cellexalvr.network( cellexalObj2, cellidfile,outpath, cutoff.ggm, top.n.inter) #function definition in file 'NetworkFunctions.R'
		}
)
