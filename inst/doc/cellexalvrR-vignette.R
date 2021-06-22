## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=F-------------------------------------------------------------------
#  library(devtools)
#  install_github("sonejilab/cellexalvrR")

## ----message=FALSE------------------------------------------------------------
library(cellexalvrR)

if ( ! file.exists("log2data.RData") ) {
   log2data = as.matrix(cellexalObj@data)
   save( log2data, file="log2data.RData")
   rm( log2data)
   
   facs = cellexalObj@index
   save( facs, file= "facs.RData")
   rm( facs )
   
   cell.ids = cellexalObj@meta.cell
   save( cell.ids, file="cell.ids.RData")
   rm(cell.ids)
   
   diff.proj = cellexalObj@drc$diffusion
   save( diff.proj, file="diff.proj.RData")
   rm( diff.proj )
   
   ddr.proj = cellexalObj@drc$DDRtree
   save( ddr.proj, file="ddr.proj.RData" )
   rm(ddr.proj)
   
   tsne.proj = cellexalObj@drc$tSNE
   save( tsne.proj, file="tsne.proj.RData")
   rm(tsne.proj)
   
}

## ---- fig.show='hold',eval=TRUE-----------------------------------------------
	
load("log2data.RData")
load("facs.RData")
load("cell.ids.RData")
load("diff.proj.RData")
load("ddr.proj.RData")
load("tsne.proj.RData")

## ---- fig.show='hold',eval=T--------------------------------------------------
log2data[1:10,1:10]

## ---- fig.show='hold',eval=T--------------------------------------------------
head(facs)

## ---- fig.show='hold',eval=T--------------------------------------------------
head(cell.ids)

## ---- fig.show='hold',eval=T--------------------------------------------------
head(diff.proj)

## ---- fig.show='hold',eval=T--------------------------------------------------
proj.list <- list(diffusion=diff.proj,DDRtree=ddr.proj,tSNE=tsne.proj)

## ---- fig.show='hold',eval=T--------------------------------------------------
cellvr <- MakeCellexalVRObj( Matrix::Matrix(log2data, sparse=TRUE),
  drc.list=proj.list,specie="mouse",cell.meta=cell.ids,facs.data=NULL)

## -----------------------------------------------------------------------------
cellvr

## ---- fig.show='hold',eval=F--------------------------------------------------
#  export2cellexalvr(cellvr,"CellexalOut/")

## ----fig.show='hold',eval=T---------------------------------------------------
cell.vr.scr <- new("cellexalvr",data=Matrix::Matrix(log2data,sparse=T),drc=list(tsne=tsne.proj))
cell.vr.scr

## ----fig.show='hold',eval=T---------------------------------------------------
cell.vr.scr <- addDRC2cellexalvr(cell.vr.scr,ddr.proj,"DDRTree")
cell.vr.scr

## ----fig.show='hold',eval=T---------------------------------------------------
cell.vr.scr <- addCellMeta2cellexalvr(cell.vr.scr,cell.ids)
cell.vr.scr


## ----fig.show='hold',eval=F---------------------------------------------------
#  cell.vr.scr <- addFACS2cellexalvr(cell.vr.scr,facs)

## ----fig.show='hold',eval=T---------------------------------------------------
cell.vr.scr <- set.specie(cell.vr.scr,"mouse")
cell.vr.scr

## ----fig.show='hold',eval=T---------------------------------------------------
meta.df <- data.frame(CellType=sample(c("Type1","Type2","Type3"),10,replace=T),
                      Phase=sample(c("G1","G2M","S"),10,replace=T),
                      Treatment=sample(c("WT","Dox"),10,replace=T))
head(meta.df)


## ----fig.show='hold',eval=T---------------------------------------------------

required.cell.metad <- make.cell.meta.from.df(meta.df,c("CellType","Treatment"))
head(required.cell.metad)


## ----eval=F-------------------------------------------------------------------
#  # mca comes from from https://satijalab.org/seurat/v3.0/mca.html
#  mca <- RunUMAP(mca, dims = 1:75, min.dist = 0.75,n.components=3)
#  
#  mca.data <- GetAssayData(object = mca) #Extract the expression data
#  drl <- list(UMAP=Embeddings(object = mca, reduction = "umap")) # Put the UMAP coordinated into a list
#  meta <- make.cell.meta.from.df(mca[[]],"Tissue") # Make the metadata using just the "Tissue" column
#  
#  cvr <- new("cellexalvrR",data=mca.data,drc=drl) # Initialise a new cellexalvrR object with the expression data and UMAP
#  cvr <- set.specie(cvr,"mouse") # Set the specie to Mouse
#  cvr <- addCellMeta2cellexalvr(cvr,meta) # Add the metadata to the cellexalvrR object
#  export2cellexalvr(cvr,"MCA_full") #Export the files to a folder called "MCA_full"
#  

