% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01cellexalvrClass.R
\docType{class}
\name{cellexalvrR-class}
\alias{cellexalvrR-class}
\title{cellexalvrR class definition}
\description{
The R backend for the CellexalVR 3D application
}
\details{
cellexalvrR is the R class that build the backend of CellexalVR

https://www.cellexalvr.med.lu.se/ is a VR application to analyze single cell expression data.
This R class is the backend of the VR application and a dependency of that.
CellexalvrR is not developed to be a general purpose single cell analysis class like Seurat.
It e.g. does not implement normalization or drc methods, but expects all data to be preprocessed.
}
\section{Slots}{

\describe{
\item{\code{data}}{the expression matrix (unused!)}

\item{\code{data}}{the spearse matrix}

\item{\code{meta.cell}}{the cell level meta information (matrix)}

\item{\code{meta.gene}}{the gene level meta information (matrix)}

\item{\code{userGroups}}{internally used to store the user defined groupings from the 3D process (data.frame)}

\item{\code{colors}}{a list for each userGroups entry defining the color sheme for this grouping}

\item{\code{groupSelectedFrom}}{local info to store which usergroup has been selected from which drc structure}

\item{\code{usedObj}}{a generic storage list collecting objects not fitting into any other slot}

\item{\code{drc}}{a list of all DRC objects to be visible in the 3D application}

\item{\code{index}}{a matrix for FACS or other linear numeric data that should be available for colouring in the 3D application}

\item{\code{tfs}}{depricated not used any more}

\item{\code{specie}}{the species this data is from (mouse or human)}

\item{\code{outpath}}{the path this object will be saved to}

\item{\code{version}}{the cellexalvrR package version that created this object.}
}}

