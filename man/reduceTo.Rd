% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reduceTo.R
\docType{methods}
\name{reduceTo}
\alias{reduceTo}
\alias{reduceTo,cellexalvrR-method}
\title{reduce a cellexalvrR object on either genes or columns}
\usage{
reduceTo(x, what = "row", to = NULL)

\S4method{reduceTo}{cellexalvrR}(x, what = "row", to = NULL)
}
\arguments{
\item{x}{the cellexalvrR object}

\item{what}{reduce to samples or row ids default='row'}

\item{to}{select these names default=NULL}
}
\description{
The main reduction function can drop both samples and genes using the colnames/rownames of the data tables
}
\details{
reduceTo is the main cellexalvrR subsetting function. It can drop cells as well as genes
and makes sure, that all other objects still are in the same dimensianlity as the main data.
}
