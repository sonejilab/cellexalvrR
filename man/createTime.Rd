% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02cellexalTime.R
\docType{methods}
\name{createTime}
\alias{createTime}
\alias{createTime,cellexalLinear-method}
\title{create the pseudo time / linear selection for the selection}
\usage{
createTime(x, parentSelection = NULL)

\S4method{createTime}{cellexalLinear}(x, parentSelection = NULL)
}
\arguments{
\item{x}{the object}

\item{parentSelection}{the selection name this linear selection bases on.}
}
\description{
calculate pseudo time based on the internal table a b and c columns
}
\details{
This function utilizes slingshot to identify the longest possible pseudo 
linearSelection for this selection.
In short the drc model of the selected genes is clustered using kmeans
and the first and last selected cell is used to determine the start and end points of the
anlysis in the drc model.
Subsequently slignshot is used to calculate the pseudo time over the selection.
This function will create the same linearSelection if the same cells are used as input.
}
