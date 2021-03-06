% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GrabDRCs.R
\docType{methods}
\name{GrabDRCs}
\alias{GrabDRCs}
\alias{GrabDRCs,cellexalvrR,cellexalvrR-method}
\title{copy 3D graphs from objects of the same analysis}
\usage{
GrabDRCs(x, other, prefix = NULL)

\S4method{GrabDRCs}{cellexalvrR,cellexalvrR}(x, other, prefix = NULL)
}
\arguments{
\item{x}{the cellexalvrR object}

\item{other}{the other cellexalvrR object}

\item{prefix}{a prefix for the 3D graph name (shown in VR)}
}
\description{
make 3D graphs from consecutive analysies available in the main analysis VR session
}
\details{
Copy all drc models from one cellexalObj to another - even if the
order and/or the size of the objects is not the same.
}
