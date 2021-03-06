% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/renderFile.R
\docType{methods}
\name{renderFile}
\alias{renderFile}
\alias{renderFile,cellexalvrR-method}
\title{render only one html section, not the whole session log}
\usage{
renderFile(x, id, type = "")

\S4method{renderFile}{cellexalvrR}(x, id, type = "")
}
\arguments{
\item{x}{the cellexalvrR object}

\item{id}{the id of the report file to render ('x@usedObj$sessionRmdFiles[id]')}

\item{type}{the type of log saved (default '')}
}
\description{
render only one html section, not the whole session log
}
\details{
A main function in the log system - called after each log has been produced.
}
