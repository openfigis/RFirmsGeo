% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchFactsheetInfo.R
\name{fetchFactsheetBiologicalStockInfo}
\alias{fetchFactsheetBiologicalStockInfo}
\alias{fetchFactsheetStockInfo}
\title{fetchFactsheetBiologicalStockInfo}
\usage{
fetchFactsheetStockInfo(xml)
}
\arguments{
\item{xml}{an object of class "XmlInternalDocument"}
}
\value{
an object of class "character"
}
\description{
A function to fetch the stock information from the \code{AqResStruct} 
node / \code{BiologicalStock} attribute of a factsheet XML.
}
\note{
function used internally to build FIRMS spatial objects
}
\author{
Emmanuel Blondel, \email{emmanuel.blondel1@gmail.com}
}
