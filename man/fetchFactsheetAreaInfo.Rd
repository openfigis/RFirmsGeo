% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetchFactsheetInfo.R
\name{fetchFactsheetAreaInfo}
\alias{fetchFactsheetAreaInfo}
\title{fetchFactsheetAreaInfo}
\usage{
fetchFactsheetAreaInfo(xml)
}
\arguments{
\item{xml}{an object of class "XmlInternalDocument"}
}
\value{
an object of class "data.frame" listing the GIS water area layer infos
}
\description{
A function to fetch the water area information from the \code{waterAreaRef} 
node of a factsheet XML. In case there is no waterAreaRef, the function will
attempt to fetch the content from \code{landAreaRef} node of the factsheet XML.
Intersecting areas are excluded.
}
\note{
function used internally to build FIRMS spatial objects
}
\author{
Emmanuel Blondel, \email{emmanuel.blondel1@gmail.com}
}
