% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildSpatialDataset.R
\name{buildSpatialDataset}
\alias{buildSpatialDataset}
\title{buildSpatialDataset}
\usage{
buildSpatialDataset(
  host,
  domain,
  doc,
  ids = NULL,
  runParallel = FALSE,
  runCores = 1,
  exportPartialResults = FALSE,
  exportPath = getwd(),
  verbose = TRUE
)
}
\arguments{
\item{host}{an object of class "character" giving the host}

\item{domain}{an object of class "character" giving the FIRMS domain}

\item{doc}{a FIRMS inventory doc (retrieved as JSON through web-service)}

\item{ids}{a vector of resource IDs for which the computation has to be run}

\item{runParallel}{To run code with parallel R interface. Default is FALSE}

\item{runCores}{Number of cores to use when parallel = TRUE.}

\item{exportPartialResults}{if partial shapefiles have to be exported Default
is FALSE.}

\item{exportPath}{the path were shapefiles should be exported}

\item{verbose}{an object of class "logical" either logs have to be printed out.
Default value is TRUE.}
}
\value{
an object of class "sf"
}
\description{
A function to build a spatial dataset for a given factsheet domain.
}
\note{
end-user function
}
\author{
Emmanuel Blondel, \email{emmanuel.blondel1@gmail.com}
}
