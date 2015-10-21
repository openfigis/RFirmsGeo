#' @name readSpatialObject
#' @aliases readSpatialObject
#' @title readSpatialObject
#' 
#' @export
#' 
#' @description
#' A function to fetch the water area information from the \code{waterAreaRef} 
#' node of a factsheet XML.
#'
#' @param item an object of class "data.frame" (single record) giving information
#'        about a GIS web-layer to query
#' @param cleanGeom an object of class "logical" indicating if geometries have to
#'        be validated with \pkg{cleangeo}. Default value is TRUE.
#' @param verbose an object of class "logical" either logs have to be printed out.
#'        Default value is TRUE.
#' @return an object of class "data.frame" listing the GIS water area layer infos
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
readSpatialObject <- function(item, cleanGeom = TRUE, verbose = TRUE){
  
  #base request
  host <- unique(item$url)
  typeName <- unique(item$typeName)
  wfsRequest <- sprintf("%s?service=WFS&version=1.0.0&request=GetFeature&typeName=%s",
                        host, typeName)
  
  #filters
  cqlFilter = NULL
  for(i in 1:nrow(item)){
    gisItem <- item[i,]
    key <- gisItem$propertyName
    value <- gisItem$propertyValue
    if(!is.na(key) | !is.na(value)){
      gisFilter = sprintf("%s = '%s'", key, value)
      if(is.null(cqlFilter)){
        cqlFilter = gisFilter
      }else{
        cqlFilter = paste(cqlFilter, "OR", gisFilter, sep=" ")
      }
    }
  }
  if(!is.null(cqlFilter)) wfsRequest <- paste0(wfsRequest, "&cql_filter=", cqlFilter)
  if(verbose) message(wfsRequest)
  
  #download data
  out <- NULL
  out <- tryCatch(suppressWarnings(readWFS(wfsRequest, verbose = verbose)),
                  error = function(err){
                    if(verbose){
                      message("Unknown or Empty filtered GIS web-resource")
                    }
                  })
  if(!is.null(out)){
    if(cleanGeom){
      out <- clgeo_Clean(out)
    }
  }
  return(out)
}