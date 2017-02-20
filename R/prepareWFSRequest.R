#' @name prepareWFSRequest
#' @aliases prepareWFSRequest
#' @title prepareWFSRequest
#' 
#' @description
#' A function to prepare the WFS request from a layer object
#'
#' @param layer an object of class "data.frame" (single record) giving information
#'        about a GIS web-layer to query
#' @return an object of class "character" giving the WFS GetFeature request
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
prepareWFSRequest <- function(layer){
  
  #base request
  host <- unique(layer$url)
  typeName <- unique(layer$typeName)
  wfsRequest <- sprintf("%s?service=WFS&version=1.0.0&request=GetFeature&typeName=%s",
                        host, typeName)
  
  #filters
  cqlFilter = NULL
  for(i in 1:nrow(layer)){
    gisItem <- layer[i,]
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
  return(wfsRequest)
}