#' @name readSpatialObject
#' @aliases readSpatialObject
#' @title readSpatialObject
#' 
#' @description
#' A function to read a spatial object from an layer item
#'
#' @param layer an object of class "data.frame" (single record) giving information
#'        about a GIS web-layer to query
#' @param cleanGeom an object of class "logical" indicating if geometries have to
#'        be validated with \pkg{cleangeo}. Default value is TRUE.
#' @param verbose an object of class "logical" either logs have to be printed out.
#'        Default value is TRUE.
#' @return an object of class "spatial" giving the spatial object
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
readSpatialObject <- function(layer, cleanGeom = TRUE, verbose = TRUE){
  
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
  if(verbose) logger.info(paste0("GET ",wfsRequest))
  
  #download data
  out <- NULL
  out <- tryCatch(suppressWarnings(readWFS(wfsRequest, verbose = verbose)),
                  error = function(err){
                    if(verbose){
                      logger.warn("Unknown or Empty filtered GIS web-resource")
                    }
                  })
  if(!is.null(out)){
    if(cleanGeom){
      out <- clgeo_Clean(out)
    }
  }
  return(out)
}

#' @name readSpatialObjects
#' @aliases readSpatialObjects
#' @title readSpatialObjects
#' 
#' @description
#' A function to list spatial objects from a list of layer items
#'
#' @param layers an object of class "data.frame" (single record) giving information
#'        about a GIS web-layer to query
#' @param cleanGeom an object of class "logical" indicating if geometries have to
#'        be validated with \pkg{cleangeo}. Default value is TRUE.
#' @param verbose an object of class "logical" either logs have to be printed out.
#'        Default value is TRUE.
#' @return an object of class "list" listing the spatial objects
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
readSpatialObjects <- function(layers, cleanGeom, verbose){
  logger.info("Read spatial objects...")
  sp.layers <- lapply(unique(layers$typeName), function(x){
    geoitem <- layers[layers$typeName == x,]
    sp <- readSpatialObject(geoitem, cleanGeom, verbose)
    return(sp)
  })
  sp.layers <- sp.layers[!sapply(sp.layers, is.null)]
}