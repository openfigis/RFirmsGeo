#' @name readSpatialObject
#' @aliases readSpatialObject
#' @title readSpatialObject
#' 
#' @description
#' A function to read a spatial object from an layer item
#'
#' @param wfs instance of WFS client, object of class \code{WFSClient} from \pkg{ows4R}
#' @param layer an object of class "data.frame" (single record) giving information
#'        about a GIS web-layer to query
#' @param cleanGeom an object of class "logical" indicating if geometries have to
#'        be validated with \pkg{cleangeo}. Default value is TRUE.
#' @param cleanStrategy an object of class "character". Default value is "BUFFER"
#' @param verbose an object of class "logical" either logs have to be printed out.
#'        Default value is TRUE.
#' @return an object of class "spatial" giving the spatial object
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
readSpatialObject <- function(wfs, layer, cleanGeom = TRUE, cleanStrategy = "BUFFER", verbose = TRUE){
  
  #get featuretype
  caps <- wfs$getCapabilities()
  ft <- caps$findFeatureTypeByName(layer$typeName[1], exact = TRUE)
  if(is.null(ft)){
    logger.error(sprintf("Unknown GIS web-resource '%s'", layer$typeName))
    return(NULL)
  }
  
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
  if(!is.null(cqlFilter)) cqlFilter <- utils::URLencode(cqlFilter)
  
  #download data
  out <- NULL
  if(!is.null(cqlFilter)){
    out <- suppressWarnings(tryCatch(ft$getFeatures(cql_filter = cqlFilter),
                    error = function(err){
                      if(verbose){
                        logger.warn("Unknown or Empty filtered GIS web-resource")
                      }
                    }))
  }else{
    out <- suppressWarnings(tryCatch(ft$getFeatures(),
                    error = function(err){
                      if(verbose){
                        logger.warn("Unknown or Empty filtered GIS web-resource")
                      }
                    }))
  }
  if(!is.null(out)) if(nrow(out)==0L) out <- NULL
  if(!is.null(out)){
    out <- as(out, "Spatial")
    if(cleanGeom){
      out <- clgeo_Clean(out, strategy = cleanStrategy)
    }
  }else{
    logger.warn("Unknown or Empty filtered GIS web-resource")  
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
#' @param wfs instance of WFS client, object of class \code{WFSClient} from \pkg{ows4R}
#' @param layers an object of class "data.frame" (single record) giving information
#'        about a GIS web-layer to query
#' @param cleanGeom an object of class "logical" indicating if geometries have to
#'        be validated with \pkg{cleangeo}. Default value is TRUE.
#' @param cleanStrategy an object of class "character". Default value is "BUFFER"
#' @param verbose an object of class "logical" either logs have to be printed out.
#'        Default value is TRUE.
#' @return an object of class "list" listing the spatial objects
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
readSpatialObjects <- function(wfs, layers, cleanGeom = TRUE, cleanStrategy = TRUE, verbose = TRUE){
  logger.info("Read spatial objects...")
  sp.layers <- lapply(unique(layers$typeName), function(x){
    geoitem <- layers[layers$typeName == x,]
    sp <- readSpatialObject(wfs, geoitem, cleanGeom, cleanStrategy, verbose)
    return(sp)
  })
  sp.layers <- sp.layers[!sapply(sp.layers, is.null)]
}
