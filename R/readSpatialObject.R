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
#' @param cleanStrategy an object of class "character". Default value is "BUFFER"
#' @param verbose an object of class "logical" either logs have to be printed out.
#'        Default value is TRUE.
#' @return an object of class "spatial" giving the spatial object
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
readSpatialObject <- function(layer, cleanGeom = TRUE, cleanStrategy = "BUFFER", verbose = TRUE){
  
  #prepare WFS request
  wfsRequest <- prepareWFSRequest(layer)
  if(verbose) logger.info(paste0("GET ",wfsRequest))
  
  #download data
  out <- NULL
  out <- suppressWarnings(tryCatch(readWFS(wfsRequest, verbose = verbose),
                  error = function(err){
                    if(verbose){
                      logger.warn("Unknown or Empty filtered GIS web-resource")
                    }
                  }))
  if(!is.null(out)){
    if(cleanGeom){
      out <- clgeo_Clean(out, strategy = cleanStrategy)
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
#' @param cleanStrategy an object of class "character". Default value is "BUFFER"
#' @param verbose an object of class "logical" either logs have to be printed out.
#'        Default value is TRUE.
#' @return an object of class "list" listing the spatial objects
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
readSpatialObjects <- function(layers, cleanGeom = TRUE, cleanStrategy = TRUE, verbose = TRUE){
  logger.info("Read spatial objects...")
  sp.layers <- lapply(unique(layers$typeName), function(x){
    geoitem <- layers[layers$typeName == x,]
    sp <- readSpatialObject(geoitem, cleanGeom, cleanStrategy, verbose)
    return(sp)
  })
  sp.layers <- sp.layers[!sapply(sp.layers, is.null)]
}