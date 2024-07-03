#' @name buildSpatialObject
#' @aliases buildSpatialObject
#' @title buildSpatialObject 
#'
#' @description
#' A function to build a spatial object for a given factsheet
#'
#' @param x FIRMS inventory item (as retrieved through web-services)
#' @param domain inventory domain (fishery/resource)
#' @param wfs an object of class "WFSClient" from \pkg{ows4R} package
#' @param verbose an object of class "logical" either logs have to be printed out.
#'        Default value is TRUE.
#' @return an object of class "SpatialPolygonsDataFrame"
#' 
#' @note end-user function
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
buildSpatialObject <- function(x,
                               domain,
                               wfs,
                               verbose = TRUE){
  
  item = x$document$invObsId
  
  logger.info("----------------------------------------------------")
  logger.info(sprintf("Build spatial object for %s factsheet %s...", domain, item))
  
  fs <- fetchFactsheetInfo(x, domain, verbose = verbose)
  if(!is.null(fs)){
    if(nrow(fs$waterRefs) == 0){
      logger.warn(sprintf("No geographic reference for %s factsheet %s...", domain, item))
      return(NULL)
    }else{
      categories <- unique(fs$waterRefs$category)
      if(length(categories) == 1){
        if(categories[1] != "WaterArea"){
          logger.warn(sprintf("No WaterAreaRef for %s factsheet %s...", domain, item))
        }
        if(categories[1] == "LandArea"){
          logger.warn(sprintf("Presence of LandAreaArea for %s factsheet %s...", domain, item))
        }
      }
    }
  }else{
    logger.warn(sprintf("No geographic reference for %s factsheet %s...", domain, item))
    return(NULL)
  }

  #collect list of Spatial objects (sf collections) for water areas
  items = fs$waterRefs
  waterareas <- items[items$category %in% c("WaterArea","LandArea"),]
  area.sf.list <- readSpatialObjects(wfs, waterareas, verbose)
  #patch in case of WFS failure
  anyNullArea <- any(sapply(area.sf.list, is.null))
  if(any(sapply(area.sf.list, is.null))){
    it <- 1
    while(anyNullArea & it<=3){
      area.sf.list <- readSpatialObjects(wfs, waterareas, verbose)
      anyNullArea <- all(sapply(area.sf.list, is.null))
      it <- it+1
    }
  }
  
  sf.list = area.sf.list #we don't use anymore possible species distributions
  
  #geospatial processing
  out.sf <- NULL
  if(length(sf.list) == 1){
    logger.info(sprintf("Grabing unique geographic reference for %s factsheet %s...",domain,item))
    out.sf <- sf::st_union(sf.list[[1]])
  }
  if(length(sf.list) > 1){
    
    #prioritize by area
    sf.list <- sf.list[order(sapply(sf.list, sf::st_area))]
    
    if(verbose){
      logger.info("Geoprocessing sequential intersection...")
      logger.info(sprintf("Sequential intersection with %s (%s intersections)",length(sf.list), length(sf.list)-1))
    }
    
    #performing intersection
    if(verbose){
      logger.info("Intersecting (seq 1)...")
    }
    int <- NULL
    #1st intersection
    #if(!gContainsProperly(gEnvelope(sf.list[[2]]),gEnvelope(sf.list[[1]]))){
    int <- tryCatch(sf::st_intersection(sf.list[[1]], sf.list[[2]]),
                    error = function(err){
                      if(verbose){
                        logger.error("Intersection internal error. Skip intersection process")
                      }
                    })
    
    if(!is.null(int)){
      int <- sf::st_make_valid(int)
    }
    #}
    #subsequent intersections
    if(length(sf.list) > 2 & !is.null(int)){
      for(i in 3:length(sf.list)){
        
        intNb <- i-1
        if(verbose){
          logger.info(paste0("Intersecting (seq ",intNb,")..."))
        }
        
        tmpint <- NULL
        #if(!gContainsProperly(gEnvelope(sp.list[[i]]),gEnvelope(int))){
          tmpint <- tryCatch(sf::st_intersection(int, sf.list[[i]]),
                          error = function(err){
                            if(verbose){
                              logger.error("Intersection internal error. Skip intersection process")
                            }
                            tmpint <<- NULL
                          })
          if(!is.null(tmpint)){
            int <- tmpint
          }
        #}
        if(!is.null(int)){
          cleanint <- sf::st_make_valid(int)
          int <- cleanint
        }else{
          break;
        }
      }
    }
    
    #perform union
    if(!is.null(int)){
      if(verbose) logger.info("Geoprocessing union...")
      out.sf <- int
    }else{
      if(verbose) logger.info("Pickup smallest envelope...")
      out.sf <- sf.list[[1]]
    }
    out.sf <- sf::st_union(out.sf)
    out.sf <- sf::st_make_valid(out.sf)
  }
  
  #wrap output as SpatialPolygonsDataFrame object
  if(!is.null(out.sf)){
    if(is.na(sf::st_crs(out.sf))) sf::st_crs(out.sf) = 4326
    areaCRS <- "+proj=eck4"
    pout.sf <- NULL
    pout.sf <- tryCatch(sf::st_transform(out.sf, areaCRS),
                        error = function(err){
                          if(verbose){
                            logger.error("Error in calculating the projected area.")
                          }
                          pout.sf <<- NULL
                        })
    parea <- NA
    if(!is.null(pout.sf)) parea <- as.numeric(sf::st_area(pout.sf))
    out.sf <- sf::st_sf(
      the_geom = out.sf,
      DOMAIN = domain,
      CATEGORY = fs$category,
      FIGIS_ID = fs$figis_id,
      LANG = fs$lang,
      TITLE = fs$title,
      GEOREF = fs$georef,
      SCALE = fs$scale,
      AGENCY = fs$agency,
      SURFACE = parea,
      stringsAsFactors = FALSE
    )
    for(column in colnames(out.sf)){
      if(class(out.sf[[column]])[1] == "character") Encoding(out.sf[[column]]) <- "UTF-8"
    }
    
    row.names(out.sf) <- out.sf$FIGIS_ID
  
  }
  
  return(out.sf)
}
