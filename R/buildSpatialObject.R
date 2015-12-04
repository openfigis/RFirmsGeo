#' @name buildSpatialObject
#' @aliases buildSpatialObject
#' @title buildSpatialObject 
#'
#' @description
#' A function to build a spatial object for a given factsheet
#'
#' @param item an object of class "character" giving the factsheet id
#' @param lang the language in which the factsheet is available
#' @param host an object of class "character" giving the host
#' @param domain an object of class "character" giving the FIRMS domain
#' @param cleanGeom an object of class "logical" indicating if geometries have to
#'        be validated with \pkg{cleangeo}. Default value is TRUE.
#' @param verbose an object of class "logical" either logs have to be printed out.
#'        Default value is TRUE.
#' @return an object of class "SpatialPolygonsDataFrame"
#' 
#' @note end-user function
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
buildSpatialObject <- function(item, lang, host, domain, cleanGeom = TRUE, verbose = TRUE){
  
  logger.info(sprintf("Build spatial object for %s factsheet %s...", domain, item))
  
  fs <- fetchFactsheetInfo(item, lang, domain, host, verbose)
  fs.sp <- NULL
  if(!is.null(fs)){
    if(nrow(fs$waterRefs) == 0){
      logger.warn(sprintf("No geographic reference for %s factsheet %s...", domain, item))
      return(NULL)
    }else{
      categories <- unique(fs$waterRefs$category)
      if(length(categories) == 1 && categories[1] == "SpeciesDistribution"){
        logger.warn(sprintf("No WaterAreaRef for %s factsheet %s...", domain, item))
      }
    }
  }else{
    logger.warn(sprintf("No geographic reference for %s factsheet %s...", domain, item))
    return(NULL)
  }
  
  title <- fs$title
  georef <- fs$georef
  items <- fs$waterRefs
    
  #figisID
  FigisID <- unique(items$FigisID)
  
  #collect list of Spatial objects for water areas
  waterareas <- items[items$category == "WaterArea",]
  area.sp.list <- readSpatialObjects(waterareas, cleanGeom, verbose)
  
  #collect andlist of spatial objects for species distributions
  species <- items[items$category == "SpeciesDistribution",]
  species.sp.list <- readSpatialObjects(species, cleanGeom, verbose)
  #unify species distributions (if more than one)
  if(length(species.sp.list) > 1){
    if(verbose){
      logger.info("Geoprocessing union (species distributions)...")
      logger.info(sprintf("Union of %s species distributions",length(species.sp.list)))
    }
    spUnion <- NULL
    for(i in 1:length(species.sp.list)){
      if(i == 1){
        spUnion <- species.sp.list[[i]]
      }else{
        spUnion <- gUnion(spUnion, species.sp.list[[i]])
        spUnion <- clgeo_Clean(spUnion) 
      }
    }
    spUnionId <- paste(species$typeName, collapse="_union_")
    spUnion <- spChFIDs(spUnion, spUnionId)
    spUnion.df <- data.frame(gml_id = spUnionId, stringsAsFactors = FALSE)
    row.names(spUnion.df) <- spUnionId
    species.sp.list <- SpatialPolygonsDataFrame(Sr = spUnion, data = spUnion.df)
    
  }
  
  #collect list of spatial objects
  sp.list <- c(area.sp.list, species.sp.list)
  sp.list <- sp.list[!sapply(sp.list, is.null)]
  
  #geospatial processing
  if(verbose){
    logger.info("Geoprocessing sequential intersection...")
    logger.info(sprintf("Sequential intersection with %s (%s intersections)",length(sp.list), length(sp.list)-1))
  }
  out.sp <- NULL
  if(length(sp.list) == 1) out.sp <- gUnaryUnion(sp.list[[1]])
  if(length(sp.list) > 1){
    
    #performing intersection
    if(verbose){
      logger.info("Intersecting (seq 1)...")
    }  
    int <- NULL
    int <- tryCatch(intersection(sp.list[[1]], sp.list[[2]]),
                    error = function(err){
                      if(verbose){
                        logger.info("Intersection internal error. Skip intersection process")
                      }
                    })
    if(!is.null(int)){
      int <- clgeo_Clean(int)
    }
    if(length(sp.list) > 2 & !is.null(int)){
      for(i in 3:length(sp.list)){
        
        intNb <- i-1
        if(verbose){
          logger.info(paste0("Intersecting (seq ",intNb,")..."))
        }
        
        tmpint <- NULL
        tmpint <- tryCatch(intersection(int, sp.list[[i]]),
                        error = function(err){
                          if(verbose){
                            logger.info("Intersection internal error. Skip intersection process")
                          }
                          tmpint <<- NULL
                        })
        if(!is.null(tmpint)){
          int <- tmpint
        }
        if(!is.null(int)){
          cleanint <- clgeo_Clean(int)
          int <- cleanint
        }else{
          break;
        }
      }
    }
    
    #perform union
    if(verbose){
      logger.info("Geoprocessing union...")
    }
    if(!is.null(int)){
      out.sp <- int
    }else{
      out.sp <- sp.list[[1]] #to discuss this rule
    }
    out.sp <- gUnaryUnion(out.sp)
    out.sp <- clgeo_Clean(out.sp)
    
  }
  
  #wrap output as SpatialPolygonsDataFrame object
  if(!is.null(out.sp)){
    out.sp <- spChFIDs(out.sp, FigisID)
    areaCRS <- CRS("+proj=eck4 +lon_0=Central Meridian +x_0=False Easting +y_0=False Northing")
    out.df <- data.frame(
      FIGIS_ID = FigisID,
      LANG = lang,
      TITLE = title,
      GEOREF = georef$title,
      SCALE = georef$scale,
      SURFACE = gArea(spTransform(out.sp, areaCRS)),
      stringsAsFactors = FALSE)
    row.names(out.df) <- out.df$FIGIS_ID
    out.sp <- SpatialPolygonsDataFrame(Sr = out.sp, data = out.df, match.ID = TRUE)
  }
  
  return(out.sp)
}
