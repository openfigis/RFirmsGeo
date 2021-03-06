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
#' @param wfs an object of class "WFSClient" from \pkg{ows4R} package
#' @param cleanGeom an object of class "logical" indicating if geometries have to
#'        be validated with \pkg{cleangeo}. Default value is TRUE.
#' @param cleanStrategy an object of class "character". Default is "BUFFER"
#' @param unionStrategy an object of class "character". Accepted values are "union"
#' (pure geoprocessing union - time consuming -), "bbox" (strategy to estimate the
#' largest spatial object to retain, by comparing envelopes, and results much less
#' time consuming). Default value is "bbox".
#' @param verbose an object of class "logical" either logs have to be printed out.
#'        Default value is TRUE.
#' @return an object of class "SpatialPolygonsDataFrame"
#' 
#' @note end-user function
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
buildSpatialObject <- function(item, lang, host, domain,
                               wfs,
                               cleanGeom = TRUE, cleanStrategy = "BUFFER",
                               unionStrategy = "bbox",
                               verbose = TRUE){
  
  logger.info("----------------------------------------------------")
  logger.info(sprintf("Build spatial object for %s factsheet %s...", domain, item))
  
  fs <- fetchFactsheetInfo(item, lang, domain, host, species = FALSE, verbose = verbose)
  fs.sp <- NULL
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
  
  title <- fs$title
  georef <- fs$georef
  items <- fs$waterRefs
  category <- fs$category
  agency <- fs$agency
    
  #figisID
  FigisID <- unique(items$FigisID)
  
  #collect list of Spatial objects for water areas
  waterareas <- items[items$category %in% c("WaterArea","LandArea"),]
  area.sp.list <- readSpatialObjects(wfs, waterareas, cleanGeom, cleanStrategy, verbose)
  #patch in case of WFS failure
  anyNullArea <- any(sapply(area.sp.list, is.null))
  if(any(sapply(area.sp.list, is.null))){
    it <- 1
    while(anyNullArea & it<=3){
      area.sp.list <- readSpatialObjects(wfs, waterareas, cleanGeom, cleanStrategy, verbose)
      anyNullArea <- all(sapply(area.sp.list, is.null))
      it <- it+1
    }
  }
  
  #collect andlist of spatial objects for species distributions
  species <- items[items$category == "SpeciesDistribution",]
  species.sp.list <- readSpatialObjects(wfs, species, cleanGeom, cleanStrategy, verbose)
  
  #apply union strategy with species distributions (if more than one)
  if(length(species.sp.list) > 1){
    
    if(verbose){
      logger.info(sprintf("Unifying %s species distributions...",length(species.sp.list)))
      logger.info(sprintf("Union strategy : %s ", unionStrategy))
    }
    
    if(unionStrategy == "union"){
      #apply a pure geoprocessing union
      spUnion <- NULL
      for(i in 1:length(species.sp.list)){
        if(i == 1){
          spUnion <- species.sp.list[[i]]
        }else{
          spUnion <- gUnion(spUnion, species.sp.list[[i]])
          spUnion <- clgeo_Clean(spUnion, strategy = cleanStrategy) 
        }
      }
      spUnionId <- paste(species$typeName, collapse="_union_")
      spUnion <- spChFIDs(spUnion, spUnionId)
      spUnion.df <- data.frame(gml_id = spUnionId, stringsAsFactors = FALSE)
      row.names(spUnion.df) <- spUnionId
      species.sp.list <- SpatialPolygonsDataFrame(Sr = spUnion, data = spUnion.df)
      
    }else if(unionStrategy == "bbox"){
      #apply an approximated bbox union (retain the largest spatial object)
      spRef <- NULL
      spRef.env <- NULL
      for(i in 1:length(species.sp.list)){
        if(i == 1){
          spRef <- species.sp.list[[1]]
          spRef.env <- gEnvelope(spRef)
        }
        nextsp <- species.sp.list[[i]]
        nextsp.env <- gEnvelope(nextsp)
        if(gArea(nextsp.env) > gArea(spRef.env)){
          spRef <- nextsp
          spRef.env <- nextsp.env
        }
      }
      species.sp.list <- spRef
    }
  }
  
  #collect list of spatial objects
  sp.list <- c(area.sp.list, species.sp.list)
  sp.list <- sp.list[!sapply(sp.list, is.null)]
  
  #geospatial processing
  out.sp <- NULL
  if(length(sp.list) == 1){
    logger.info(sprintf("Grabing unique geographic reference for %s factsheet %s...",domain,item))
    out.sp <- gUnaryUnion(sp.list[[1]])
  }
  if(length(sp.list) > 1){
    
    #prioritize by area
    sp.list <- sp.list[order(sapply(sp.list, gArea))]
    
    if(verbose){
      logger.info("Geoprocessing sequential intersection...")
      logger.info(sprintf("Sequential intersection with %s (%s intersections)",length(sp.list), length(sp.list)-1))
    }
    
    #performing intersection
    if(verbose){
      logger.info("Intersecting (seq 1)...")
    }
    int <- NULL
    #1st intersection
    if(!gContainsProperly(gEnvelope(sp.list[[2]]),gEnvelope(sp.list[[1]]))){
      int <- tryCatch(intersection(sp.list[[1]], sp.list[[2]]),
                      error = function(err){
                        if(verbose){
                          logger.error("Intersection internal error. Skip intersection process")
                        }
                      })
      
      if(!is.null(int)){
        int <- clgeo_Clean(int, strategy = cleanStrategy)
      }
    }
    #subsequent intersections
    if(length(sp.list) > 2 & !is.null(int)){
      for(i in 3:length(sp.list)){
        
        intNb <- i-1
        if(verbose){
          logger.info(paste0("Intersecting (seq ",intNb,")..."))
        }
        
        tmpint <- NULL
        if(!gContainsProperly(gEnvelope(sp.list[[i]]),gEnvelope(int))){
          tmpint <- tryCatch(intersection(int, sp.list[[i]]),
                          error = function(err){
                            if(verbose){
                              logger.error("Intersection internal error. Skip intersection process")
                            }
                            tmpint <<- NULL
                          })
          if(!is.null(tmpint)){
            int <- tmpint
          }
        }
        if(!is.null(int)){
          cleanint <- clgeo_Clean(int, strategy = cleanStrategy)
          int <- cleanint
        }else{
          break;
        }
      }
    }
    
    #perform union
    if(!is.null(int)){
      if(verbose) logger.info("Geoprocessing union...")
      out.sp <- int
    }else{
      if(verbose) logger.info("Pickup smallest envelope...")
      out.sp <- sp.list[[1]]
    }
    out.sp <- gUnaryUnion(out.sp)
    out.sp <- clgeo_Clean(out.sp, strategy = cleanStrategy)
  }
  
  #wrap output as SpatialPolygonsDataFrame object
  if(!is.null(out.sp)){
    out.sp <- RFigisGeo::clipToGlobalExtent(out.sp)
    if(is.na(proj4string(out.sp))) proj4string(out.sp) <- CRS("+init=epsg:4326")
    out.sp <- spChFIDs(out.sp, FigisID)
    areaCRS <- CRS("+proj=eck4")
    pout.sp <- NULL
    pout.sp <- tryCatch(spTransform(out.sp, areaCRS),
                        error = function(err){
                          if(verbose){
                            logger.error("Error in calculating the projected area.")
                          }
                          pout.sp <<- NULL
                        })
    parea <- NA
    if(!is.null(pout.sp)) parea <- gArea(pout.sp)
    out.df <- data.frame(
      DOMAIN = domain,
      CATEGORY = category,
      FIGIS_ID = FigisID,
      LANG = lang,
      TITLE = title,
      GEOREF = georef$title,
      SCALE = georef$scale,
      AGENCY = agency,
      SURFACE = parea,
      stringsAsFactors = FALSE)
    for(column in colnames(out.df)){
      if(class(out.df[,column]) == "character") Encoding(out.df[,column]) <- "UTF-8"
    }
    
    row.names(out.df) <- out.df$FIGIS_ID
    out.sp <- SpatialPolygonsDataFrame(Sr = out.sp, data = out.df, match.ID = TRUE)
  }
  
  return(out.sp)
}
