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
  
  fs <- fetchFactsheetInfo(item, lang, domain, host, verbose)
  fs.sp <- NULL
  if(!is.null(fs)){
    if(nrow(fs$waterRefs) > 0){    
      #waterRefs as data.frame
      fs$waterRefs <- cbind(rep(item, nrow(fs$waterRefs)), fs$waterRefs,
                         stringsAsFactors = FALSE)
      colnames(fs$waterRefs) <- c("FigisID", "url", "typeName",
                               "propertyName", "propertyValue",
                               "level", "rank")
      if(verbose){
        print(fs$waterRefs)
      }
    }else{
      return(NULL)
    }
  }else{
    return(NULL)
  }
  
  title <- fs$title
  georef <- fs$georef
  items <- fs$waterRefs
    
  #figisID
  FigisID <- unique(items$FigisID)
  
  #list of typeNames to query
  typeNames <- unique(items$typeName)
  
  #collect list of Spatial objects
  sp.list <- lapply(typeNames, function(x){
    item <- items[items$typeName == x,]
    sp <- readSpatialObject(item, cleanGeom, verbose)
    return(sp)
  })
  sp.list <- sp.list[!sapply(sp.list, is.null)]
  
  #geospatial processing
  if(verbose){
    message("Geoprocessing...")
  }
  out.sp <- NULL
  if(length(sp.list) == 1) out.sp <- gUnaryUnion(sp.list[[1]])
  if(length(sp.list) > 1){
    
    #performing intersection
    if(verbose){
      message("Intersecting (seq 1)...")
    }  
    int <- NULL
    int <- tryCatch(intersection(sp.list[[1]], sp.list[[2]]),
                    error = function(err){
                      if(verbose){
                        message("Intersection internal error. Skip intersection process")
                      }
                    })
    if(!is.null(int)){
      int <- clgeo_Clean(int)
    }
    if(length(sp.list) > 2 & !is.null(int)){
      for(i in 3:length(sp.list)){
        
        intNb <- i-1
        if(verbose){
          message(paste0("Intersecting (seq ",intNb,")..."))
        }
        
        tmpint <- NULL
        tmpint <- tryCatch(intersection(int, sp.list[[i]]),
                        error = function(err){
                          if(verbose){
                            message("Intersection internal error. Skip intersection process")
                          }
                          tmpint <<- NULL
                        })
        if(!is.null(tmpint)){
          int <- tmpint
        }
        if(!is.null(int)){
          int <- clgeo_Clean(int)
        }else{
          break;
        }
      }
    }
    
    #perform union
    if(!is.null(int)){
      out.sp <- int
      if(cleanGeom){
        out.sp <- clgeo_Clean(out.sp)
      }
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
