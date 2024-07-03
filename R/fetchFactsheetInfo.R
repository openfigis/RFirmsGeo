#' @name fetchFactsheetAreaInfo
#' @aliases fetchFactsheetAreaInfo
#' @title fetchFactsheetAreaInfo
#' 
#' @description
#' A function to fetch the water area information from the \code{waterAreaRef} 
#' node of a factsheet XML. In case there is no waterAreaRef, the function will
#' attempt to fetch the content from \code{landAreaRef} node of the factsheet XML.
#' Intersecting areas are excluded.
#'
#' @param x inventory service item (as retrieved through web-service)
#' @param domain inventory domain (fishery/resource)
#' @return an object of class "data.frame" listing the GIS water area layer infos
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
fetchFactsheetAreaInfo <- function(x, domain){
  
  category = "WaterArea"
  
  waterareas = switch(domain,
    "fishery" = {
      areas = list() 
      if(length(x$document$fishingarea$fisheryPrimaryAreaCodes)>0) areas = sapply(x$document$fishingarea$fisheryPrimaryAreaCodes, function(area){area$value})
      if(length(areas)==0 & length(x$document$perspective$geoRefLandarea)>0){
        category = "LandArea"
        areas = x$document$perspective$geoRefLandarea$value
      }
      areas
    },
    "resource" = sapply(x$document$distribution$primaryArea, function(area){area$value})
  )
  waterAreaList = do.call("rbind", lapply(waterareas,function(waterarea){
    waterarea_parts = unlist(strsplit(waterarea, ":"))
    RFirmsGeo::buildGISLayerInfo(category = category, codesystem = waterarea_parts[1], code = waterarea_parts[2])
  }))
  

  if(nrow(waterAreaList) > 0){
    #remove duplicates
    waterAreaList <- unique(waterAreaList)
    
    #skip uppper levels for fishing areas
    ranks <- unique(waterAreaList$rank)
    ranks <- ranks[!is.na(ranks)]
    if(length(ranks) > 0){
      filtered <- waterAreaList[!is.na(waterAreaList$rank),]
      filtered <- do.call("rbind",
                          lapply(ranks,
                                 function(x){
                                   df <- filtered[filtered$rank == x,]
                                   df <- df[df$level == max(df$level),]
                                 }))
      waterAreaList <- rbind(filtered, waterAreaList[is.na(waterAreaList$rank),])
    }
  }
  
  return(waterAreaList)
}

#' @name fetchFactsheetInfo
#' @aliases fetchFactsheetInfo
#' @title fetchFactsheetInfo
#' 
#' @description
#' A function to fetch the factsheet information from a factsheet XML
#'
#' @param x inventory service item (as retrieved through web-service)
#' @param domain inventory domain (fishery/resource)
#' @param verbose if logs should be printed out. Default is TRUE
#' @return an object of class "list" giving the factsheet information
#'         (title and georef)
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
fetchFactsheetInfo <- function(x, domain, verbose = TRUE){
  
  if(verbose) logger.info(sprintf("Extracting information for factsheet %s", x$document$invObsId))

  out <- list(
    category = switch(domain,
      "fishery" = stringr::str_to_title(x$document$perspective$thematicApproach$value),
      "resource" = {
        if(!is.null(x$document$structure$singleStock)){
          if(x$document$structure$singleStock) "Biological Stock" else "Marine Resource"
        }else{
          "Marine Resource"
        }
      } 
        
    ),
    figis_id = x$document$invObsId,
    lang = x$document$language,
    title = x$document$title,
    georef = switch(domain,
      "fishery" = x$document$perspective$geoRefTitle,
      "resource" = if(!is.null(x$document$distribution$areaName)) x$document$distribution$areaName else ""
    ),
    scale = switch(domain,
     "fishery" = if(!is.null(x$document$perspective$spatialScale$value)) x$document$perspective$spatialScale$value else "",
     "resource" = if(!is.null(x$document$distribution$spatialScale$value)) x$document$distribution$spatialScale$value else ""
    ),
    agency = x$document$objectSource$dataProvider$code,
    waterRefs = fetchFactsheetAreaInfo(x, domain)
  )
  
  return(out)
}