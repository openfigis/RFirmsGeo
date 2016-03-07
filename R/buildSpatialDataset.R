#' @name buildSpatialDataset
#' @aliases buildSpatialDataset
#' @title buildSpatialDataset
#' 
#' @description
#' A function to build a spatial dataset for a given factsheet domain.
#'
#' @param host an object of class "character" giving the host
#' @param domain an object of class "character" giving the FIRMS domain
#' @param cleanGeom an object of class "logical" indicating if geometries have to
#'        be validated with \pkg{cleangeo}. Default value is TRUE.
#' @param unionStrategy an object of class "character". Accepted values are "union"
#' (pure geoprocessing union - time consuming -), "bbox" (strategy to estimate the
#' largest spatial object to retain, by comparing envelopes, and results much less
#' time consuming). Default value is "bbox".
#' @param verbose an object of class "logical" either logs have to be printed out.
#'        Default value is TRUE.
#' @param ids a vector of resource IDs for which the computation has to be run
#' @param exportPartialResults if partial shapefiles have to be exported Default
#'        is FALSE.
#' @param exportPath the path were shapefiles should be exported
#' @return an object of class "SpatialPolygonsDataFrame"
#' 
#' @note end-user function
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
buildSpatialDataset <- function(host, domain,
                                cleanGeom = TRUE, unionStrategy = "bbox",
                                verbose = TRUE, ids = NULL,
                                exportPartialResults = FALSE, exportPath = getwd()){
  
  if(!(host %in% c("http://figisapps.fao.org", "http://www.fao.org")))
    stop("Unknown FAO host")
  
  if(!(domain %in% c("resource","fishery")))
    stop("Unknown domain")
  
  #list of items/lang per domain
  domainUrl <- sprintf("%s/figis/ws/factsheets/domain/%s/factsheet", host, domain)
  logger.info(paste0("GET ", domainUrl))
  reqText <- getURL(domainUrl)
  domainXml <- xmlParse(reqText)
  items <- sapply(getNodeSet(domainXml, "//fiws:FactsheetDiscriminator"),
                  xmlGetAttr, "factsheet")
  lang <- sapply(getNodeSet(domainXml, "//fiws:FactsheetDiscriminator"),
                xmlGetAttr, "lang")
  refs <- data.frame(
    factsheet = items,
    lang = lang,
    stringsAsFactors = FALSE
  ) 
  dup <- refs[duplicated(refs$factsheet),"factsheet"]
  refs <- rbind(
    refs[!(refs$factsheet %in% dup),],
    refs[refs$factsheet %in% dup & refs$lang == "en", ] 
  )
  
  if(!is.null(ids)){
    refs <- refs[refs$factsheet %in% ids,]
  }
  
  out.sp <- lapply(1:nrow(refs),
                   function(x){
                     out <- NULL
                     out.points <- NULL
                     
                     #produce polygon dataset
                     out <- buildSpatialObject(refs[x,"factsheet"], refs[x,"lang"], host, domain,
                                               cleanGeom, unionStrategy, verbose)
                     if(exportPartialResults){
                       if(!is.null(out)){
                         if(verbose){
                           logger.info("Exporting polygon output to ESRI shapefile...")
                         }
                         filename <- paste0(domain, "_", refs[x,"factsheet"])
                         exportFeatures(out, file.path = exportPath, file.name = filename)
                       }
                     }
                     
                     #produce pointOnSurface dataset
                     if(!is.null(out)){
                      out.points <- gPointOnSurface(out, byid = TRUE)
                      out.points <- SpatialPointsDataFrame(out.points, data = out@data, match.ID = TRUE)
                      if(exportPartialResults){
                        if(!is.null(out.points)){
                          if(verbose){
                            logger.info("Exporting point output to ESRI shapefile...")
                          }
                          filename <- paste0(domain, "_point_", refs[x,"factsheet"])
                          exportFeatures(out.points, file.path = exportPath, file.name = filename)
                        }
                      }
                     }
                     
                     return(out.points)
                   })
  out.sp <- out.sp[!sapply(out.sp, is.null)]
  
  output.sp <- NULL
  invisible(lapply(1:length(out.sp),function(i){
       if(i==1){
         output.sp <<- out.sp[[i]]
       }else{
         output.sp <<- spRbind(output.sp, out.sp[[i]])
       }
  }))
  
  return(output.sp)
}
