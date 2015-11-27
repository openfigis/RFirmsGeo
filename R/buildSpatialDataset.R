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
buildSpatialDataset <- function(host, domain, cleanGeom = TRUE, verbose = TRUE,
                                ids = NULL, exportPartialResults = FALSE,
                                exportPath = getwd()){
  
  if(!(host %in% c("http://figisapps.fao.org", "http://www.fao.org")))
    stop("Unknown FAO host")
  
  if(!(domain %in% c("resource","fishery")))
    stop("Unknown domain")
  
  #list of items/lang per domain
  domainUrl <- sprintf("%s/figis/ws/factsheets/domain/%s/factsheet", host, domain)
  req <- GET(domainUrl)
  stop_for_status(req)
  domainXml <- content(req)
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
                     out <- buildSpatialObject(refs[x,"factsheet"], refs[x,"lang"], host, domain,
                                               cleanGeom, verbose)
                     if(exportPartialResults){
                       if(!is.null(out)){
                         if(verbose){
                           message("Exporting to ESRI shapefile...")
                         }
                         filename <- paste0(domain, "_", refs[x,"factsheet"])
                         exportFeatures(out, file.path = exportPath, file.name = filename)
                       }
                     }
                     return(out)
                   })
  out.sp <- out.sp[!sapply(out.sp, is.null)]
  out.sp <- do.call("spRbind", out.sp)
  
  #adding domain as attribute
  if(!is.null(out.sp)){
    out.sp@data <- cbind(DOMAIN = rep(domain,nrow(out.sp@data)),
                        out.sp@data, stringsAsFactors = FALSE)
  }
  
  return(out.sp)
}
