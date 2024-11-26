#' @name buildSpatialDataset
#' @aliases buildSpatialDataset
#' @title buildSpatialDataset
#' 
#' @description
#' A function to build a spatial dataset for a given factsheet domain.
#'
#' @param host an object of class "character" giving the host
#' @param domain an object of class "character" giving the FIRMS domain
#' @param doc a FIRMS inventory doc (retrieved as JSON through web-service)
#' @param ids a vector of resource IDs for which the computation has to be run
#' @param runParallel To run code with parallel R interface. Default is FALSE
#' @param runCores Number of cores to use when parallel = TRUE.
#' @param exportPartialResults if partial shapefiles have to be exported Default
#'        is FALSE.
#' @param exportPath the path were shapefiles should be exported
#' @param verbose an object of class "logical" either logs have to be printed out.
#'        Default value is TRUE.
#' @return an object of class "sf"
#' 
#' @note end-user function
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
buildSpatialDataset <- function(host, domain, doc, ids = NULL,
                                runParallel = FALSE, runCores = 1,
                                exportPartialResults = FALSE, exportPath = getwd(),
                                verbose = TRUE){
  
  if(!(host %in% c("https://fisheries.review.fao.org", "https://www.fao.org")))
    stop("Unknown FAO host")
  
  if(!(domain %in% c("resource","fishery")))
    stop("Unknown domain")
  
  #list of items/lang per domain
  refs = doc
  if(!is.null(ids)){
    refs <- doc[sapply(doc, function(x){x$document$inventoryId %in% ids})]
  }
  
  #wfs client
  wfs_verbose <- ifelse(verbose, "DEBUG", "INFO")
  wfs <- ows4R::WFSClient$new(paste0(host,"/fishery/geoserver/ows"), "1.0.0", logger = wfs_verbose)
  
  #doBuild
  doBuild <- function(ref, domain, wfs, verbose){
    
    capture.output({
    
      out <- NULL
      out.points <- NULL
      
      print(ref$document$inventoryId)
      
      #produce polygon dataset
      out <- buildSpatialObject(ref, domain, wfs, verbose)
      if(exportPartialResults){
        if(!is.null(out)){
          if(verbose){
            logger.info("Exporting polygon output to ESRI shapefile...")
          }
          filename <- paste0(domain, "_", ref$document$inventoryId)
          sf::st_write(out, file.path(exportPath, paste0(filename, ".shp")))
        }
      }
      
      #produce pointOnSurface dataset
      if(!is.null(out)){
        out.points <- sf::st_point_on_surface(out)
        if(exportPartialResults){
          if(!is.null(out.points)){
            if(verbose){
              logger.info("Exporting point output to ESRI shapefile...")
            }
            filename <- paste0(domain, "_point_", ref$document$inventoryId)
            esf::st_write(out, file.path(exportPath, paste0(filename, ".shp")))
          }
        }
      }
    }, file = paste0(paste("log", domain, ref$document$inventoryId, sep="_"), ".txt"))
      
    return(out.points)
  }
  
  if(runParallel){
    out.sf <- parallel::mclapply(
      refs, doBuild, 
      domain, wfs, verbose,
      mc.preschedule = TRUE, mc.cores = runCores
    )
  }else{
    out.sf <- lapply(
      refs, doBuild,
      domain, wfs, verbose
    )
  }
  
  
  out.sf <- out.sf[!sapply(out.sf, is.null)]
  
  output.sf <- NULL
  if(length(out.sf)>0){
    invisible(lapply(1:length(out.sf),function(i){
         if(i==1){
           output.sf <<- out.sf[[i]]
         }else{
           output.sf <<- rbind(output.sf, out.sf[[i]])
         }
    }))
  }
  
  return(output.sf)
}
