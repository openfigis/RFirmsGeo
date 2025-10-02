#'getTabularTemplate
#'@title getTabularTemplate
#'@description Produces a tabular data based on current FIRMS fishery/resource JSON web-service response
#'It aims to explain which field are used and expected to process FIRMS fishery/resource GIS placemarks used 
#'in the FIRMS stocks & fisheries map viewer.
#'This function is a temporary utility to illustrate the data structure that will be used in transition to dismiss 
#'the JSON approach followed now to fetch factsheet information.
#'@param data a object of class \link{list} read from a JSON
#'@param domain the FIRMS domain either 'fishery' or 'resource'
#'@return an object of class \link{data.frame} giving the expected tabular data structure
#'@export
getTabularTemplate = function(data, domain = c("fishery", "resource")){
  
  domain = match.arg(domain)
  
  data_structure = do.call(rbind, lapply(data, function(x){
    out_rec = data.frame(
      DOMAIN = domain,
      CATEGORY = switch(domain,
                        "fishery" = stringr::str_to_title(gsub("_", " ", x$document$perspective$thematicApproach$value)),
                        "resource" = {
                          if(!is.null(x$document$structure$singleStock)){
                            if(x$document$structure$singleStock) "Biological Stock" else "Marine Resource"
                          }else{
                            "Marine Resource"
                          }
                        } 
      ),
      FIGIS_ID = x$document$inventoryId,
      INV_OBS_ID = x$document$invObsId,
      OLD_ID = if(!is.null(x$document$figisId)) x$document$figisId else "",
      LANG = x$document$language,
      TITLE = x$document$title,
      GEOREF = switch(domain,
                      "fishery" = x$document$perspective$geoRefTitle,
                      "resource" = if(!is.null(x$document$distribution$areaName)) x$document$distribution$areaName else ""
      ),
      SCALE = switch(domain,
                     "fishery" = if(!is.null(x$document$perspective$spatialScale$value)) x$document$perspective$spatialScale$value else "",
                     "resource" = if(!is.null(x$document$distribution$spatialScale$value)) x$document$distribution$spatialScale$value else ""
      ),
      AGENCY = x$document$objectSource$dataProvider$code,
      AREA_TYPE = switch(domain,
                         "fishery" = {
                           area_type = "WaterArea"
                           areas = list() 
                           if(length(x$document$fishingarea$fisheryPrimaryAreaCodes)>0) areas = sapply(x$document$fishingarea$fisheryPrimaryAreaCodes, function(area){area$value})
                           if(length(areas)==0 & length(x$document$perspective$geoRefLandarea)>0){
                             area_type = "LandArea" 
                           }
                           area_type
                         },
                         "resource" = "WaterArea"
      ),
      AREA_CODES = switch(domain,
                          "fishery" = {
                            areas = list() 
                            if(length(x$document$fishingarea$fisheryPrimaryAreaCodes)>0) areas = sapply(x$document$fishingarea$fisheryPrimaryAreaCodes, function(area){area$value})
                            if(length(areas)==0 & length(x$document$perspective$geoRefLandarea)>0){
                              areas = x$document$perspective$geoRefLandarea$value #no figisId
                            }
                            paste0(areas, collapse = ",")
                          },
                          "resource" = paste0(sapply(x$document$distribution$primaryArea, function(area){area$value}), collapse=",")
      ),
      stringsAsFactors = FALSE
    )
    return(out_rec)
  }))
  
  return(data_structure)
}