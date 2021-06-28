#' @name buildGISLayerInfo
#' @aliases buildGISLayerInfo
#' @title buildGISLayerInfo
#' 
#' @description
#' A utility function to build a GIS layer info, using \code{codesystem} and 
#' \code{code} values inherited from a factsheet XML. The layer info consists in
#' a standard named list with all the information required to query a GIS layer, i.e.
#' the \code{url}, \code{typeName}, \code{propertyName}, \code{propertyValue}.
#' 
#' Additional fields, are added to report:
#' - the info \code{level} and the \code{rank}. Both fields are required to 
#' optimize the query of FAO fishing areas, by using the finest fishing area
#' granularity.
#' - a \code{weight} specific to each layer. The weight is reversely proportional
#' to the geographic extent of the layer features. Maximum weight is attributed
#' to EEZs. In case of no overlap, or intersection failure, the feature that will
#' be kept to georeference a resource will be the one with the highest weight 
#' (ie highest resolution).
#'
#' @param category an object of class "character" describing the category
#'        Possible values: "WaterArea", "SpeciesDistribution"
#' @param codesystem an object of class "character"
#' @param code an object of class "character"
#' @return an object of class "list"
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
#'
buildGISLayerInfo <- function(category, codesystem, code){
  
  gsUrl <- "http://www.fao.org/figis/geoserver/ows" 
  
  #infoLevel (applies for fishing areas)
  infoLevel <- switch(codesystem,
                      "fao_major" = 1,
                      "fao_sub_area" = 2,
                      "fao_div" = 3,
                      "fao_sub_div" = 4,
                      "fao_sub_unit" = 5,
                      NA	
  )
  fishingareaWeight <- infoLevel
  
  #cases
  #1. fishing areas
  layer <- NULL
  if(codesystem %in% c("fao_major","fao_sub_area",
                       "fao_div","fao_sub_div","fao_sub_unit")){
    layer <- list(
      url = gsUrl,
      typeName = "fifao:FAO_AREAS_LOWRES",
      propertyName = "F_CODE",
      propertyValue = code,
      level = infoLevel,
      rank = substr(code,1,2),
      weight = fishingareaWeight)
  
  #2. species distributions
  }else if(codesystem == "fao3alpha"){
    layer <- list(
      url = gsUrl,
      typeName = sprintf("species:SPECIES_DIST_%s",toupper(code)),
      propertyName = NA,
      propertyValue = NA,
      level = infoLevel,
      rank = NA,
      weight = 0)
  
  #3. lmes
  }else if(codesystem == "lme"){
    layer <- list(
      url = gsUrl,
      typeName = paste0("fifao:",toupper(codesystem)),
      propertyName = "LME_NUMBER",
      propertyValue = code,
      level = infoLevel,
      rank = NA,
      weight = 6)
    
  #4. gfcm_sub_area
  }else if(codesystem == "gfcm_sub_area"){
    layer <- list(
      url = gsUrl,
      typeName = paste0("fifao:",toupper(codesystem)),
      propertyName = "SMU_CODE",
      propertyValue = code,
      level = infoLevel,
      rank = NA,
      weight = 2)
    
  #5. pac_tuna_rep
  }else if(codesystem == "pac_tuna_rep"){
    layer <- list(
      url = gsUrl,
      typeName = paste0("fifao:",toupper(codesystem)),
      propertyName = "REP_AREA",
      propertyValue = code,
      level = infoLevel,
      rank = NA,
      weight = 2)
  
  #6. rfb_comp
  }else if(codesystem == "rfb_comp"){
    layer <- list(
      url = gsUrl,
      typeName = paste0("fifao:",toupper(codesystem)),
      propertyName = "RFB",
      propertyValue = code,
      level = infoLevel,
      rank = NA,
      weight = 1)
  
  #7. eez (nja)  
  }else if(codesystem == "eez"){
    layer <- list(
      url = gsUrl,
      typeName = "fifao:NJA",
      propertyName = "ISO3",
      propertyValue = code,
      level = infoLevel,
      rank = NA,
      weight = 10)
  
  #8. land areas (for the timebeing refer to corresponding EEZ)
  }else if(codesystem == "iso3"){
    layer <- list(
      url = gsUrl,
      typeName = "fifao:NJA",
      propertyName = "ISO3",
      propertyValue = code,
      level = infoLevel,
      rank = NA,
      weight = 10)
    
  #other layers (water area refs)
  }else{
    layer <- list(
      url = gsUrl,
      typeName = paste0("fifao:",toupper(codesystem)),
      propertyName = toupper(codesystem),
      propertyValue = code,
      level = infoLevel,
      rank = NA,
      weight = 1)
  }
  
  layer <- c(category = category, layer)
  layer <- as.data.frame(layer, stringsAsFactors = FALSE)
  return(layer)
}