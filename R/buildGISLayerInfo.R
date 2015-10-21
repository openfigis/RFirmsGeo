#' @name buildGISLayerInfo
#' @aliases buildGISLayerInfo
#' @title buildGISLayerInfo
#' @export
#' 
#' @description
#' A utility function to build a GIS layer info, using \code{codesystem} and 
#' \code{code} values inherited from a factsheet XML. The layer info consists in
#' a standard named list with all the information required to query a GIS layer, i.e.
#' the \code{url}, \code{typeName}, \code{propertyName}, \code{propertyValue}.
#' 
#' Additional fields, are added respectively to report the info \code{level} 
#' and the \code{rank}. Both fields are required to optimize the query of FAO
#' fishing areas, by using the finest fishing area granularity.
#'
#' @param codesystem an object of class "character"
#' @param code an object of class "character"
#' @return an object of class "list"
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
#'
buildGISLayerInfo <- function(codesystem, code){
  
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
  
  #cases
  #1. fishing areas
  layer <- NULL
  if(codesystem %in% c("fao_major","fao_sub_area",
                       "fao_div","fao_sub_div","fao_sub_unit")){
    layer <- list(
      url = gsUrl,
      typeName = "fifao:FAO_AREAS_ERASE",
      propertyName = "F_CODE",
      propertyValue = code,
      level = infoLevel,
      rank = substr(code,1,2))
  
  #2. species distributions
  }else if(codesystem == "fao3alpha"){
    layer <- list(
      url = gsUrl,
      typeName = sprintf("species:SPECIES_DIST_%s",toupper(code)),
      propertyName = NA,
      propertyValue = NA,
      level = infoLevel,
      rank = NA)
  
  #3. lmes
  }else if(codesystem == "lme"){
    layer <- list(
      url = gsUrl,
      typeName = paste0("fifao:",toupper(codesystem)),
      propertyName = "LME_NUMBER",
      propertyValue = code,
      level = infoLevel,
      rank = NA)
    
  #4. gfcm_sub_area
  }else if(codesystem == "gfcm_sub_area"){
    layer <- list(
      url = gsUrl,
      typeName = paste0("fifao:",toupper(codesystem)),
      propertyName = "SMU_CODE",
      propertyValue = code,
      level = infoLevel,
      rank = NA)
    
  #5. pac_tuna_rep
  }else if(codesystem == "pac_tuna_rep"){
    layer <- list(
      url = gsUrl,
      typeName = paste0("fifao:",toupper(codesystem)),
      propertyName = "REP_AREA",
      propertyValue = code,
      level = infoLevel,
      rank = NA)
  
  #6. rfb_comp
  }else if(codesystem == "rfb_comp"){
    layer <- list(
      url = gsUrl,
      typeName = paste0("fifao:",toupper(codesystem)),
      propertyName = "RFB",
      propertyValue = code,
      level = infoLevel,
      rank = NA)
  
  #other layers (water area refs)
  }else{
    layer <- list(
      url = gsUrl,
      typeName = paste0("fifao:",toupper(codesystem)),
      propertyName = toupper(codesystem),
      propertyValue = code,
      level = infoLevel,
      rank = NA)
  }
  
  layer <- as.data.frame(layer, stringsAsFactors = FALSE)
  return(layer)
}