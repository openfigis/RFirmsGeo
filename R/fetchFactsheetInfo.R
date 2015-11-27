#' @name fetchFactsheetAreaInfo
#' @aliases fetchFactsheetAreaInfo
#' @title fetchFactsheetAreaInfo
#' 
#' @description
#' A function to fetch the water area information from the \code{waterAreaRef} 
#' node of a factsheet XML.
#'
#' @param xml an object of class "XmlInternalDocument"
#' @return an object of class "data.frame" listing the GIS water area layer infos
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
fetchFactsheetAreaInfo <- function(xml){
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  waterAreaRefXML <- getNodeSet(xml, "//ns:WaterAreaRef", c(ns = fiNS))
  waterAreaList = as.data.frame(
    do.call("rbind",
            lapply(waterAreaRefXML,
                   function(x){
                     out <- NULL
                     foreignIdXML <- getNodeSet(xmlDoc(x), "//ns:ForeignID", c(ns = fiNS))
                     if(length(foreignIdXML) > 0){
                       foreignIdXML <- foreignIdXML[[1]]
                       out <- buildGISLayerInfo(xmlGetAttr(foreignIdXML, "CodeSystem"), xmlGetAttr(foreignIdXML, "Code"))
                     }
                     return(out)
                   })),
    stringsAsFactors = FALSE
  )
  
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

#' @name fetchFactsheetSpeciesInfo
#' @aliases fetchFactsheetSpeciesInfo
#' @title fetchFactsheetSpeciesInfo
#' 
#' @description
#' A function to fetch the species information from the \code{SpeciesRef} node of
#' a factsheet XML.
#'
#' @param xml an object of class "XmlInternalDocument"
#' @return an object of class "data.frame" listing the GIS species distributions 
#'         layer infos
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
fetchFactsheetSpeciesInfo <- function(xml){
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  speciesRefXML <- getNodeSet(xml, "//ns:SpeciesRef", c(ns = fiNS))
  speciesList = as.data.frame(
    do.call("rbind",
            lapply(speciesRefXML,
                   function(x){
                     out <- NULL
                     foreignIdXML <- getNodeSet(xmlDoc(x), "//ns:ForeignID", c(ns = fiNS))
                     foreignIdXML <- foreignIdXML[sapply(foreignIdXML, function(x) xmlGetAttr(x,"CodeSystem") == "fao3alpha")]
                     if(length(foreignIdXML) > 0){
                       foreignIdXML <- foreignIdXML[[1]]
                       out <- buildGISLayerInfo(xmlGetAttr(foreignIdXML, "CodeSystem"), xmlGetAttr(foreignIdXML, "Code"))
                     }
                     return(out)
                   })),
    stringsAsFactors = FALSE
  )
  
  return(speciesList)
}

#' @name fetchFactsheetGeorefInfo
#' @aliases fetchFactsheetGeorefInfo
#' @title fetchFactsheetGeorefInfo
#' 
#' @description
#' A function to fetch the georeference information from the \code{Georeference} 
#' node of a factsheet XML.
#'
#' @param xml an object of class "XmlInternalDocument"
#' @return an object of class "character"
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
fetchFactsheetGeorefInfo <- function(xml){
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  dcNS <- "http://purl.org/dc/elements/1.1/"
  georefXML <- getNodeSet(xml, "//fi:GeoReference/dc:Title", c(fi = fiNS, dc = dcNS))
  georef <- xmlValue(georefXML[[1]])
  return(georef)
}


#' @name fetchFactsheetInfo
#' @aliases fetchFactsheetInfo
#' @title fetchFactsheetInfo
#' 
#' @description
#' A function to fetch the factsheet information from a factsheet XML
#'
#' @param factsheet an object of class "character" giving the factsheet FIGIS id
#' @param lang the language in which the factsheet is available
#' @param domain an object of class "character" giving the FIRMS domain
#' @param host an object of class "character" giving the host
#' @param verbose if logs should be printed out. Default is TRUE
#' @return an object of class "list" giving the factsheet information
#'         (title and georef)
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
fetchFactsheetInfo <- function(factsheet, lang, domain, host, verbose = TRUE){
  
  if(verbose) message(sprintf("-> Extracting information for factsheet %s", factsheet))
  
  #namespaces
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  dcNS <- "http://purl.org/dc/elements/1.1/"
  
  #xpath domain identity
  xpathIdent <- switch(domain,
                       "resource" = "AqRes/AqResIdent",
                       "fishery" = "Fishery/FisheryIdent",
                       NULL)
  
  fsUrl <- sprintf("%s/figis/ws/factsheets/domain/%s/factsheet/%s/language/%s/xpath/FIGISDoc/%s", host, domain, factsheet, lang, xpathIdent)
  print(fsUrl)
  req <- GET(fsUrl)
  out <- NULL
  if(http_status(req)$category != "success") {
    message(sprintf("Factsheet %s does not exist", factsheet))
  }else{
    fsXML <- content(req)
    
    titleXML <- getNodeSet(fsXML, "//ns:Title", c(ns = dcNS))
    isValidFactsheetXML <- length(titleXML) > 0
    if(isValidFactsheetXML) {
      
      #get factsheet title
      title <- xmlValue(titleXML[[1]])
      
      #georeference title
      georef <- fetchFactsheetGeorefInfo(fsXML)
      
      #water references
      waterAreaList <- fetchFactsheetAreaInfo(fsXML)
      speciesList <- fetchFactsheetSpeciesInfo(fsXML)
      waterRefs <- rbind(waterAreaList, speciesList)
      waterRefs <- waterRefs[order(waterRefs$weight, decreasing = TRUE),]
      
      out <- list(
        title = title,
        georef = georef,
        waterRefs = waterRefs
      )
    }
  }	
  
  return(out)
}
