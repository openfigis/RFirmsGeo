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
#' @param xml an object of class "XmlInternalDocument"
#' @return an object of class "data.frame" listing the GIS water area layer infos
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
fetchFactsheetAreaInfo <- function(xml){
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  waterAreaRefXML <- getNodeSet(xml, "//ns:WaterAreaList/ns:WaterAreaRef", c(ns = fiNS))
  waterAreaList = as.data.frame(
    do.call("rbind",
            lapply(waterAreaRefXML,
                   function(x){
                     out <- NULL
                     foreignIdXML <- getNodeSet(xmlDoc(x), "//ns:ForeignID", c(ns = fiNS))
                     if(length(foreignIdXML) > 0){
                       foreignIdXML <- foreignIdXML[[1]]
                       out <- buildGISLayerInfo("WaterArea",xmlGetAttr(foreignIdXML, "CodeSystem"), xmlGetAttr(foreignIdXML, "Code"))
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
  }else{
    #look into land areas
    landAreaRefXML <- getNodeSet(xml, "//ns:LandAreaList/ns:LandAreaRef", c(ns = fiNS))
    landAreaList = as.data.frame(
      do.call("rbind",
              lapply(landAreaRefXML,
                     function(x){
                       out <- NULL
                       foreignIdXML <- getNodeSet(xmlDoc(x), "//ns:ForeignID", c(ns = fiNS))
                       if(length(foreignIdXML) > 0){
                         for(i in 1:length(foreignIdXML)){                         
                          if(xmlGetAttr(foreignIdXML[[i]], "CodeSystem") == "iso3"){
                            foreignIdXML = foreignIdXML[[i]]
                            break
                          }
                         } 
                         out <- buildGISLayerInfo("LandArea",xmlGetAttr(foreignIdXML, "CodeSystem"), xmlGetAttr(foreignIdXML, "Code"))
                       }
                       return(out)
                     })),
      stringsAsFactors = FALSE
    )
    
    if(nrow(landAreaList) > 0){
      landAreaList <- unique(landAreaList)
      waterAreaList <- landAreaList
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
#' @param domain an object of class "character" the FIRMS domain
#' @return an object of class "data.frame" listing the GIS species distributions 
#'         layer infos
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
fetchFactsheetSpeciesInfo <- function(xml, domain){
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  speciesRefXpath <- switch(domain,
                    "resource" = "//ns:SpeciesRef",
                    "fishery" = "//ns:TargetSpecies/ns:SpeciesList/ns:SpeciesRef",
                    NULL)
  speciesRefXML <- getNodeSet(xml, speciesRefXpath, c(ns = fiNS))
  speciesList = as.data.frame(
    do.call("rbind",
            lapply(speciesRefXML,
                   function(x){
                     out <- NULL
                     foreignIdXML <- getNodeSet(xmlDoc(x), "//ns:ForeignID", c(ns = fiNS))
                     foreignIdXML <- foreignIdXML[sapply(foreignIdXML, function(x) xmlGetAttr(x,"CodeSystem") == "fao3alpha")]
                     if(length(foreignIdXML) > 0){
                       foreignIdXML <- foreignIdXML[[1]]
                       out <- buildGISLayerInfo("SpeciesDistribution",xmlGetAttr(foreignIdXML, "CodeSystem"), xmlGetAttr(foreignIdXML, "Code"))
                     }
                     return(out)
                   })),
    stringsAsFactors = FALSE
  )
  
  return(speciesList)
}


#' @name fetchFactsheetThematicApproachInfo
#' @aliases fetchFactsheetThematicApproachInfo
#' @title fetchFactsheetThematicApproachInfo
#' 
#' @description
#' A function to fetch the thematic approach information from the \code{ThematicApproach} 
#' node of a factsheet XML.
#'
#' @param xml an object of class "XmlInternalDocument"
#' @return an object of class "list"
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
fetchFactsheetThematicApproachInfo <- function(xml){
  
  approach<- as.character(NA)
  
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  dcNS <- "http://purl.org/dc/elements/1.1/"
  
  appXML <- getNodeSet(xml, paste0("//fi:ThematicApproach"), c(fi = fiNS))
  if(length(appXML) > 0){
    appXML <- appXML[[1]]
    approach <- xmlGetAttr(appXML,"Value")
  }

  return(approach)
}


#' @name fetchFactsheetBiologicalStockInfo
#' @aliases fetchFactsheetBiologicalStockInfo
#' @title fetchFactsheetBiologicalStockInfo
#' 
#' @description
#' A function to fetch the stock information from the \code{AqResStruct} 
#' node / \code{BiologicalStock} attribute of a factsheet XML.
#'
#' @param xml an object of class "XmlInternalDocument"
#' @return an object of class "list"
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
fetchFactsheetStockInfo <- function(xml){
  
  stock<- as.character(NA)
  
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  dcNS <- "http://purl.org/dc/elements/1.1/"
  
  appXML <- getNodeSet(xml, paste0("//fi:AqResStruct"), c(fi = fiNS))
  if(length(appXML) > 0){
    appXML <- appXML[[1]]
    stock <- xmlGetAttr(appXML,"Biological Stock")
    if(stock == "false"){
      stock <- NA
    }else{
      stock <- "BiologicalStock"
    }
  }
  
  return(stock)
}


#' @name fetchFactsheetGeorefInfo
#' @aliases fetchFactsheetGeorefInfo
#' @title fetchFactsheetGeorefInfo
#' 
#' @description
#' A function to fetch the georeference information from the \code{Georeference} 
#' node of a factsheet XML. Information is presented as named list, containing
#' the scale and title of the georeference.
#'
#' @param xml an object of class "XmlInternalDocument"
#' @param domain an object of class "character representing the FIRMS resource
#' @return an object of class "list"
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
fetchFactsheetGeorefInfo <- function(xml, domain){
  
  scale <- as.character(NA)
  title <- as.character(NA)
  
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  dcNS <- "http://purl.org/dc/elements/1.1/"
  
  georefElement <- switch(domain,
                  "fishery" = "GeoReference",
                  "resource" = "WaterAreaOverview",
                  NULL)
  georefXML <- getNodeSet(xml, paste0("//fi:",georefElement), c(fi = fiNS))
  if(length(georefXML) > 0){
    georefXML <- georefXML[[1]]
    scaleXML <- getNodeSet(xmlDoc(georefXML), "//fi:SpatialScale", c(fi = fiNS))
    if(length(scaleXML) > 0) scale <- xmlGetAttr(scaleXML[[1]], "Value")
    titleXML <- getNodeSet(xmlDoc(georefXML), "//dc:Title", c(dc = dcNS))
    if(length(titleXML) > 0) title <- xmlValue(titleXML[[1]])
  }
 
  georef <- list(scale = scale, title = title)
  return(georef)
}

#' @name fetchFactsheetIndicatorInfo
#' @aliases fetchFactsheetIndicatorInfo
#' @title fetchFactsheetIndicatorInfo
#' 
#' @description
#' A function to fetch statistical indicator information
#'
#' @param xml an object of class "XmlInternalDocument"
#' @param indicator an object of class "character representing the xml indicator node
#' @return an object of class "list"
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
fetchFactsheetIndicatorInfo <- function(xml, indicator){
  
  indvalue <- as.character(NA)
  
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  
  indXML = getNodeSet(xml, paste0("//ns:", indicator), c(ns = fiNS))
  if(length(indXML) > 0){
    indvalue <- xmlGetAttr(indXML[[1]], "Value") 
  }
  
  ind <- list(key = indicator, value = indvalue)
  return(ind)
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
  
  if(verbose) logger.info(sprintf("Extracting information for factsheet %s", factsheet))
  
  #namespaces
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  dcNS <- "http://purl.org/dc/elements/1.1/"
  
  fsUrl <- sprintf("%s/figis/ws/factsheets/domain/%s/factsheet/%s/language/%s", host, domain, factsheet, lang)
  logger.info(paste0("GET ", fsUrl))
  out <- NULL
  fsXML <- tryCatch({
    reqText <- getURL(fsUrl)
    Encoding(reqText) <- "UTF-8"
    fsXML <- xmlParse(reqText)
  },error = function(err){
    logger.info(sprintf("Factsheet %s does not exist", factsheet))
    return(NULL)
  })
  if(!is.null(fsXML)){
  
    xpathIdent <- switch(domain,  	
                        "resource" = "AqResIdent",		
                        "fishery" = "FisheryIdent",		
                        NULL)
    
    titleXML <- getNodeSet(fsXML, sprintf("//fi:%s/dc:Title",xpathIdent), c(fi = fiNS, dc = dcNS))
    isValidFactsheetXML <- length(titleXML) > 0
    if(isValidFactsheetXML) {
      
      #get factsheet title
      title <- xmlValue(titleXML[[1]])
      
      #georeference title
      georef <- fetchFactsheetGeorefInfo(fsXML, domain)
      
      #thematic approach
      category <- switch(domain,
             "fishery" = fetchFactsheetThematicApproachInfo(fsXML),
             "resource"= fetchFactsheetStockInfo(fsXML),
             NA
      )
      
      #water references
      waterAreaList <- fetchFactsheetAreaInfo(fsXML)
      speciesList <- fetchFactsheetSpeciesInfo(fsXML, domain)
      waterRefs <- rbind(waterAreaList, speciesList)
      if(!is.null(waterRefs)){
        if(nrow(waterRefs) > 0){
          waterRefs <- waterRefs[order(waterRefs$weight, decreasing = TRUE),]
          waterRefs <- cbind(rep(factsheet, nrow(waterRefs)), waterRefs, stringsAsFactors = FALSE)
          colnames(waterRefs) <- c("FigisID", "category","url", "typeName",
                                   "propertyName", "propertyValue",
                                   "level", "rank")
          if(verbose){
            logger.info("List of geographic references")
            print(waterRefs)
          }
        }
      }
      
      out <- list(
        title = title,
        georef = georef,
        waterRefs = waterRefs,
        category = category
      )
     
    }
  }	
  
  return(out)
}
