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
                     if(length(foreignIdXML) > 0) foreignIdXML <- foreignIdXML[sapply(foreignIdXML, function(x) xmlGetAttr(x,"CodeSystem") == "fao3alpha")]
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
#' @return an object of class "character"
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
    stock <- xmlGetAttr(appXML,"BiologicalStock")
    if(stock == "false"){
      stock <- "Marine Resource"
    }else if(stock == "true"){
      stock <- "Biological Stock"
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

#' @name fetchFactsheetAgencyInfo
#' @aliases fetchFactsheetAgencyInfo
#' @title fetchFactsheetAgencyInfo
#' 
#' @description
#' A function to fetch the owner acronym from the factsheet XML. The function
#' tries to grab the acronym of the executing agency. In case the latter is 'FAO',
#' it will try to fetch the corresponding FAO body.
#'
#' @param xml an object of class "XmlInternalDocument"
#' @return an object of class "character"
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
fetchFactsheetAgencyInfo <- function(xml){
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  orgXML <- getNodeSet(xml, "//ns:Owner/ns:CollectionRef/ns:OrgRef[contains(@Role,'Executing_agency')]/ns:ForeignID", c(ns = fiNS))
  org <- as.character(NA)
  if(length(orgXML) > 0){
    org <- xmlGetAttr(orgXML[[1]], "Code")
  }
  
  #in case of FAO, try to inherit FAO body / project
  #-------------------------------------------------
  bodies <- c("CCAMLR","CCSBT","CECAF","GFCM","IATTC","ICCAT","ICES","IOTC","NAFO","SEAFO","SWIOFC","WECAFC", "RECOFI", "BNP")
  #attempt with CreatorCorporate tag
  if(is.na(org) || org == "FAO") {
    agsNS <- "http://www.purl.org/agmes/1.1/"
    corpXML <- getNodeSet(xml, "//ns1:CorporateCoverPage/ns2:CreatorCorporate", c(ns1 = fiNS, ns2 = agsNS))
    if(length(corpXML)>0) {
      creatorTitle <- xmlValue(corpXML[[1]])
      creator <- NULL
      invisible(lapply(bodies,function(x) {if(regexpr(x,creatorTitle)>0) creator <<- x}))
      if(!is.null(creator)) org <- creator
    }
  }
  
  #attempt with dc:Identifier
  if(is.na(org) || org == "FAO") {
    dcNS <- "http://purl.org/dc/elements/1.1/"
    idXML <-  getNodeSet(xml, "//ns1:CorporateCoverPage/ns2:Identifier", c(ns1 = fiNS, ns2 = dcNS))
    if(length(idXML)>0) {
      creatorIdentifier <- xmlValue(idXML[[1]])
      creator <- NULL
      invisible(lapply(bodies,function(x) {if(regexpr(x,creatorIdentifier)>0) creator <<- x}))
      if(!is.null(creator)) org <- creator
    }
  }

  #attempt to set FAO as org in case of UN FAO branch as implementing agency
  #(assumption is made that when a factsheet as an "implementing agency" it is within FAO)
  if(is.na(org)){
    branch <- NULL
    branchXML <- getNodeSet(xml, "//ns:Owner/ns:CollectionRef/ns:OrgRef[contains(@Role,'Implementing_agency')]/ns:ForeignID", c(ns = fiNS))
    if(length(branchXML) > 0) branch <- xmlGetAttr(branchXML[[1]], "Code")
    if(!is.null(branch)) org <- "FAO"
  }
  
  return(org)
}

#' @name fetchFactsheetJurisdictionalInfo
#' @aliases fetchFactsheetJurisdictionalInfo
#' @title fetchFactsheetJurisdictionalInfo
#' 
#' @description 
#' A function to fetch the factsheet jurisdictional dist from a factsheet XML
#' 
#' @param xml
#' @return an object of class "character"
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' 
fetchFactsheetJurisdictionalInfo <- function(xml){
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  out <- xpathSApply(xml, "//fi:FIGISDoc/fi:AqRes/fi:GeoDist/fi:JurisdictionalDist", xmlGetAttr, "Value", namespaces = c(fi = fiNS))
  if(length(out)==0) out <- NA
  return(out)
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
#' @param species Include species information. Default is FALSE
#' @return an object of class "list" giving the factsheet information
#'         (title and georef)
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
fetchFactsheetInfo <- function(factsheet, lang, domain, host, species = FALSE, verbose = TRUE){
  
  if(verbose) logger.info(sprintf("Extracting information for factsheet %s", factsheet))
  
  #namespaces
  fiNS <- "http://www.fao.org/fi/figis/devcon/"
  dcNS <- "http://purl.org/dc/elements/1.1/"
  
  fsUrl <- sprintf("%s/figis/monikerj/figisdoc/%s/%s/%s", host, domain, factsheet, lang)
  logger.info(paste0("GET ", fsUrl))
  out <- NULL
  reqText <- tryCatch({
    userAgent <- paste0("FAO-RFirmsGeo-", packageVersion("RFirmsGeo"))
    reqText <- getURL(fsUrl, httpheader = list('User-Agent' = userAgent), .encoding = "UTF-8") 
    Encoding(reqText) <- "UTF-8"
    reqText
  },error = function(err){
    logger.info(sprintf("Factsheet %s does not exist", factsheet))
    logger.error(err)
    return(NULL)
  })
  
  fsXML <- try(xmlParse(reqText))

  if(class(fsXML)[1]!="try-error"){
  
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
      waterRefs <- waterAreaList
      if(species){
        speciesList <- fetchFactsheetSpeciesInfo(fsXML, domain)
        waterRefs <- rbind(waterAreaList, speciesList)
      }
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
      
      #owner agency
      agency <- fetchFactsheetAgencyInfo(fsXML)
      
      #jurisdictionalDist
      jurisdictionalDist <- fetchFactsheetJurisdictionalInfo(fsXML)
      
      out <- list(
        title = title,
        georef = georef,
        waterRefs = waterRefs,
        category = category,
        agency = agency,
        jurisdictionalDist = jurisdictionalDist 
      )
     
    }
  }else{
    logger.error("Factsheet XML request doesn't return XML content")
    logger.error(reqText)
  }
  
  return(out)
}

#' @name fetchFactsheetReferences
#' @aliases fetchFactsheetReferences
#' @title fetchFactsheetReferences
#' 
#' @description
#' A function to fetch the factsheet references (factsheet id, language)
#'
#' @param host an object of class "character" giving the host
#' @param domain an object of class "character" giving the FIRMS domain
#' @param verbose if logs should be printed out. Default is TRUE
#' @return an object of class "data.frame"
#' 
#' @note function used internally to build FIRMS spatial objects
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
fetchFactsheetReferences <- function(host, domain, verbose = TRUE){
  
  if(verbose) logger.info("Fetching list of factsheet references")
  
  domainName <- switch(domain,
                       "resource" = "firmsmarineresources",
                       "fishery" = "firmsfisheries"
  )
  domainUrl <- sprintf("%s/figis/moniker/%s", host, domainName)
  
  logger.info(paste0("GET ", domainUrl))
  reqText <- httr::content(httr::GET(domainUrl),"text")
  domainXml <- xmlParse(reqText)
  xmlURIList <- sapply(getNodeSet(domainXml, "//arrayitem"), xmlGetAttr, "xmlURI")
  xmlURIList <- xmlURIList[!sapply(xmlURIList, is.null)]
  baseURI <- sprintf("%s/figis/monikerj/figisdoc/%s/", host, domain)
  refs <- as.data.frame(
    do.call("rbind",
            lapply(xmlURIList,
                   function(x){
                     obj <- unlist(strsplit(unlist(strsplit(x, baseURI))[2],"/"))
                     return(obj)
                   }
            )),
    stringsAsFactors = FALSE
  )
  colnames(refs) <- c("factsheet","lang")
  return(refs)
}
