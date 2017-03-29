#' @name buildSpatialMetadata
#' @aliases buildSpatialMetadata
#' @title buildSpatialMetadata
#'
#' @description
#' A function to build a spatial metadata for the FIRMS data being processed
#'
#' @param an object of class "SpatialPointsDataFrame
#' @return an object of class "ISOMetadata"
#' 
#' @note end-user function
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#'
buildSpatialMetadata <- function(sp){
  
  domain <- unique(sp$DOMAIN)
  domainName <- switch(domain,
    "resource" = "Marine Resources",
    "fishery" = "Fisheries",
    NULL
  )
  mdId <- paste0("firms-mv-map-", domain)
  mdTitle <- sprintf("FIRMS %s placemarks", domainName)
  mdDate <- Sys.time()
  mdLinkHTML <- sprintf("http://www.fao.org/geonetwork/srv/eng/main.home?uuid=%s", mdId)
  mdLinkXML <- sprintf("http://www.fao.org/geonetwork/srv/eng/csw?service=CSW&request=GetRecordById&Version=2.0.2&elementSetName=full&outputSchema=http://www.isotc211.org/2005/gmd&id=%s", mdId)
  mdLayerTitle <- sprintf("%s_all_points", domain)
  mdLinkWFS <- sprintf("http://www.fao.org/figis/geoserver/firms/ows?SERVICE=WFS&request=GetFeature&version=1.0.0&typeName=%s", mdLayerTitle)
  
  md = ISOMetadata$new()
  md$setFileIdentifier(mdId)
  md$setCharacterSet("utf8")
  md$setLanguage("eng")
  md$setDateStamp(mdDate)
  md$setMetadataStandardName("ISO 19115:2003/19139")
  md$setMetadataStandardVersion("1.0")
  
  #individual point of contact
  #----------------------------
  indRP1 <- ISOResponsibleParty$new()
  indRP1$setIndividualName("FIRMS Secretariat")
  indRP1$setOrganisationName("Fisheries and Resources Monitoring System (FIRMS)")
  indRP1$setRole("pointOfContact")
  contact <- ISOContact$new()
  address <- ISOAddress$new()
  address$setEmail("FIRMS-Secretariat@fao.org")
  contact$setAddress(address)
  res <- ISOOnlineResource$new()
  res$setLinkage("http://firms.fao.org/firms/en")
  res$setName("Fisheries and Resources Monitoring System (FIRMS)")
  contact$setOnlineResource(res)
  indRP1$setContactInfo(contact)
  md$addContact(indRP1)
  indRP2 <- ISOResponsibleParty$new()
  indRP2$setIndividualName("Emmanuel Blondel")
  indRP2$setOrganisationName("FAO - Fisheries and Aquaculture Department (FI). Fisheries and Aquaculture Policy and Resources Division (FIA)")
  indRP2$setPositionName("GIS Scientist - International Consultant in Geographic Information Systems")
  indRP2$setRole("processor")
  contact <- ISOContact$new()
  address <- ISOAddress$new()
  address$setEmail("Emmanuel.Blondel@fao.org")
  contact$setAddress(address)
  res <- ISOOnlineResource$new()
  res$setLinkage("http://www.fao.org/fishery/en")
  res$setName("FAO - Fisheries and Aquaculture Department (FI)")
  contact$setOnlineResource(res)
  indRP2$setContactInfo(contact)
  md$addContact(indRP2)
  
  #VectorSpatialRepresentation
  #---------------------------
  vsr <- ISOVectorSpatialRepresentation$new()
  vsr$setTopologyLevel("geometryOnly")
  geomObject <- ISOGeometricObjects$new()
  geomObject$setGeometricObjectType("point")
  geomObject$setGeometricObjectCount(nrow(sp))
  vsr$setGeometricObjects(geomObject)
  md$setSpatialRepresentationInfo(vsr)
  
  #ReferenceSystem
  #----------------
  rs <- ISOReferenceSystem$new()
  rs$setReferenceSystemIdentifier(code = "http://www.opengis.net/def/crs/EPSG/0/4326", codeSpace = "EPSG")
  md$setReferenceSystemInfo(rs)
  
  #data identification
  #--------------------
  ident <- ISODataIdentification$new()
  ident$setAbstract("Information on state of stocks and fishery status are provided by the FIRMS partners and is published in the form of fact sheets.")
  ident$setPurpose("The primary aim of the Fisheries and Resources Monitoring System (FIRMS) is to provide access to a wide range of high-quality information on the global monitoring and management of fishery marine resources.")
  ident$setLanguage("eng")
  ident$setCharacterSet("utf8")
  ident$addTopicCategory("biota")
  ident$addTopicCategory("economy")
  
  #adding a point of contact
  ident$addPointOfContact(indRP1)
  ident$addPointOfContact(indRP2)
  
  #citation
  ct <- ISOCitation$new()
  ct$setTitle(mdTitle)
  d <- ISODate$new()
  d$setDate(mdDate)
  d$setDateType("revision")
  ct$addDate(d)
  ct$setEdition("1.0")
  ct$setEditionDate(as.Date(mdDate))
  ct$setIdentifier(mdId)
  ct$setPresentationForm("mapDigital")
  
  #organization contact
  orgRP <- ISOResponsibleParty$new()
  orgRP$setOrganisationName("FAO - Fisheries and Aquaculture Department (FI).")
  orgRP$setRole("owner")
  contact <- ISOContact$new()
  address <- ISOAddress$new()
  address$setDeliveryPoint("Viale delle Terme di Caracalla")
  address$setCity("Rome")
  address$setPostalCode("00153")
  address$setCountry("Italy")
  contact$setAddress(address)
  res <- ISOOnlineResource$new()
  res$setLinkage("http://www.fao.org/fishery/en")
  res$setName("FAO - Fisheries and Aquaculture Department (FI)")
  contact$setOnlineResource(res)
  orgRP$setContactInfo(contact)
  ct$setCitedResponsibleParty(orgRP)
  ident$setCitation(ct)
  
  #graphic overview
  go <- ISOBrowseGraphic$new(
    fileName = sprintf("http://www.fao.org/figis/geoserver/wms?service=WMS&version=1.1.0&request=GetMap&layers=firms:%s,fifao:UN_CONTINENT2&styles=&bbox=-180,-90,180,90&width=771&height=330&srs=EPSG:4326&format=image/png", mdLayerTitle),
    fileDescription = "Map Overview",
    fileType = "image/png"
  )
  ident$setGraphicOverview(go)
  
  #maintenance information
  mi <- ISOMaintenanceInformation$new()
  mi$setMaintenanceFrequency("asNeeded")
  ident$setResourceMaintenance(mi)
  
  #adding legal constraints
  lc <- ISOLegalConstraints$new()
  lc$addUseLimitation("The terms and conditions are available at http://www.fao.org/contact-us/terms/en")
  lc$addUseLimitation(sprintf("Usabe subject to mandatory citation: © FAO, %s. %s. In: FAO - Fisheries and Aquaculture Department (FI) [online]. Rome. Updated %s [Cited <DATE>] %s",
                      format(mdDate, "%Y"), mdTitle, format(mdDate, "%Y-%m-%d"), mdLinkHTML))
  lc$addUseLimitation("Disclaimer: The designations employed and the presentation of material in the map(s) are for illustration only and do not imply the expression of any opinion whatsoever on the part of FAO concerning the legal or constitutional status of any country, territory or sea area or concerning the delimitation of frontiers or boundaries")
  lc$addAccessConstraint("copyright")
  lc$addAccessConstraint("license")
  lc$addUseConstraint("copyright")
  lc$addUseConstraint("license")
  ident$setResourceConstraints(lc)
  
  #adding extent
  extent <- ISOExtent$new()
  bbox <- ISOGeographicBoundingBox$new(bbox = bbox(sp))
  extent$setGeographicElement(bbox)
  ident$setExtent(extent)
  
  #add keywords
  kwds <- ISOKeywords$new()
  kwds$addKeyword("FIRMS")
  kwds$addKeyword("FIGIS")
  if(domain == "resource"){
    kwds$addKeyword("marine resource")
  }else if(domain == "fishery"){
    kwds$addKeyword("fishery")
  }
  kwds$addKeyword("stock")
  kwds$setKeywordType("theme")
  th <- ISOCitation$new()
  th$setTitle("General")
  th$addDate(d)
  kwds$setThesaurusName(th)
  ident$addKeywords(kwds)
  
  md$setIdentificationInfo(ident)
  
  #Distribution
  #---------------
  distrib <- ISODistribution$new()
  dto <- ISODigitalTransferOptions$new()  
  #add WMS
  wms <- ISOOnlineResource$new()
  wms$setLinkage("http://www.fao.org/figis/geoserver/firms/ows?SERVICE=WMS")
  wms$setName(mdLayerTitle)
  wms$setDescription(mdTitle)
  wms$setProtocol("OGC:WMS-1.3.0-http-get-map")
  dto$addOnlineResource(wms)
  #add WFS - GML
  wfs1 <- ISOOnlineResource$new()
  wfs1$setLinkage(mdLinkWFS)
  wfs1$setName(mdLayerTitle)
  wfs1$setDescription("GIS data download (WFS - GML)")
  wfs1$setProtocol("WWW:LINK-1.0-http--link")
  dto$addOnlineResource(wfs1)
  #add WFS - ESRI Shapefile
  wfs2 <- ISOOnlineResource$new()
  wfs2$setLinkage(paste0(mdLinkWFS, "&outputFormat=SHAPE-ZIP&format_options=filename:firms-", mdLayerTitle, ".zip"))
  wfs2$setName(mdLayerTitle)
  wfs2$setDescription("GIS data download (WFS - ESRI Shapefile)")
  wfs2$setProtocol("WWW:LINK-1.0-http--link")
  dto$addOnlineResource(wfs2)
  #add CSW - XML
  csw <- ISOOnlineResource$new()
  csw$setLinkage(mdLinkXML)
  csw$setName("XML")
  csw$setDescription("metadata (XML)")
  csw$setProtocol("WWW:LINK-1.0-http--link")
  dto$addOnlineResource(csw)
  #add FIRMS viewer link
  mv <- ISOOnlineResource$new()
  mv$setLinkage(sprintf("http://firms.fao.org/firms/stocks-fisheries-map-viewer?layer=%s", domain))
  mv$setName("Map viewer")
  mv$setDescription("FIRMS Stocks and Fisheries map viewer")
  mv$setProtocol("WWW:LINK-1.0-http--link")
  dto$addOnlineResource(mv)
  #add FIRMS link
  firms <- ISOOnlineResource$new()
  firms$setLinkage("http://firms.fao.org")
  firms$setName("FIRMS")
  firms$setDescription("")
  firms$setProtocol("WWW:LINK-1.0-http--link")
  dto$addOnlineResource(firms)
  
  distrib$setDigitalTransferOptions(dto)
  md$setDistributionInfo(distrib)
  
  #Data Quality
  #-------------
  dq <- ISODataQuality$new()
  #add scope
  scope <- ISOScope$new()
  scope$setLevel("dataset")
  dq$setScope(scope)
  #add report
  dc <- ISODomainConsistency$new()
  result <- ISOConformanceResult$new()
  spec <- ISOCitation$new()
  spec$setTitle("Commission Regulation (EU) No 1089/2010 of 23 November 2010 implementing Directive 2007/2/EC of the European Parliament and of the Council as regards interoperability of spatial datasets and services")
  d1 <- ISODate$new()
  d1$setDate(ISOdate(2012, 12, 08))
  d1$setDateType("publication")
  spec$addDate(d1)
  d2 <- ISODate$new()
  d2$setDate(ISOdate(2013,12,10))
  d2$setDateType("revision")
  spec$addDate(d2)
  result$setSpecification(spec)
  result$setExplanation("The conformity to the above INSPIRE data specification has not been fully evaluated. In the absence of an explicit way to specify in ISO 19115 such absence of full evaluation, or eventual intermediate degree of conformity, and in order to fulfill the INSPIRE metadata validation, a conformity metadata element has been added (with the degree arbitrarily set to 'false')")
  result$setPass(FALSE)
  dc$addResult(result)
  dq$addReport(dc)
  #add lineage
  lineage <- ISOLineage$new()
  monikerLink <- switch(domain,
                        "resource" = "http://www.fao.org/figis/moniker/firmsmarineresources",
                        "fishery" = "http://www.fao.org/figis/moniker/firmsfisheries"
  )
  statement <- paste0("Methodology: The dataset of ", domainName," here described has been derivated from the factsheets (listed at ", monikerLink,") and its geographic standard references. ",
                       "Factsheet geographic references were fetched through OGC Web Feature Services (WFS). The was performed through a 'bounding-box' to estimate ",
                       "the largest bounding box to retain, by comparing geographic reference envelopes. All the tasks were performed through the R package ", 
                       "'RFirmsGeo' (more information at http://github.com/openfigis/RFirmsGeo) developed by the FAO Fisheries & Aquaculture Department, ",
                      "and benefiting from the computing resources of the D4Science infrastructure (see https://www.d4science.org). ",
                      "Data and the present metadata description were published (updated) on ",
                       format(mdDate, "%Y-%m-%d")," from R using the suite of standard packages ",
                       "'geosapi' (see https://github.com/eblondel/geosapi) for data publication, ",
                       "'geometa' (see https://github.com/eblondel/geometa) for metadata creation, ",
                       "'geonapi' (see https://github.com/eblondel/geonapi) for metadata publication."
                     )
  lineage$setStatement(statement)
  dq$setLineage(lineage)
  md$setDataQualityInfo(dq)
  
  return(md)
}
