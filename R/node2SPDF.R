node2SPDF <-
function(XML, crs=CRS("+init=epsg:4326")){
  stopifnot("XMLNodeSet" %in% class(XML)| "list" %in% class(XML))
  eltype<-attr(XML, "element")
  if(length(XML)==0)
    return(paste(c("no elements of type", eltype, "recorded"), collapse=" "))
    
  nodesmeta<-getXMLMeta(XML)
  nodescoords<-data.frame(lon=nodesmeta$lon, lat=nodesmeta$lat, row.names=nodesmeta$id)
  ret<- SpatialPointsDataFrame(coords=nodescoords, proj4string=crs,
                                      data=as.data.frame(nodesmeta), match.ID=TRUE)
  ret
}

