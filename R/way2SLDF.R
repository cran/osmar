way2SLDF <-
function(XML, nodedata, crs=CRS("+init=epsg:4326")){

  stopifnot("XMLNodeSet" %in% class(XML)| "list" %in% class(XML))
  stopifnot(attr(XML, "element")=="way")
  eltype<-attr(XML, "element")
  if(length(XML)==0)
    return(paste(c("no elements of type", eltype, "recorded"), collapse=" "))
  
  waynodelist<-vector("list", length(XML))
  for( i in 1:length(XML)){
    temp<-sapply(getNodeSet(XML[[i]], path="nd"), xmlGetAttr, "ref") 
    waynodelist[[i]]<- temp[which(temp %in% row.names(nodedata)==TRUE)]  
  }   

  wayIDs<-sapply(XML, xmlGetAttr, "id")
  wayLineslist<-vector("list", length(XML))
  for(i in 1:length(XML)){
    temp<-cbind(c(nodedata[waynodelist[[i]],]$lon), c(nodedata[waynodelist[[i]],]$lat))
    wayLineslist[[i]]<-Lines(Line(temp),wayIDs[i])
  }
  
  ways.meta<-getXMLMeta(XML)
  ret<-SpatialLinesDataFrame(SpatialLines(wayLineslist, proj4string=crs),
                                    data=as.data.frame(ways.meta), match.ID=TRUE)
  ret
}

