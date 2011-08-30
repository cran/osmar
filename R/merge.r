merge.wayDataFrame<-merge.relationDataFrame<-merge.nodeDataFrame <-
merge.wayMember<- merge.relationMember <-
merge.relationMeta <- merge.wayMeta <-function(x, ...){
  z<-list(x,...)
  ret<-do.call("rbind", z)
  ret<-unique(ret)
  return(ret)
}

merge.SpatialPointsDataFrame<- function(x,y,...){
  z<-list(x,y,...)
  nodesmeta<-do.call("rbind", lapply(z, function(k) k@data))
  nodesmeta<-nodesmeta[!duplicated(nodesmeta$id),]

  factorCols<-names(nodesmeta)[sapply(nodesmeta, is.factor)]
  nodesmeta[,factorCols] <- droplevels(nodesmeta[,factorCols])
  nodescoords<-cbind(nodesmeta$lon, nodesmeta$lat)
  nodescoords<-data.frame(lon=nodesmeta$lon, lat=nodesmeta$lat, row.names=nodesmeta$id)
  nodeSP<- SpatialPointsDataFrame(coords=nodescoords, proj4string=z[[1]]@proj4string,
                                      data=as.data.frame(nodesmeta), match.ID=TRUE)
  return(nodeSP)
}

merge.SpatialLinesDataFrame <- function(x,y,...){
  z<-list(x,y,...)
  merglines<-unlist(lapply(z, function(k) k@lines))
  merglines<-merglines[!duplicated(sapply(merglines, function(k) k@ID))]
  mergdata<-do.call("rbind",lapply(z, function(k) k@data))
  ret<-SpatialLinesDataFrame(SpatialLines(merglines, proj4string=z[[1]]@proj4string),
                                      data=mergdata, match.ID=TRUE)
  return(ret)
}

merge.Node <- function(x,y,...){
  z<-list(x,y,...)
  le<-length(z)
  
  firstNotChar<-which(sapply(z, function(k) class(k[[1]])[1])!="character")
  if(length(firstNotChar)!=0){
    nodeData<-do.call("merge", lapply(z[firstNotChar], function(k) k[[1]]))
  } else {nodeData<-"no data of node elements recorded"}
  
  scndNotChar<-which(sapply(z, function(k) class(k[[2]])[1])!="character")
  if(length(scndNotChar)!=0){
    nodesSP<-do.call("merge", lapply(z[scndNotChar], function(k) k[[2]]))
  } else {nodesSP<-"no elements of type node recorded"}  

  ret<-Node(nodeData, nodesSP)
  ret
}

merge.Way <- function(x,y,...){
  z<-list(x,y,...)
  le<-length(z)
  
  firstNotChar<-which(sapply(z, function(k) class(k[[1]])[1])!="character")
  if(length(firstNotChar)!=0){
    wayData<-do.call("merge", lapply(z[firstNotChar], function(k) k[[1]]))
  } else{wayData<-"no data of way elements recorded"}
  
  waySP<-do.call("merge", lapply(z, function(k) k[[2]]))
  
  thirdNotChar<-which(sapply(z, function(k) class(k[[3]])[1])!="character")
  if(length(thirdNotChar)!=0){
    wayMem<-do.call("merge", lapply(z[thirdNotChar], function(k) k[[3]]))
  } else{wayMem<-"no elements of type way recorded"}
  
  ret<-Way(wayData, waySP, wayMem)
  ret
}

merge.Relation<- function(x,y,...){
  z<-list(x,y,...)
  le<-length(z)
  firstNotChar<-which(sapply(z, function(k) class(k[[1]])[1])!="character")
  if(length(firstNotChar)!=0){
    relationMeta<-do.call("merge", lapply(z[firstNotChar], function(k) k[[1]]))
  } else{relationMeta<-"no elements of type relation recorded"}
  
  scndNotChar<-which(sapply(z, function(k) class(k[[2]])[1])!="character")
  if(length(scndNotChar)!=0){
    relationData<-do.call("merge", lapply(z[scndNotChar], function(k) k[[2]]))
  } else{relationData<-"no data of relation elements recorded"}
  
  thirdNotChar<-which(sapply(z, function(k) class(k[[3]])[1])!="character")
  if(length(thirdNotChar)!=0){
    relationMem<-do.call("merge", lapply(z[thirdNotChar], function(k) k[[3]]))
  } else{relationMem<-"no elements of type relation recorded"}
  
  ret<-Relation(relationMeta, relationData, relationMem)
  ret
}

merge.OSM<- function(x,y,...){
  z<-list(x,y,...)
  node<-do.call("merge", lapply(z, function(k) k$Node))
  way<-do.call("merge", lapply(z, function(k) k$Way))
  relation<-do.call("merge", lapply(z, function(k) k$Relation))
  return(OSM(node,way,relation))
}