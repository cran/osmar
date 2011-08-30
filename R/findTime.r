findTime<- function(x, time="", what="", time2=""){
  stopifnot(class(x)[1]=="OSM")
  stopifnot(what %in% c("older", "newer", "between"))
  stopifnot(class(time)[1] %in% c("POSIXlt", "POSIXct"))
  
  if(what=="older"){
    nodeIDs     <- subset(x$Node[[2]]@data, timestamp < time)$id
    wayIDs      <- subset(x$Way[[2]]@data, timestamp < time)$id
    relationIDs <- subset(x$Relation[[1]], timestamp < time)$id
    return(findID(x, c(nodeIDs, wayIDs, relationIDs)))
  }
  if(what=="newer"){
    nodeIDs     <- subset(x$Node[[2]]@data, timestamp > time)$id
    wayIDs      <- subset(x$Way[[2]]@data, timestamp > time)$id
    relationIDs <- subset(x$Relation[[1]], timestamp > time)$id
    return(findID(x, c(nodeIDs, wayIDs, relationIDs)))
  }
  if(what=="between"){
    stopifnot(class(time2)[1] %in% c("POSIXlt", "POSIXct"))
    nodeIDs     <- subset(x$Node[[2]]@data, timestamp>=time & timestamp<time2)$id
    wayIDs      <- subset(x$Way[[2]]@data, timestamp>=time & timestamp<time2)$id
    relationIDs <- subset(x$Relation[[1]], timestamp>=time & timestamp<time2)$id
    return(findID(x, c(nodeIDs, wayIDs, relationIDs)))
  }
}