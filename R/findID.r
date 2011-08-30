findID<- function(x,...){
  UseMethod("findID")
}

findID.Node <- function(x, ID, ...){
  stopifnot(is.character(ID))
  stopifnot(class(x)[1] =="Node")
  ID<-unique(ID)
  ID <- ID[ID %in% x[[2]]$id]
  if(length(ID)==0) return(Node("no data of node elements recorded", "no elements of type node recorded"))
  
  nodesmeta<-x[[2]]@data[ID,]
  factorCols<-names(nodesmeta)[sapply(nodesmeta, is.factor)]
  nodesmeta[,factorCols] <- droplevels(nodesmeta[,factorCols])  
  nodescoords<-cbind(nodesmeta$lon, nodesmeta$lat)
  nodescoords<-data.frame(lon=nodesmeta$lon, lat=nodesmeta$lat, row.names=nodesmeta$id)
  nodesSP<- SpatialPointsDataFrame(coords=nodescoords, proj4string=x[[2]]@proj4string,
                                      data=as.data.frame(nodesmeta), match.ID=TRUE)
  if(is.character(x[[1]])){
    nodeData<-x[[1]]
  } else{
    nodeData<-x[[1]][x[[1]]$id %in% ID,]
    class(nodeData)<-class(x[[1]])
    if(nrow(nodeData)==0) nodeData<-"no data of node elements recorded" 
  }
  ret<-Node(nodeData, nodesSP)
  ret
}

findID.Way <- function(x, ID,...){
  stopifnot(is.character(ID))
  ID<-unique(ID) 
     
  if(class(x[[2]])[1]=="wayMeta"){
    ID<- ID[ID %in% x[[2]]$id]
    if(length(ID)==0) return(Way("no data of way elements recorded","no elements of type way recorded",
                                  "no data of way elements recorded"))
    wayMeta <- x[[2]][ID,]
    factorCols<-names(wayMeta)[sapply(wayMeta, is.factor)]
    wayMeta[,factorCols] <- droplevels(wayMeta[,factorCols])  
    wayData <- x[[1]][x[[1]]$id %in% ID,]
    wayMember<-x[[3]][x[[3]]$id %in% ID,]
    class(wayData)<-class(x[[1]])
    class(wayMeta)<-class(x[[2]])
    class(wayMember)<-class(x[[3]])
    return(Way(wayData, wayMeta, wayMember))   
  }
  if(class(x[[2]])[1]== "SpatialLinesDataFrame"){
    ID<- ID[ID %in% x[[2]]$id]
    if(length(ID)==0) return(Way("no data of way elements recorded","no elements of type way recorded",
                                  "no data of way elements recorded"))
    wayData<- x[[1]][x[[1]]$id %in% ID,]
    class(wayData)<-class(x[[1]])
    LinesID <- sapply(x[[2]]@lines, function(k) slot(k, "ID"))
    LinesID <- which(LinesID %in% ID) 
    wayLines<- x[[2]]@lines[LinesID]   
    wayMeta<- x[[2]]@data[ID,]
    factorCols<-names(wayMeta)[sapply(wayMeta, is.factor)]
    wayMeta[,factorCols] <- droplevels(wayMeta[,factorCols])  
    waySpatial <- SpatialLinesDataFrame(SpatialLines(wayLines, proj4string=x[[2]]@proj4string),
                                    data=as.data.frame(wayMeta), match.ID=TRUE)
    wayMember<-x[[3]][x[[3]]$id %in% ID,]
    class(wayMember)<-class(x[[3]])
    return(Way(wayData, waySpatial, wayMember))   
  }
}

findID.Relation <- function(x, ID,...){
  stopifnot(is.character(ID))
  ID<-unique(ID) 
  if(is.character(x[[1]]))
    return(Relation("no elements of type relation recorded",
                    "no data of relation elements recorded",
                    "no elements of type relation recorded"))
  ID<- ID[ID %in% x[[1]]$id]
  if(length(ID)==0) 
    return(Relation("no elements of type relation recorded",
                    "no data of relation elements recorded",
                    "no elements of type relation recorded"))
  relationMeta<-x[[1]][ID,]
  factorCols<-names(relationMeta)[sapply(relationMeta, is.factor)]
  relationMeta[,factorCols] <- droplevels(relationMeta[,factorCols])  
  class(relationMeta) <- class(x[[1]])
  relationData<- x[[2]][x[[2]]$id %in% ID,]
  class(relationData) <- class(x[[2]])
  relationMember<-x[[3]][x[[3]]$id %in% ID,]
  class(relationMember)<-class(x[[3]])
  
  return(Relation(relationMeta, relationData, relationMember))
}

findID.OSM<- function(x, ID, full=FALSE, what="", check=TRUE, ...){
  if(full==FALSE){
    node<-findID(x$Node, ID)
    way <-findID(x$Way, ID)
    relation<-findID(x$Relation, ID)
    return(OSM(node, way, relation))
  }
  if(full==TRUE){
    stopifnot(length(ID)==1)
    if(!(what %in% c("relation", "way"))) stop("operator <what> has to be given")
    if(what=="relation"){
      relation<-findID(x$Relation, ID)
      nodeIDs<-subset(relation[[3]], type=="node")$ref
      wayIDs<-subset(relation[[3]], type=="way")$ref
      relationIDs<-c(subset(relation[[3]], type=="relation")$ref, ID)      
      node<-findID(x$Node, nodeIDs)
      way<-findID(x$Way, wayIDs)
      relation<-findID(x$Relation, relationIDs)
      if(check==TRUE) compCheck(node, way, relation, nodeIDs, wayIDs, relationIDs, what=what)
      return(OSM(node, way, relation))
    }
    if(what=="way"){
      way<-findID(x$Way, ID)
      nodeIDs<-way[[3]]$ref
      node<-findID(x$Node, nodeIDs)
      relation<-findID(x$Relation, "")
      if(check==TRUE) compCheck(node, way, relation, nodeIDs, ID, what=what)
      return(OSM(node, way, relation)) 
    }    
  }
}