summary.Node<- function(object,...){
  x<-object
  ret<-list(number=numeric())
  ret$number<-c(total=numeric(1), withData=numeric(1))
  
  if(is.character(x[[2]])==FALSE){ 
    temp<-summary(x[[2]]) 
    ret$number[1]   <-temp$npoints
    ret$Bbox   <-temp$bbox
    temp<-summary(x[[2]]$timestamp) 
    ret$times    <-c(oldest=min(temp), newest=max(temp))
    temp<-summary(x[[2]]$user)
    ret$topUser <-temp[order(temp, decreasing=TRUE)]
  } else {ret$number[1]<-0}
  if(is.character(x[[1]])==FALSE){
    ret$number[2]       <- length(x[[1]]$id[!duplicated(x[[1]]$id)])
    ret$amenity     <- extractValues("amenity",x[[1]])
    ret$building    <- extractValues("building",x[[1]])
    ret$railway     <- extractValues("railway", x[[1]])    
    ret$place       <- extractValues("place", x[[1]])
    ret$allValue    <- extractValuesAll(x[[1]], c("railway", "amenity", "place", "building"))
  } else {ret$number[2] <-0}
  class(ret)<-"summary.Node"
  ret
}

summary.Way<- function(object,...){
  x<-object
  ret<-list(number=numeric())
  ret$number<-c(total=numeric(1), withData=numeric(1))
  
  if(is.character(x[[2]])==FALSE){
    ret$number[1]      <-nrow(x[[2]])
    if(class(x[[2]])[1]=="SpatialLinesDataFrame")
      ret$Bbox   <-summary(x[[2]])$bbox
    temp<-summary(x[[2]]$timestamp) 
    ret$times    <-c(oldest=min(temp), newest=max(temp))
    temp<-summary(x[[2]]$user)
    ret$topUser <-temp[order(temp, decreasing=TRUE)]

  } else {ret$number[1]<-0}
  if(is.character(x[[1]])==FALSE){
    ret$number[2] <- length(unique(x[[1]]$id))
    ret$streets     <-list(highway=extractValues("highway", x[[1]]),
                            railway  =extractValues("railway", x[[1]]),
                            tunnel   =extractValues("tunnel", x[[1]]),
                            bridge   =extractValues("bridge", x[[1]]))
    ret$nature   <-list(natural=extractValues("natural", x[[1]]),
                            landuse=extractValues("landuse", x[[1]]),
                            waterway=extractValues("waterway", x[[1]]))
    ret$building <-extractValues("building",x[[1]])
    ret$amenity  <-extractValues("amenity", x[[1]])

    ret$allValue <-extractValuesAll(x[[1]],
                                        c("highway","railway","tunnel","bridge",
                                          "natural","landuse","waterway","building",
                                          "amenity"))
  } else {ret$number[2] <-0}
  class(ret)<-"summary.Way"
  ret
}

summary.Relation<- function(object,...){
  x<-object
  ret<-list(number=numeric())
  ret$number<-c(total=numeric(1), withData=numeric(1))
  
  if(is.character(x[[1]])==FALSE){
    ret$number[1]   <-nrow(x[[1]])
    temp<-summary(x[[1]]$timestamp) 
    ret$times    <-c(oldest=min(temp), newest=max(temp))
    temp<-summary(x[[1]]$user)
    ret$topUser <-temp[order(temp, decreasing=TRUE)]
  }else{ret$number[1]<-0}
  
  if(is.character(x[[2]])==FALSE){
    ret$number[2]     <- length(unique(x[[2]]$id))
    ret$type        <- extractValues("type", x[[2]])
    if("route" %in% names(ret$type))
      ret$route     <- extractValues("route", x[[2]])
    ret$boundary    <- extractValues("boundary", x[[2]])

    ret$allValue    <- extractValuesAll(x[[2]], c("type", "route", "boundary")) 
  }else{ret$number[2]<-0}
  class(ret)<-"summary.Relation"
  ret
}

summary.OSM <- function(object,...){
  x<-object
  ret<-list(nodeSummary=list(), waySummary=list(), relationSummary=list())

  ret$nodeSummary<- summary(x$Node)
  ret$waySummary<- summary(x$Way)
  ret$relationSummary<-summary(x$Relation)
  nd<-ret$nodeSummary
  wy<-ret$waySummary
  rel<-ret$relationSummary
  
  ret$number<-rbind(nd$number,wy$number,rel$number)
  row.names(ret$number)<-c("Nodes","Ways","Relations")
  if(!any(ret$number[,1]==0)){
    ret$times<-t(data.frame(nd$times,wy$times,rel$times))
    row.names(ret$times)<-c("Nodes","Ways","Relations")
  }
  if(all(ret$number[c(1,2),1]!=c(0,0))){
    ret$Bbox<-matrix(0,2,2, dimnames=list(c("x","y"),c("min","max")))
    ret$Bbox[1,1]<-min(nd$Bbox[1,1], wy$Bbox[1,1])
    ret$Bbox[2,1]<-min(nd$Bbox[2,1], wy$Bbox[2,1])
    ret$Bbox[1,2]<-max(nd$Bbox[1,2], wy$Bbox[1,2])
    ret$Bbox[2,2]<-min(nd$Bbox[2,2], wy$Bbox[2,2])
  } else{ret$Bbox<-"no BoundingBox"} 
  class(ret)<-"summary.OSM"
  ret
}