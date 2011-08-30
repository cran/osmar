Node<- function(...){
  x<-list(...)
  stopifnot(length(x)==2)
  stopifnot(any(class(x[[1]]) %in% c("nodeDataFrame", "character")))
  stopifnot(any(class(x[[2]]) %in% c("SpatialPointsDataFrame", "character")))
  
  class(x)<-c("Node", class(x))
  x
}

Way<- function(...){
  x<-list(...)
  stopifnot(length(x)==3)
  stopifnot(any(class(x[[1]]) %in% c("wayDataFrame", "character")))
  stopifnot(any(class(x[[2]]) %in% c("SpatialLinesDataFrame","wayMeta", "character")))
  stopifnot(any(class(x[[3]]) %in% c("wayMember", "character")))
  class(x)<-c("Way", class(x))
  x
}

Relation<- function(...){
  x<-list(...)
  stopifnot(length(x)==3)
  stopifnot(any(class(x[[1]]) %in% c("relationMeta", "character")))
  stopifnot(any(class(x[[2]]) %in% c("relationDataFrame", "character")))
  stopifnot(any(class(x[[3]]) %in% c("relationMember", "character")))
  class(x) <- c("Relation", class(x))
  x
}

OSM <-
function(...){
  x <- list(...)
  stopifnot(length(x)==3)
  stopifnot("Node" %in% class(x[[1]]))
  stopifnot("Way" %in% class(x[[2]]))
  stopifnot("Relation" %in% class(x[[3]]))
  names(x)<-c("Node","Way","Relation")
  class(x)<-c("OSM", class(x))
  x
}