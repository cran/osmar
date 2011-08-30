plot.Node <- function(x, pch=20, data="no", ptcol="red", ...){
  stopifnot(class(x[[2]])[1]=="SpatialPointsDataFrame")
  stopifnot(data %in% c("no","yes","only"))
  if(data=="no"){
    pch<-pch 
    return(plot(x[[2]], pch=pch, ...))
  }
  if(data=="yes"){
    plot(x[[2]], pch=pch,...)
    nodesWithData<-unique(x[[1]]$id)
    nodes<-findID(x, nodesWithData)
    return(plot(nodes, pch=8, col=ptcol, add=TRUE))
  }
  if(data=="only"){
    nodesWithData<-unique(x[[1]]$id)
    nodes<-findID(x, nodesWithData)
    return(plot(nodes, pch=pch, col=ptcol, ...))
  }
}

plot.Way<- function(x, ...){
  stopifnot(class(x[[2]])[1]=="SpatialLinesDataFrame")
  plot(x[[2]], ...)
}

plot.Relation<- function(x,...){
  cat("No Spatial information existent", collapse="\n")
}

plot.OSM<-function(x, nodes=FALSE, ptcol="red",...){
  if(class(x$Way[[2]])=="SpatialLinesDataFrame"){
    plot(x$Way,...)
    if(nodes==TRUE)
      plot(x$Node, data="only", ptcol=ptcol, add=TRUE)
  } else {
    stopifnot(class(x$Node[[2]])[1]=="SpatialPointsDataFrame")
    plot(x$Node, data="no",...)
  }
}