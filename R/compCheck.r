compCheck<- function(... , what){
#node, way, relation, nodeIDs, wayIDs, relationIDs
  x<-list(...)
  noNodes     <- ifelse(is.character(x[[1]][[2]]), "all", sum(!(x[[4]] %in% x[[1]][[2]]@data$id)))
  noWays      <- ifelse(is.character(x[[2]][[2]]), "all", sum(!(x[[5]] %in% x[[2]][[2]]@data$id)))
  noRelations <- ifelse(is.character(x[[3]][[1]]), "all", sum(!(x[[6]] %in% x[[3]][[1]]$id)))
  if(what=="relation")
    return(cat(paste(c(noNodes," (of ", length(unique(x[[4]])),") nodes are missing"), collapse=""),
            paste(c(noWays," (of ", length(unique(x[[5]])),") ways are missing"), collapse=""),
            paste(c(noRelations," (of ", length(unique(x[[6]])),") relations are missing"), collapse=""), sep="\n")
            )
  if(what=="way")
    return(cat(paste(c(noNodes," (of ", length(unique(x[[4]])),") nodes are missing"), collapse=""),
            paste(c(noWays," (of ", length(unique(x[[5]])),") ways are missing"), collapse=""), sep="\n")
            )
}