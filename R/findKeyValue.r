findKeyValue <- function(x, key="", value="", found=TRUE){
  stopifnot(class(x)[[1]]=="OSM")
  stopifnot(sum(nchar(c(key,value)))!=0)
  osmDat<-rbind(x$Node[[1]],x$Way[[1]],x$Relation[[2]])
  if(nchar(key)!=0 & nchar(value)==0){
    tempIDs<-subset(osmDat, k==key)$id
    if(found==TRUE)
      cat(paste(c("found IDs", length(tempIDs)), collapse=": "), sep="\n")
    return(findID(x, tempIDs))
  }
  if(nchar(key)==0 & nchar(value)!=0){
    tempIDs<-subset(osmDat, v==value)$id
    if(found==TRUE)
      cat(paste(c("found IDs", length(tempIDs)), collapse=": "), sep="\n")
    return(findID(x, tempIDs))
  }
  if(nchar(key)!=0 & nchar(value)!=0){
    tempIDs<-subset(osmDat, k==key & v==value)$id
    if(found==TRUE)
      cat(paste(c("found IDs", length(tempIDs)), collapse=": "), sep="\n")
    return(findID(x, tempIDs))
  }   
}