bounds <-
function(x){
  stopifnot(class(x) %in% c("XMLInternalElementNode","XMLInternalNode","XMLAbstractNode" ))
  temp<-getNodeSet(x, path="//bounds")
  if(length(temp)==0) stop("reduce==TRUE not possible because no Bbox was given")
  ret<-as.numeric(xmlAttrs(temp[[1]]))
  names(ret)<-names(xmlAttrs(temp[[1]]))
  ret
}