gNode <-
function(x){
  stopifnot(class(x) %in% c("XMLInternalElementNode","XMLInternalNode","XMLAbstractNode" ))
  ret<-getNodeSet(x, path="//node")
  attr(ret, "element") <- "node"
  ret
}