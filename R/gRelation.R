gRelation <-
function(x){
  stopifnot(class(x) %in% c("XMLInternalElementNode","XMLInternalNode","XMLAbstractNode" ))
  ret<-getNodeSet(x, path="//relation")
  attr(ret, "element") <- "relation"
  ret
}