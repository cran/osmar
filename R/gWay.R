gWay <-
function(x){
  stopifnot(class(x) %in% c("XMLInternalElementNode","XMLInternalNode","XMLAbstractNode" ))
  ret<-getNodeSet(x, path="//way")
  attr(ret, "element") <- "way"
  ret
}