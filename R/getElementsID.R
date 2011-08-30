getElementsID <-
function(XML){
  stopifnot("XMLNodeSet" %in% class(XML)| "list" %in% class(XML))
  ret<-sapply(XML, xmlGetAttr, "id")
  ret
}