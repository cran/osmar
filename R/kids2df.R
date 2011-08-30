kids2df <-
function(XML, dfType){
    ### setzt values der xml (abzüglich der refs) in long.data.frame um
  size<-xmlSize(XML)
  ret<-data.frame(id=character(size))
  ret$id<-rep(xmlGetAttr(XML, "id"), each=size)
        
  if(dfType=="data")
    colnames<-c("k", "v")
  if(dfType=="member")
    names(xmlAttrs(xmlChildren(XML)[[1]]))->colnames
    
  ret[colnames]<-character(size)
  for(i in 1:length(colnames))
    ret[colnames[i]]<-xmlSApply(XML, xmlGetAttr, colnames[i])
  ret
}