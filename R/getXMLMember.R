getXMLMember <-
function(XML){
  eltype<-attr(XML,"element")
  if(eltype=="node")
    return("node-elements don't have members")
  if(length(XML)==0)
    return(paste(c("no elements of type", eltype, "recorded"), collapse=" "))

  values<-requiredValues(XML, "member")
  ret<-do.call("rbind", lapply(values, kids2df, "member"))
  
  class(ret)<-c(paste(c(eltype,"Member"), collapse=""), class(ret))      
  ret
}