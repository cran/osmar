getXMLMeta <-
function(XML){
    ## gibt MetaDaten der XMLNode zurück
  eltype<-attr(XML, "element")
  if(length(XML)==0)
    return(paste(c("no elements of type", eltype, "recorded"), collapse=" "))
    
  ret<-lapply(XML, xmlAttrs)
  noEl<-ifelse(eltype=="node", 9,7 )
  if(any(sapply(ret, length)!=noEl)){
    ret<-as.data.frame(do.call("smartbind", ret))
  } else{
    ret<-as.data.frame(do.call("rbind", ret))  
  }
  ret$id <- as.character(ret$id)
  ret$timestamp<-strptime(ret$timestamp, format="%Y-%m-%dT%H:%M:%S")
  row.names(ret)<-ret$id
  if(eltype=="node"){
    ret$lat<- as.numeric(as.character(ret$lat))    
    ret$lon<- as.numeric(as.character(ret$lon))
  }
  class(ret)<-c(paste(c(eltype,"Meta"), collapse=""), class(ret))      
  ret
}