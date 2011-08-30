getElementXML <-
function(ID, element="", full=FALSE){
  stopifnot(element %in% c("node", "way", "relation"))
  if(length(ID)==1){
    request<- paste("http://api.openstreetmap.org/api/0.6", element, ID, sep="/")
    if(element!="node" & full==TRUE)
      request<-paste(request,"full", sep="/")
  } else{
    elements<-paste(element,"s?", element,"s=",sep="")
    request<- paste("http://api.openstreetmap.org/api/0.6/", elements, paste(ID, collapse=",") , sep="")
  }
  cat(paste(c("Request: \"", request, "\""), collapse=""), sep="\n")
  response<-getURL(request)  
                              
  if(nchar(response) %in% c(0,1)) return(warning("empty response"))
  xml<- xmlParse(response)   
                              
  core<- xmlRoot(xml)       
  core
}