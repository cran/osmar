getXMLData <-
function(XML){
    ## macht aus liste mit XMLdateien gleicher Art ein Df im longformat
  stopifnot("XMLNodeSet" %in% class(XML)| "list" %in% class(XML))
  eltype<-attr(XML, "element")
  
  values<-requiredValues(XML, eltype)
    ##unwichtige children rausschmeißen            
  values<-values[sapply(values, xmlSize)!=0]
    ##wenn keine data vorhanden sind, dann werden die Nodes gelöscht
  if(length(values)==0) 
    return(paste("no data of", eltype, "elements recorded", collapse=""))
  ret<-do.call("rbind", lapply(values, kids2df, "data"))
  class(ret)<-c(paste(c(eltype,"DataFrame"), collapse=""), class(ret))
  ret  
}