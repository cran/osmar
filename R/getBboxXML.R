getBboxXML <-
function(coords, URL=FALSE){

  if( ((coords[1]-coords[3])*(coords[2]-coords[4])) >=0.25)
    return(cat("BoundingBox is bigger than 0.25-Square-Degrees", sep="\n"))
  request<-paste("http://api.openstreetmap.org/api/0.6/map?bbox=",coords[1],",",coords[2],",",coords[3],",",coords[4], sep="")
  response<-getURL(request, .encoding="UTF-8")  
                              
  xml<- xmlParse(response)   
                              
  core<- xmlRoot(xml)       
  if(URL==TRUE) cat(paste(c("Request: \"", request, "\""), collapse=""), sep="\n")
  if(xmlName(core)=="osm")
    return(core)
  if(xmlName(core)=="html"){
    cat(xmlValue(getNodeSet(core, path="head")[[1]]),
        xmlValue(getNodeSet(core, path="body")[[1]]), sep="\n")
  }
}