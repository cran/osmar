requiredValues <-
function(XML, eltype){
  stopifnot("XMLNodeSet" %in% class(XML)| "list" %in% class(XML))
  if(eltype=="node"){
    ret<- XML[which(sapply(XML,xmlSize)!=0)]
    return(ret)
  }
  if(eltype=="way"){
    XMLclone<- lapply(XML, xmlClone)
    XMLclone<-removeKids(XMLclone, "nd")
    ret<- XMLclone[which(sapply(XMLclone,xmlSize)!=0)]
    return(ret)    
  }
  if(eltype=="relation"){
    XMLclone<-lapply(XML, xmlClone)
    XMLclone<-removeKids(XMLclone, "member")
    ret<- XMLclone[which(sapply(XMLclone,xmlSize)!=0)]         
    return(ret)
  }
  if(eltype=="member"){
    XMLclone<-lapply(XML, xmlClone)
    ret<-removeKids(XMLclone, "tag")
    return(ret)
  }
}

