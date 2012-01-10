

### Attributes: ######################################################

extract_attr <- function(x) {
  UseMethod("extract_attr")
}

extract_attr.osm_parsed <- function(parsed) {
  nodeattr <- extract_attr(parsed[[1]])
  wayattr <- extract_attr(parsed[[2]])
  relationattr <- extract_attr(parsed[[3]])

  list(nodeattr = nodeattr, wayattr = wayattr, relationattr = relationattr)
}

extract_attr.node_parsed<- function(elparsed){
  ret <- lapply(elparsed$elements, xmlAttrs)

  if(length(ret) == 0) {
    return(data.frame(id = numeric(),
                      visible = character(),
                      timestamp = character(),  # How to create a POSIXlt(0)?
                      version = numeric(),
                      changeset = numeric(),
                      user = factor(),
                      uid = factor(),
                      lat = numeric(),
                      lon = numeric()))
  }

  if(any(sapply(ret, length)!=9)){
    ret <- as.data.frame(do.call("smartbind", ret))
  } else{
    ret <- as.data.frame(do.call("rbind", ret))
  }

  ret$timestamp <- strptime(ret$timestamp, format="%Y-%m-%dT%H:%M:%S")
  ret$lat<- as.numeric(as.character(ret$lat))
  ret$lon<- as.numeric(as.character(ret$lon))
  ret$id<- as.numeric(as.character(ret$id))
  ret$version<- as.numeric(as.character(ret$version))
  ret$uid<- as.factor(as.character(ret$uid))
  ret$user<- as.factor(as.character(ret$user))
  ret$changeset<- as.numeric(as.character(ret$changeset))

  ret
}

extract_attr.way_parsed <- function(elparsed){
  ret <- lapply(elparsed$elements, xmlAttrs)

  if(length(ret)==0) {
    return(data.frame(id = numeric(),
                      visible = character(),
                      timestamp = character(),  # How to create a POSIXlt(0)?
                      version = numeric(),
                      changeset = numeric(),
                      user = factor(),
                      uid = factor()))
  }

  if(any(sapply(ret, length)!=7)){
    ret<-as.data.frame(do.call("smartbind", ret))
  } else{
    ret<-as.data.frame(do.call("rbind", ret))
  }

  ret$timestamp <- strptime(ret$timestamp, format="%Y-%m-%dT%H:%M:%S")
  ret$id<- as.numeric(as.character(ret$id))
  ret$version<- as.numeric(as.character(ret$version))
  ret$uid<- as.factor(as.character(ret$uid))
  ret$user<- as.factor(as.character(ret$user))
  ret$changeset<- as.numeric(as.character(ret$changeset))

  ret
}

extract_attr.relation_parsed <- extract_attr.way_parsed




### Data: ############################################################

extract_data <- function(x){
  UseMethod("extract_data")
}

extract_data.osm_parsed <- function(parsed){
  nodedata <- extract_data(parsed[[1]])
  waydata <- extract_data(parsed[[2]])
  relationdata <- extract_data(parsed[[3]])
  list(nodedata=nodedata, waydata=waydata, relationdata=relationdata)
}

extract_data.node_parsed <- function(nparsed){
  values<-nparsed$elements[which(sapply(nparsed$elements,xmlSize)!=0)]
  ##auswahl der nodes MIT daten
  if(length(values)==0)
    return(data.frame(id=numeric(),
                      k=factor(),
                      v=factor()))

  ret <- do.call("rbind", lapply(values, xml2long, "data"))

  ret$id <- as.numeric(as.character(ret$id))
  ret$k <- as.factor(as.character(ret$k))
  ret$v <- as.factor(as.character(ret$v))

  ret
}

extract_data.way_parsed <- function(wparsed){
  XMLclone<- lapply(wparsed$elements, xmlClone)
  XMLclone<- removeKids(XMLclone, "nd")
  XMLclone<- XMLclone[which(sapply(XMLclone, xmlSize)!=0)]
  if(length(XMLclone)==0)
    return(data.frame(id=numeric(),
                      k=factor(),
                      v=factor()))

  ret <- do.call("rbind", lapply(XMLclone, xml2long, "data"))

  ret$id <- as.numeric(as.character(ret$id))
  ret$k <- as.factor(as.character(ret$k))
  ret$v <- as.factor(as.character(ret$v))

  ret
}

extract_data.relation_parsed <- function(rparsed){
  XMLclone<- lapply(rparsed$elements, xmlClone)
  XMLclone<- removeKids(XMLclone, "member")
  XMLclone<- XMLclone[which(sapply(XMLclone,xmlSize)!=0)]
  if(length(XMLclone)==0)
    return(data.frame(id=numeric(),
                      k=factor(),
                      v=factor()))

  ret <- do.call("rbind", lapply(XMLclone, xml2long, "data"))

  ret$id <- as.numeric(as.character(ret$id))
  ret$k <- as.factor(as.character(ret$k))
  ret$v <- as.factor(as.character(ret$v))

  ret
}

xml2long <- function(x, dfType){
  size<-xmlSize(x)
  ret<-data.frame(id=character(size))
  ret$id<-rep(xmlGetAttr(x, "id"), each=size)

  if(dfType=="data")
    colnames<-c("k", "v")
  if(dfType=="member")
    colnames<-names(xmlAttrs(xmlChildren(x)[[1]]))

  ret[colnames]<-character(size)
  for(i in 1:length(colnames))
    ret[colnames[i]]<-xmlSApply(x, xmlGetAttr, colnames[i])
  ret
}

removeKids <- function(XML, kidsname){
    ## gibt den XML nodes ohne children einer bestimmten Art zurück
  lapply(XML, function(x) removeChildren(x, kids=which(names(x)==kidsname)))
}



### Relation refernces: ##############################################

extract_ref <- function(x){
  UseMethod("extract_ref")
}

extract_ref.osm_parsed <- function(parsed){
  wayref <- extract_ref(parsed[[2]])
  relationref <- extract_ref(parsed[[3]])
  list(wayref=wayref, relationref=relationref)
}

extract_ref.way_parsed <- function(wparsed){
  XMLclone<- lapply(wparsed$elements, xmlClone)
  XMLclone<- removeKids(XMLclone, "tag")
  XMLclone<- XMLclone[which(sapply(XMLclone, xmlSize)!=0)]
  if(length(XMLclone)==0)
    return(data.frame(id=numeric(), ref=numeric()))
  ret <- do.call("rbind", lapply(XMLclone, xml2long, "member"))
  ret$id <- as.numeric(as.character(ret$id))
  ret$ref <- as.numeric(as.character(ret$ref))
  ret
}

extract_ref.relation_parsed <- function(rparsed){
  XMLclone<- lapply(rparsed$elements, xmlClone)
  XMLclone<- removeKids(XMLclone, "tag")
  XMLclone<- XMLclone[which(sapply(XMLclone, xmlSize)!=0)]
  if(length(XMLclone)==0)
    return(data.frame(id=numeric(), ref=numeric(), role=factor(), type = factor()))
  ret <- do.call("rbind", lapply(XMLclone, xml2long, "member"))
  ret$id <- as.numeric(as.character(ret$id))
  ret$ref <- as.numeric(as.character(ret$ref))
  ret$role <- as.factor(as.character(ret$role))
  ret$type <- as.factor(as.character(ret$type))
  ret
}



