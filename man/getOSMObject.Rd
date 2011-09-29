\name{getOSMObject}
\alias{getOSMObject}

\title{
  making an OSM-Object out of a XML-file
}
\description{
  This function converts a XML-file by OSM-API into an OSM-Object.
}
\usage{
  getOSMObject(XML, reduced = FALSE, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
}

\arguments{
  \item{XML}{
    has to be a XML of class \code{"XMLElementNode"} and returned by the OpenStreetMap API
  }
  \item{reduced}{
    logical; If \code{TRUE} the elements are reduced to the elements in the bbox only.
  }
  \item{crs}{
    Object of type \code{\link[rgdal]{CRS}}. Standard value is the GPS-coordinate-system (WGS84)
  }
}
\details{
  An OSM-Object is a list containing 3 elements. Furthermore every data.frame is in long-format. 
  
  The first element is a S3-class called \code{Node} with 2 elements: (1)Data and (2) a
  \linkS4class{SpatialPointsDataFrame} with geographical information about the Nodes. If no Nodes 
  where in the XML-File the 2 elements will be character strings.
  
  The second element is a S3-class called \code{Way} with 3 elements: (1)Data,
  (2)\linkS4class{SpatialLinesDataFrame} with geographical information about the Ways
  and (3) data.frame containing the NodeIDs which are referenced in a Way. If no
  Nodes are available, a SpatialLinesDataFrame can not be build. In this case MetaInformation
  about the Ways are stored in the second slot. If no Ways where in the XML-File the 3 elements will
  be character strings. 
  
  The third element is a S3-class called \code{Relation} with 3 elements. Relations don't
  have Spatial information which can be converted into an object of the sp package.
  (1) MetaInformation about the Relations itself. These usually is stored in the SpatialObjects (in case of Node and Way),
  (2) Data and
  (3) data.frame containing the members and their role in a relation.
  If no Relations where in the XML-File the 3 elements will be character strings. 
  
  There are several generic methods available for these three classes and the \code{OSM}-class
}
\value{
  Object of type \code{\link{OSM-class}}
}
\author{
  Thomas Schlesinger
}
\seealso{
  \code{\link{getBboxXML}}
  \code{\link{getElementXML}}
  \code{\link{Node-class}}
  \code{\link{Way-class}}
  \code{\link{Relation-class}}
  \code{\link{OSM-class}}
}
\examples{
  \dontrun{
  coords<-c(11.579341,48.15102,11.582852,48.1530)
  xml1<-getBboxXML(coords, URL=TRUE)
  xml2<-getBboxXML(bbox2coords(c(11.579341,48.15102),c(200,200)), URL=TRUE)
  StatRed1<-getOSMObject(xml1, reduced=TRUE)
  StatRed2<-getOSMObject(xml2, reduced=TRUE)  
  StatUnRed1<-getOSMObject(xml1, reduced=FALSE)
  StatUnRed2<-getOSMObject(xml2, reduced=FALSE)
  op<-par(mfrow=c(2,2))
    plot(StatRed1)
    plot(StatRed2)
    plot(StatUnRed1)
    plot(StatUnRed2)
  par(op)
  
  relationfull<-getElementXML("916476", "relation", TRUE)
  rfosm<-getOSMObject(relationfull, reduced=FALSE)
  Not RUN : rfosm<-getOSMObject(relationfull, reduced=TRUE)
  plot(rfosm, nodes=TRUE)
  relation<-getElementXML("916476", "relation", FALSE)
  rosm<-getOSMObject(relation, reduced=FALSE)
  
  wayfull<-getElementXML("19423091", "way", TRUE)
  wfosm<-getOSMObject(wayfull, reduced=FALSE)
  Not RUN : wfosm<-getOSMObject(wayfull, reduced=TRUE)
  plot(wfosm, nodes=TRUE)
  way<-getElementXML("19423091", "way", FALSE)
  wosm<-getOSMObject(way, reduced=FALSE)
  
  nodefull<-getElementXML("789659585", "node", TRUE)
  nfosm<-getOSMObject(nodefull, reduced=FALSE)
    ## Error-Code: nfosm<-getOSMObject(nodefull, reduced=TRUE)
  plot(nfosm)
  node<-getElementXML("789659585", "node", FALSE)
  nosm<-getOSMObject(node, reduced=FALSE)
  }
}