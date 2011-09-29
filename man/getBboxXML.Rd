\name{getBboxXML}
\alias{getBboxXML}
\title{
  getting OSM-Box-XML
}
\description{
  This function uses the OpenStreetMap API v0.6 to get Data from a rectangle area
  defined by a bounding box. The area is defined by latitude & longitude values.
}
\usage{
  getBboxXML(coords, URL = FALSE)
}
\arguments{
  \item{coords}{
    numeric vector of length four which contains edges of the bounding box in 
    decimal latitude and longitude values. The form has to be 
    \code{c(west longitude, south latitude, east longitude, north latitude)}. 
  }
  \item{URL}{
    logical. TRUE prints the http request in RConsole
  }
}
\details{
  The HTTP-Request has this form "http://api.openstreetmap.org/api/0.6/map?bbox=left,bottom,right,top"
  and can be typed in your browser window, too. In R it is handled by the \code{\link{getURL}}-function.
  The result will have a \sQuote{UTF-8} character encoding.
  
  The returned XML-File contains: (1)nodes in the given bbox, (2)ways that reference BboxNodes,
   (3) nodes that are referenced in the ways but NOT in the bbox and (4) all relations that reference one 
   of the above elements (but not vice-versa).
  
  Two error-codes can occur:
  (1) HTTP status code 400 (Bad Request): When any of the node/way/relation limits are crossed, such as
    requesting more than 50.000 Nodes or an area with more than 0.25 square-degrees (maxlon-minlon)x(maxlat-minlat)
  and (2) HTTP status code 509 (Bandwidth Limit Exceeded): When you have downloaded too much data.
  
  Furthermore the returned XMLDocument won't have the first Tag with XML-Version and encoding. 
  
  Licence of downloaded data: Copyright OpenStreetMap & Contrib, CC-BY-SA
}
\value{
  returns an object of class \sQuote{XMLInternalElementNode} 
}
\references{
  \url{http://wiki.openstreetmap.org/wiki/API_v0.6#Retrieving_map_data_by_bounding_box:_GET_.2Fapi.2F0.6.2Fmap}
}
\author{
Thomas Schlesinger
}
\note{
  speed of function depends on the business of OpenStreeMap-Server.
  you need a working internet connection while using this function.
}

\seealso{
  \code{\link{getURL}}
  \code{\link{bbox2coords}}
  \code{\link{getOSMObject}}
}
\examples{
  \dontrun{
  coords<-c(11.580341,48.15102,11.582852,48.1530)
  XMLRaw<-getBboxXML(coords, URL=TRUE)
  XMLRaw
  coords2<-bbox2coords(c(11.579341,48.15102),c(50,50))
  XML<- getBboxXML(coords2, URL=TRUE)
  XML
  
  ##error messages
  coords<-bbox2coords(c(11.579341,48.15102),c(5000,5000))
  getBboxXML(coords)
  #Bad Request
  #You requested too many nodes (limit is 50000). Either request a smaller area, or use planet.osm
  getBboxXML(c(10,40,12,50))
  #Bad Request
  #The maximum bbox size is 0.25, and your request was too large. Either request a smaller area, or use planet.osm  
  }
}