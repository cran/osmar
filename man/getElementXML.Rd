\name{getElementXML}
\alias{getElementXML}
\title{
  getting OSM-Element-XML
}
\description{
  This function uses the OpenStreetMap API v0.6 to get a XML-File from an OSM-Element
  defined by \sQuote{ID}. It can return just the information about the element or the FULL-information   
}
\usage{
  getElementXML(ID, element = "", full = FALSE)
}

\arguments{
  \item{ID}{
    character string with ID of an element. vector of ID's can also be given but must be of same elementtype
  }
  \item{element}{
    type of element to be searched. options: \code{"node"}, \code{"way"} and \code{"relation"}
  }
  \item{full}{
    logical; if \code{FALSE}, just the XML of the node will be given. 
    If \code{TRUE}, the information about all referenced Members
    will be given. (1)Way: searched way + referenced nodes. 
    (2)Relation:
    searched relation + referenced Nodes + referenced ways(+their nodes) + referenced relations(not their ways|relations).
    only possible with ONE ID.
  }
}
\details{
  The HTTP-Request has this form
  \enumerate{
    \item{getting one element}{"http://api.openstreetmap.org/api/0.6"/[element]/[ID]}
    \item{getting FULL element}{"http://api.openstreetmap.org/api/0.6"/[element]/[ID]/full}
    \item{getting multiple elements}{"http://api.openstreetmap.org/api/0.6"/[element]s?[element]s=[ID1,ID2,...]}
  }
  and can be typed in your typical browser window, too. In R it is handled1 by 
  the \code{\link{getURL}}-function. The result will have a \sQuote{UTF-8} character encoding.

  Several Errors can occur:
  (1) HTTP status code 404 (Not Found): When no element with the given id could be found
  (2) HTTP status code 410 (Gone): If the element has been deleted 
  In both cases the Function will give a warning message called \code{"empty response"}.
  
  Licence of downloaded data: Copyright OpenStreetMap & Contrib, CC-BY-SA
}
\value{
  returns an object of class \sQuote{XMLInternalElementNode} 
}
\references{
  \url{http://wiki.openstreetmap.org/wiki/API_v0.6} 
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
  \code{\link{getOSMObject}}  
}
\examples{
  \dontrun{
  relationfull<-getElementXML("916476", "relation", TRUE)
  relation<-getElementXML("916476", "relation", FALSE)
  wayfull<-getElementXML("19423091", "way", TRUE)
  way<-getElementXML("19423091", "way", FALSE)
  node<-getElementXML("789659585", "node", FALSE)
  
  nodes<-getElementXML(c("454282761", "815845067"), "node")
  ways<-getElementXML(c("26825622","26824377"), "way")
  relations<-getElementXML(c("54388", "54384"), "relation")
  }
}