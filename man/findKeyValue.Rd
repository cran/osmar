\name{findKeyValue}
\alias{findKeyValue}

\title{subset of a specific key or/and value}
\description{
  Given a \code{\link{OSM-class}} this function searches for specific elements with given
  key or value and builds a new OSM Object with these new elements.
}
\usage{findKeyValue(x, key = "", value = "", found = TRUE)}

\arguments{
  \item{x}{Object of \code{\link{OSM-class}}}
  \item{key}{character matching the key to be extracted. It can be given alone or 
              in combination with \command{value}}
  \item{value}{character matching the value to be extracted. It can be given alone or
              in combination with\command{key}}
  \item{found}{logical; if \command{TRUE} displays the number of found IDs}
}
\details{
  Basically this function searches through the Node, Way and Relation objects in \command{x}
  and finds the IDs with the given \command{key} and/or \command{value}.
  Then the IDs are given to \code{\link{findID.OSM}} and an \code{\link{OSM-class}} is build.
  Very similar to \code{\link{findTime}}.  
}
\value{object of type \code{\link{OSM-class}}}
\author{Thomas Schlesinger}
\seealso{
  \code{\link{findID.OSM}}
  \code{\link{findTime}}
}
\examples{
  \dontrun{
  coords<-bbox2coords(c(11.579341,48.15102),c(500,500)) 
  xml<- getBboxXML(coords, URL=TRUE)
  osm<-getOSMObject(xml, reduced=TRUE)
  
  lights<-findKeyValue(osm, value="traffic_signals")
  class(lights)
  plot(osm)
  
  roads<-findKeyValue(osm, key="highway")
  plot(roads, col=3, lwd=2, add=TRUE)
  
  bike<-findKeyValue(osm, key="highway", value="cycleway")
  plot(bike, col=4, lwd=3, add=TRUE)
  
  plot(lights, col=2, add=TRUE)
  }
}