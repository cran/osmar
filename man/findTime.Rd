\name{findTime}
\alias{findTime}

\title{subset of a specific time period}
\description{
  Given a \code{\link{OSM-class}} this function searches for specific elements
  in a given time period and builds a new OSM Object with these new elements.
}
\usage{findTime(x, time = "", what = "", time2 = "")}

\arguments{
  \item{x}{Object of \code{\link{OSM-class}}}
  \item{time, time2}{needs an object of type \code{\link{POSIXlt}}}
  \item{what}{character string: 
              If \code{"older"} or \code{"newer"} searches every element
              with latest modification date older or newer than \command{time} and
              if \code{"between"} searches every element with latest modification
              date between \command{time} and \command{time2}}
}
\details{
  Basically this function searches through the Node, Way and Relation objects in \command{x}
  and finds the IDs with latest date of change in the given time period.
  Then the IDs are given to \code{\link{findID.OSM}} and an \code{\link{OSM-class}} is build.
  Very similar to \code{\link{findKeyValue}}.  
}
\value{object of type \code{\link{OSM-class}}}

\author{Thomas Schlesinger}

\seealso{  
  \code{\link{findID.OSM}}
  \code{\link{findKeyValue}}
}
\examples{
  \dontrun{
  coords<-bbox2coords(c(11.579341,48.15102),c(500,500)) 
  xml<- getBboxXML(coords, URL=TRUE)
  osm<-getOSMObject(xml, reduced=TRUE)
  
  plot(osm)
  thisyear<-strptime("2011.01.01", format="\%Y.\%m.\%d")
  class(thisyear)
  oneone<-findTime(osm, time=thisyear, "older")
  summary(oneone)$time
  plot(oneone$Node, col=2, add=TRUE)
  
  onetwo<-findTime(osm, time=thisyear, "newer")
  plot(onetwo$Node, col=3, add=TRUE) 
  
  twonine1<-strptime("2009.01.01", format="\%Y.\%m.\%d")
  twonine2<-strptime("2009.12.31", format="\%Y.\%m.\%d")
  nine<-findTime(osm, time=twonine1, time2=twonine2, what="between")
  summary(nine)$time
  plot(osm)
  plot(nine, add=TRUE, col=2, lwd=2)
  plot(nine$Node, add=TRUE, col=2)
  ## now we marked every node and way object with last alteration date in 2009
  }
}