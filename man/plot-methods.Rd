\name{plot-osmar}
\alias{plot.osmar}
\alias{plot.OSM}
\alias{plot.Node}
\alias{plot.Way}


\title{
  Plotting Object of osmar-package
}
\description{
  methods for Plotting objects of the osmar-package.   
}
\usage{
  \method{plot}{Node}(x, pch=20, data="no", ptcol="red",...)
  \method{plot}{Way}(x, ...)
  \method{plot}{OSM}(x, nodes=FALSE, ptcol="red", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Object of \code{\link{Node-class}}, \code{\link{Way-class}} or \code{\link{OSM-class}}}
  \item{pch}{point character code for Nodes. see \code{\link{points}}}
  \item{data}{three options: (1) \code{"no"}= all Nodes will be plotted, 
                (2) \code{"yes"}= Nodes with additional data will be marked with 
                    \code{pch=8} and \code{col="red"}
                (3) \code{"only"}= only Nodes with data will be plotted}
  \item{nodes}{Nodes with additional data will be marked}
  \item{ptcol}{color for Nodes with additional data}
  \item{\dots}{Arguments to be passed to plot-methods}
}
\details{
  \describe{
    \item{plot.Node}{uses generic plot method for \linkS4class{SpatialPoints}}
    \item{plot.Way}{uses generic plot method for \linkS4class{SpatialLines}}
    \item{plot.OSM}{uses plot.Way or in case of no Ways plot.Node}
  }
}
\value{
  plot window with graphics
}
\author{
  Thomas Schlesinger
}
\examples{
  \dontrun{
  xml<-getBboxXML(bbox2coords(c(11.581262,48.150626),c(500,500)), URL=TRUE)
  uni<-getOSMObject(xml, reduced=TRUE) 
  uni2<-getOSMObject(xml, reduced=FALSE) 
  
  plot(uni, nodes=TRUE)
  plot(uni, nodes=FALSE)
  plot(uni2, nodes=TRUE)
  plot(uni2,nodes=FALSE)  
  op<-par(mfrow=c(2,2))
    plot(uni$Node)            
    plot(uni$Node, data="yes")
    plot(uni$Node, data="only")
  par(op)
  
  plot(uni$Way)
  plot(uni$Relation)
  }
}