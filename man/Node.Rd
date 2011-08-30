\name{Node-class}
\alias{Node}
\alias{Node-class} 

\title{
  Class for Node information
}
\description{
  Makes a S3-class object called \code{"Node"}
}
\usage{
  Node(...)
}

\arguments{
  \item{\dots}{
    expects two elements. 1st element being a \code{"nodeDataFrame"} or \code{"character"},
     2nd element a \linkS4class{SpatialPointsDataFrame} or \code{"character"}
  }  
}
\details{
  Function is not meant to be used by users.
}
\value{
  S3-class called \code{"Node"} with 2 elements: (1)Data and (2) a
  \linkS4class{SpatialPointsDataFrame} with geographical information about the Nodes. If no Nodes 
  where in the XML-File the 2 elements will be character strings.
}
\section{Methods}{
  \describe{
    \item{plot}{for more details see \code{\link{plot.Node}}}
    \item{summary}{summary of spatial information and important keys/values}
    \item{findID}{subset of a specific ID}
  }
}
\author{
  Thomas Schlesinger
}
\seealso{
  \code{\link{getOSMObject}}
  \code{\link{Way-class}}
  \code{\link{Relation-class}}
  \code{\link{OSM-class}}
}