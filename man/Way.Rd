\name{Way-class}
\alias{Way}
\alias{Way-class} 

\title{
  Class for Way information
}
\description{
  Makes a S3-class object called \code{"Way"}
}
\usage{
  Way(...)
}

\arguments{
  \item{\dots}{
    expects three elements. 1st element being a \code{"WayDataFrame"} or \code{"character"}, 
    2nd element a \linkS4class{SpatialLinesDataFrame}, \code{"wayMeta"}-dataframe or \code{"character"} and
    3rd element a \code{"wayMember"}-dataframe.
  }  
}
\details{
  Function is not meant to be used by users.
}
\value{
  S3-class called \code{"Way"} with 3 elements: 
  (1)Data,
  (2)\linkS4class{SpatialLinesDataFrame} with geographical information about the Ways
  and (3) a data.frame containing the NodeIDs which are referenced in a Way. 
  If no Nodes are available, a SpatialLinesDataFrame can not be build. In this case MetaInformation
  about the Ways are stored in the second slot. If no Ways where in the XML-File the 3 elements will
  be character strings. 
}
\section{Methods}{
  \describe{
    \item{plot}{for more details see \code{\link{plot.Way}}}
    \item{summary}{summary of spatial information and important keys/values}
    \item{findID}{subset of a specific ID}
  }
}
\author{
  Thomas Schlesinger
}

\seealso{
  \code{\link{getOSMObject}}
  \code{\link{Node-class}}
  \code{\link{Relation-class}}
  \code{\link{OSM-class}}
}