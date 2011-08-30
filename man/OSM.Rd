\name{OSM-class}
\alias{OSM}
\alias{OSM-class}

\title{
  Class for OpenStreeMap information
}
\description{
  This function checks if the order of given list element is right, 
  names the listelements and makes class \code{"OSM"} of these elements.
}
\usage{
  OSM(...)
}

\arguments{
  \item{\dots}{
    three elements: (1)\code{\link{Node-class}},(2)\code{\link{Way-class}},(3)\code{\link{Relation-class}}
  }
}
\details{
  builds a list of three elements and names the elements \code{"Node"}, \code{"Way"} and \code{"Relation"}
}
\value{
  Object of type OSM is returned
}
\section{Methods}{
  \describe{
    \item{plot}{for more details see \code{\link{plot.OSM}}}
    \item{summary}{summary of spatial information and important keys/values}
    \item{findID}{subset of a specific ID}
    \item{findKeyValue}{subset of a specific key or value}
    \item{findTime}{subset of specific changing times}
  }
}

\author{
  Thomas Schlesinger
}
\note{
  This function shouldn't be used by user.
  the Easiest way to make \code{"OSM"} class is by using \code{\link{getOSMObject}}
}

\seealso{
  \code{\link{getOSMObject}}
  \code{\link{Node-class}}
  \code{\link{Way-class}}
  \code{\link{Relation-class}}
}
\examples{
 ## its best to look at source code of the getOSMObject-function by just typing getOSMObject in RConsole
}