\name{Relation-class}
\alias{Relation}
\alias{Relation-class} 

\title{
  Class for Relation information
}
\description{
  Makes a S3-class object called \code{"Relation"}
}
\usage{
  Relation(...)
}

\arguments{
  \item{\dots}{
    expects three elements. 1st element being a \code{"RelationMeta"}-dataframe or \code{"character"}, 
    2nd element a \code{"RelationDataFrame"} or \code{"character"} and
    3rd element a \code{"RelationMember"}-dataframe.
  }  
}
\details{
  Function is not meant to be used by users.
}
\value{
  The third element is a S3-class called \code{"Relation"} with 3 elements. Relations don't
  have Spatial information which can be converted into an object of the sp package.
  (1) MetaInformation about the Relations itself. These usually is stored in the SpatialObjects (in case of Node and Way),
  (2) Data and
  (3) data.frame containing the members and their role in a relation.
  If no Relations where in the XML-File the 3 elements will be character strings. 
}
\section{Methods}{
  \describe{
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
  \code{\link{Way-class}}
  \code{\link{OSM-class}}
}