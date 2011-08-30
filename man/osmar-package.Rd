\name{osmar-package}
\alias{osmar-package}
\alias{osmar}
\docType{package}
\title{
  Importing and working with OpenStreetMapData
}
\description{
  This package imports the (Spatial and Non-Spatial) Data of OpenStreetMap into objects of R by using the OSM-API v0.6.
  For now it is reduced on using various commands of type "GET", like getting data of a explicit element or a region defined by coordinates.
  It also includes some methods on working with the newly created objects.
}
\details{
\tabular{ll}{
    Package: \tab osmar\cr
    Type: \tab Package\cr
    Version: \tab 1.0\cr
    Date: \tab 2011-08-30\cr
    License: \tab GPL-2\cr
    Depends: \tab XML, RCurl, sp, rgdal, gtools\cr
}

}
\author{
  Thomas Schlesinger and Manuel Eugster
  
  Maintainer: Thomas Schlesinger <tho.schlesinger@googlemail.com>
}

\keyword{ package }
\seealso{
    \code{\link{getOSMObject}}
    \code{\link{sp}}
}