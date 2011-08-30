\name{bbox2coords}         
\alias{bbox2coords}

\title{Converting defined area to bounding box}
\description{
  Converts a given centerpoint and height/width (in meters) of the area into a vector of length four.
  This vector contains the 4 edges of the given area in lat/lon-format
}
\usage{bbox2coords(center, area)}

\arguments{
  \item{center}{needs a vector of length two giving the center of the area in 
                the form \command{c(longitude, latitude)}. Only accapting the decimal format.
                }
  \item{area}{needs a vector of length two giving the width & height of the 
              rectangle area in the form \command{c(width, height)}. The unit is meter.}
}
\details{formulas for converting meter to geographical coordinates are used 
         from this webpage \url{http://bse.unl.edu/adamchuk/web_ssm/web_GPS_eq.html}
         (16)-(19).
         }
\value{
  a vector of length 4 giving the edges of the described area in the form:
  \code{c(east longitude, south latitude, west longitude, north latitude)}. all in decimal format.
}
\references{
  using the formulas in this webpage:
  \url{http://bse.unl.edu/adamchuk/web_ssm/web_GPS_eq.html}
}
\author{
  Thomas Schlesinger
}
\note{
  Results can be incorrect at extreme longitude values, e.g 90 or -90.
}

\seealso{
 \code{\link{getBboxXML}}
}
\examples{
  center<- c(11.581646,48.152163)
  area<-c(2000,2000)
  bbox2coords(center, area)
  ## 11.56821 48.14317 11.59509 48.16116
  ## now we have the latitude/longitude values of the 4 edges of the area 
  ## containing 4 square kilometers (2000m x 2000m)
}