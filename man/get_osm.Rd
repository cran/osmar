\name{get_osm}
\alias{get_osm}
\title{Get OSM data}
\usage{
  get_osm(x, source = osmsource_api(), ...)
}
\arguments{
  \item{x}{Data identifier; bounding box or specific
  element, see \code{\link{get_osm_elements}}}

  \item{source}{OSM source, e.g.,
  \code{\link{osmsource_api}}}

  \item{...}{Passed to the internal \code{get_osm_data}}
}
\value{
  An \code{\link{osmar}} object
}
\description{
  Get OSM data as \code{\link{osmar}} object from different
  sources by providing a bounding box.
}
\examples{
\dontrun{
  api <- osmsource_api()

  box <- bbox(11.579341, 48.15102, 11.582852, 48.1530)
  gschw <- get_osm(box, source = api)

  kaufstr <- get_osm(way(3810479))
  }
}
\seealso{
  \code{\link{get_osm_elements}},
  \code{\link{osmsource_api}},
  \code{\link{osmsource_osmosis}}
}

