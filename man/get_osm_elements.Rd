\name{bbox}
\alias{bbox}
\alias{center_bbox}
\alias{get_osm_elements}
\title{Get OSM elements}
\usage{
  bbox(left, bottom, right, top)

  center_bbox(center_lon, center_lat, width, height)
}
\arguments{
  \item{left}{Minimum longitude}

  \item{bottom}{Minimum latitude}

  \item{right}{Maximum longitude}

  \item{top}{Maximum latitutde}

  \item{center_lon}{Center longitude}

  \item{center_lat}{Center latitude}

  \item{width}{Box width}

  \item{height}{Box height}
}
\description{
  Utility functions to specify \emph{what} to get from the
  OSM data source. These are the request elements which
  work for most sources, see the specific sources for
  specialized elements.
}
\seealso{
  \code{\link{osm_descriptors}} \code{\link{get_osm}}
}

