\name{osmsource_osmosis}
\alias{osmsource_osmosis}
\title{Osmosis OSM data source}
\usage{
  osmsource_osmosis(file, osmosis = "osmosis")
}
\arguments{
  \item{file}{The file name (and path) of the planet dump}

  \item{osmosis}{The path to the osmosis application}
}
\description{
  Planet dumps as OSM data source through the osmosis
  command line Java application.
}
\details{
  Osmosis is a command line Java application for processing
  OSM data. It allows, among other things, to extract data
  inside a bounding box or polygon from so called planet
  dumps. The usage of this source requires an installed
  osmosis; see
  \url{http://wiki.openstreetmap.org/wiki/Osmosis}.
}
\section{Supported request elements}{
  \describe{

  \item{Basic request elements}{See
  \code{\link{get_osm_elements}}.}

  \item{osmosis_args}{Special argument which enables to use
  osmosis syntax.}

  }
}
\seealso{
  \code{\link{get_osm}}, \code{\link{get_osm_elements}}

  Other osmsource: \code{\link{complete_file}},
  \code{\link{osmsource_api}}, \code{\link{osmsource_file}}
}

