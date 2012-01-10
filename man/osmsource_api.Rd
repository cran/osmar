\name{osmsource_api}
\alias{osmsource_api}
\title{API OSM data source}
\usage{
  osmsource_api(url = "http://api.openstreetmap.org/api/0.6/")
}
\arguments{
  \item{url}{URL of the API}
}
\description{
  OSM API version 0.6 data source; see
  \url{http://wiki.openstreetmap.org/wiki/API_v0.6}.
}
\section{Supported request elements}{
  \describe{

  \item{Basic request elements}{See
  \code{\link{get_osm_elements}}.}

  }
}
\seealso{
  \code{\link{get_osm}}, \code{\link{get_osm_elements}}

  Other osmsource: \code{\link{complete_file}},
  \code{\link{osmsource_file}},
  \code{\link{osmsource_osmosis}}
}

