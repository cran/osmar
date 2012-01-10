#' @include source.R
#' @include osm-descriptors.R
#' @include as-osmar.R
{}



#' Get OSM data
#'
#' Get OSM data as \code{\link{osmar}} object from different sources
#' by providing a bounding box.
#'
#' @param x Data identifier; bounding box or specific element, see
#'   \code{\link{get_osm_elements}}
#' @param source OSM source, e.g., \code{\link{osmsource_api}}
#' @param ... Passed to the internal \code{get_osm_data}
#'
#' @return An \code{\link{osmar}} object
#'
#' @examples
#'   \dontrun{
#'   api <- osmsource_api()
#'
#'   box <- bbox(11.579341, 48.15102, 11.582852, 48.1530)
#'   gschw <- get_osm(box, source = api)
#'
#'   kaufstr <- get_osm(way(3810479))
#'   }
#'
#' @seealso \code{\link{get_osm_elements}},
#'   \code{\link{osmsource_api}}, \code{\link{osmsource_osmosis}}
#'
#' @export
get_osm <- function(x, source = osmsource_api(), ...) {
  raw <- get_osm_data(source, x, ...)

  ret <- xmlParse(raw)
  ret <- as_osmar(ret)
  #attr(ret, "identifier") <- x
  #attr(ret, "source") <- source

  ret
}



### Bounding box: ####################################################


#' Get OSM elements
#'
#' Utility functions to specify \emph{what} to get from the OSM data
#' source. These are the request elements which work for most sources,
#' see the specific sources for specialized elements.
#'
#' @param left Minimum longitude
#' @param bottom Minimum latitude
#' @param right Maximum longitude
#' @param top Maximum latitutde
#'
#' @seealso \code{\link{osm_descriptors}} \code{\link{get_osm}}
#'
#' @rdname get_osm_elements
#' @aliases get_osm_elements
#'
#' @export
bbox <- function(left, bottom, right, top) {
  structure(c(left = left, bottom = bottom,
              right = right, top = top), class = "bbox")
}



#' @param center_lon Center longitude
#' @param center_lat Center latitude
#' @param width Box width
#' @param height Box height
#'
#' @rdname get_osm_elements
#'
#' @export
center_bbox <- function(center_lon, center_lat, width, height) {
  stopifnot(center_lon <= 180 & center_lon >= -180)
  stopifnot(center_lat <= 90 & center_lat >= -90)

  width <- width / 2
  height <- height / 2

  a <- 6378137
  esq <- (2 - (1/298.257223563)) * (1/298.257223563)
  W <- sqrt(1 - esq * (sin(center_lat * pi/180))^2)
  M <- a * (1 - esq)/W^3
  mPerLatD <- 1/((pi/180) * M)
  top <- center_lat + mPerLatD * height
  bottom <- center_lat - mPerLatD * height
  N <- a/W
  mPerLonD <- 1/((pi/180) * N * cos(center_lat * pi/180))
  left <- center_lon - mPerLonD * width
  right <- center_lon + mPerLonD * width

  if (left < -180)
    left <- left + 360
  if (right > 180)
    right <- right - 360

  bbox(left, bottom, right, top)
}


size <- function(x, ...) {
  UseMethod("size")
}


size.bbox <- function(x) {
  unname((x[1] - x[3]) * (x[2] - x[4]))
}


