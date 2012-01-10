#' @include get.R
#' @include source.R
{}



#' API OSM data source
#'
#' OSM API version 0.6 data source; see
#' \url{http://wiki.openstreetmap.org/wiki/API_v0.6}.
#'
#' @section Supported request elements:
#'
#' \describe{
#'
#'   \item{Basic request elements}{See
#'     \code{\link{get_osm_elements}}.}
#'
#' }
#'
#' @param url URL of the API
#'
#' @seealso \code{\link{get_osm}}, \code{\link{get_osm_elements}}
#' @family osmsource
#'
#' @export
osmsource_api <- function(url = "http://api.openstreetmap.org/api/0.6/") {
  osmsource(list(url = url), "api")
}



get_osm_data.api <- function(source, what, ...) {
  request <- osm_request(source, what, ...)
  response <- getURL(request, .encoding = "UTF-8")

  response
}



setMethod("osm_request", signature = c("api", "bbox"),
function(source, what, ...) {
  if ( size(what) >= 0.25 )
    stop("BoundingBox is bigger than 0.25-Square-Degrees\n")

  sprintf("%smap/?bbox=%s,%s,%s,%s", source$url,
          what["left"], what["bottom"], what["right"], what["top"])
})



setMethod("osm_request", signature = c("api", "element"),
function(source, what, full = FALSE, ...) {
  element <- class(what)[1]
  r <- sprintf("%s%s/%s", source$url, element, what["id"])
  if ( full )
    r <- sprintf("%s/full", r)

  r
})

