#' @include get.R
#' @include source.R
{}



#' Osmosis OSM data source
#'
#' Planet dumps as OSM data source through the osmosis command line
#' Java application.
#'
#' Osmosis is a command line Java application for processing OSM
#' data. It allows, among other things, to extract data inside a
#' bounding box or polygon from so called planet dumps. The usage of
#' this source requires an installed osmosis; see
#' \url{http://wiki.openstreetmap.org/wiki/Osmosis}.
#'
#' @section Supported request elements:
#'
#' \describe{
#'
#'   \item{Basic request elements}{See
#'     \code{\link{get_osm_elements}}.}
#'
#'   \item{osmosis_args}{Special argument which enables to use osmosis
#'     syntax.}
#'
#' }
#'
#' @param file The file name (and path) of the planet dump
#' @param osmosis The path to the osmosis application
#'
#' @seealso \code{\link{get_osm}}, \code{\link{get_osm_elements}}
#' @family osmsource
#'
#' @export
osmsource_osmosis <- function(file, osmosis = "osmosis") {
  osmsource(list(file = file, osmosis = osmosis), "osmosis")
}



get_osm_data.osmosis <- function(source, what, ...) {
  destination <- tempfile()

  request <- osm_request(source, what, destination)
  ret <- system(request, ...)
  response <- readLines(destination)

  unlink(destination)

  response
}



setMethod("osm_request", signature = c("osmosis", "bbox"),
function(source, what, destination, ...) {
  fin <- sprintf("--read-xml enableDateParsing=no file=%s", source$file)
  fout <- sprintf("--write-xml file=%s", destination)

  args <- sprintf("--bounding-box top=%s left=%s bottom=%s right=%s",
                  what["top"], what["left"], what["bottom"], what["right"])

  sprintf("%s %s %s %s", source$osmosis, fin, args, fout)
})



setMethod("osm_request", signature = c("osmosis", "element"),
function(source, what, ...) {
  stop("Not implemented yet.")

  fin <- sprintf("--read-xml enableDateParsing=no file=%s", source$file)
  fout <- sprintf("--write-xml file=%s", destination)

  args <- ""


  sprintf("%s %s %s %s", source$osmosis, fin, args, fout)
})




### Special omsosis requester: #######################################

setOldClass(c("osmosis_args"))

osmosis_args <- function(...) {
  structure(paste(..., collapse = " "), class = "osmosis_args")
}

setMethod("osm_request", signature = c("osmosis", "osmosis_args"),
function(source, what, destination, ...) {
  fin <- sprintf("--read-xml enableDateParsing=no file=%s", source$file)
  fout <- sprintf("--write-xml file=%s", destination)

  args <- unclass(what)

  sprintf("%s %s %s %s", source$osmosis, fin, args, fout)
})


