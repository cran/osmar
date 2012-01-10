#' @include get.R
#' @include source.R
{}



#' OSM file data source
#'
#' Imports the complete OSM file.
#'
#' @section Supported request elements:
#'
#' \describe{
#'
#'   \item{\code{compete_file}}{Dummy request element.}
#'
#' }
#'
#' @param file The file name (and path) of the osm file
#'
#' @examples
#'   \dontrun{
#'   get_osm(complete_file(), source = osmsource_file("muc.osm"))
#'   }
#'
#' @seealso \code{\link{get_osm}}, \code{\link{get_osm_elements}}
#' @family osmsource
#'
#' @export
osmsource_file <- function(file) {
  osmsource(list(file = file), "osmfile")
}



get_osm_data.osmfile <- function(source, what, ...) {
  readLines(source$file)
}



#' @rdname osmsource_file
#' @export
complete_file <- function() {
  structure(c(from = 0, to = Inf), class = "complete_file")
}
