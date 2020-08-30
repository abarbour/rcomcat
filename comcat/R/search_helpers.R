#' Return event information
#'
#' @details Note that this issues a \code{\link{comcat_query}} for each ID, and so could
#' take considerable time as the number of identifiers grows large.
#'
#' @param id character; event identifier (i.e., \code{'eventid'} in \code{\link{make_comcat_url}}); this
#' can be a vector of identifiers
#' @param verbose logical; should messages be given?
#' @param ... additional arguments to \code{\link{make_comcat_url}}
#'
#' @return a named list of information for each \code{'id'} should it exist; the information for each event is given as
#' a \code{\link{tibble}}; result will be empty for any event id that is bad or returned no information
#' @export
#'
#' @examples
#' \dontrun{
#' event_info(id="us20005awl")
#' event_info(id=c("us20005awl","us1000gf8u"))
#' event_info(id="something bogus")
#' }
event_info <- function(id, ...) UseMethod('event_info')
#' @rdname event_info
#' @export
event_info.default <- function(id, verbose=FALSE, ...){
  id <- as.vector(as.character(id))
  .proc_id <- function(eqid, ...) comcat::make_comcat_url(eventid = eqid, ...)
  Q <- lapply(id, .proc_id, ...)
  class(Q) <- c(class(Q), 'comcat_url_list')
  message(sprintf("Gathering information for %s events...", length(id)))
  QL <- comcat_query(Q, verbose = verbose)
  names(QL) <- id
  QL
}
