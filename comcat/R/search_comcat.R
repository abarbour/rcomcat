#' @title Search ComCat for basic earthquake data
#'
#' @description
#'  Searches the ComCat database for basic hypocentral data for earthquakes
#'  including origin time, location, magnitude, etc.
#'
#' @param starttime,endtime,updatedafter time window parameters
#' @param search_box list containing \code{minlatitude,maxlatitude,minlongitude,maxlongitude} for lat-lon box parameters;
#' note that if this and \code{search_circle} are specificied, the code will throw and error.
#' @param search_circle list containing \code{latitude,longitude,maxradius,maxradiuskm} for circle search parameters;
#' note that if this and \code{search_box} are specificied, the code will throw and error.
#' @param mindepth,maxdepth depth search parameters
#' @param minmagnitude,maxmagnitude magnitude search parameters
#' @param catalog,contributor catalog and contributor specifications; omitting them gets the "preferred" solution
#' @param reviewstatus review status defaults to all or can be automatic or reviewed only
#' @param minmmi,maxmmi Minimum and Maximum values for Maximum Modified Mercalli Intensity reported by ShakeMap.
#' @param mincdi,maxcdi Minimum and Maximum values for Maximum Community Determined Intensity reported by Did You Feel It? forms (DYFI).
#' @param minfelt limit to events with this many DYFI responses
#' @param limit integer; the maximum number of events to return -- presently it cannot exceed 20k
#' @param eventtype the type of event; if \code{eventtype='earthquake'} non-earthquakes
#' are removed from the search. \emph{Warning: there are numerous options (see
#' \url{https://earthquake.usgs.gov/fdsnws/event/1/application.json}, and no arg-checking is done here.}
#' @param alertlevel limit to events with a specific pager alert; checking done if not \code{\link{missing}}
#' @param producttype limits to events with specific product types available; checking done if not \code{\link{missing}}
#' @param ... arguments passed to \code{\link{make_comcat_url}}
#'
#' @return A url with all non-NULL parameters that are specificied; this modifies
#' the \link[httr]{url} object with an additional attribute for format, making it
#' an object with \code{'comcat_url'} inheritance.
#'
#' @export
#'
#' @seealso \link[httr]{build_url} and \link{comcat_hypo}
#'
#' @references \url{https://earthquake.usgs.gov/fdsnws/event/1/},
#'  \url{https://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php}
#'
#' @examples
#' make_comcat_url()
#' make_comcat_url(method='count')
#' make_comcat_url(method='count', starttime='2011-03-09', endtime='2011-03-11') # prior to Tohoku M9
#' make_comcat_url(method='count', starttime='2011-03-11', endtime='2011-03-13') # including Tohoku M9
#' make_comcat_url(method='count', starttime=Sys.time()-30*60)
#' u <- make_comcat_url(search_circle = list(latitude=32,longitude=-120,maxradiuskm=400))
#'
#' all.equal(u, httr::build_url(httr::parse_url(u)), check.attributes = FALSE)
# all.equal(httr::parse_url(u), attr(u,'ul'), check.attributes = FALSE)
#'
#' \dontrun{
#' eqs_raw <- readr::read_csv(u)
#' eqs <- comcat_query(u)
#' all.equal(eqs, eqs_raw)
#'
#' # Plot the earthquakes on a map, scaling them by a proxy for seismic moment
#' plot(latitude ~ longitude, eqs, cex=0.2+scale(10**mag, center=FALSE))
#' summary(eqs)
#' }
make_comcat_url <- function(starttime = NULL,
           endtime = NULL,
           updatedafter = NULL,
           search_box = NULL,
           search_circle = NULL,
           mindepth = NULL,
           maxdepth = NULL,
           minmagnitude = NULL,
           maxmagnitude = NULL,
           catalog = NULL,
           contributor = NULL,
           reviewstatus = NULL,
           minmmi = NULL,
           maxmmi = NULL,
           mincdi = NULL,
           maxcdi = NULL,
           minfelt = NULL,
           method = NULL,
           format = NULL,
           orderby = NULL,
           limit = NULL,
           eventtype = 'earthquake',
           alertlevel,
           producttype){

  if (!is.null(limit)){
    limit <- as.integer(limit)
    stopifnot((limit > 0) & (limit <= 20e3))
  }
  orderby <- match.arg(orderby, c('time-asc','time','magnitude','magnitude-asc'))
  format <- match.arg(format, c("csv","geojson","quakeml","text","kml","kmlraw","xml","cap"))

  producttype <- if (!missing(producttype)){
    match.arg(producttype, c("moment-tensor", "focal-mechanism", "shakemap", "losspager", "dyfi"))
  } else {
    NULL
  }

  alertlevel <- if (!missing(alertlevel)){
    match.arg(alertlevel, c('green','yellow','orange','red'))
  } else {
    NULL
  }

  maxcdi <- as.numeric(maxcdi)
  stopifnot((maxcdi >= 0) & (maxcdi <= 12))

  mincdi <- as.numeric(mincdi)
  stopifnot((mincdi >= 0) & (mincdi <= 12))

  maxmmi <- as.numeric(maxmmi)
  stopifnot((maxmmi >= 0) & (maxmmi <= 12))

  minmmi <- as.numeric(minmmi)
  stopifnot((minmmi >= 0) & (minmmi <= 12))

  minfelt <- as.numeric(minfelt)
  stopifnot((maxmmi > 0))

  if (!is.null(starttime)) starttime <- .to_posix(starttime)
  if (!is.null(endtime)) endtime <- .to_posix(endtime)
  if (!is.null(updatedafter)) updatedafter <- .to_posix(updatedafter)

  search_in_box <- !is.null(search_box)
  #list(minlatitude = NULL, maxlatitude = NULL, minlongitude = NULL, maxlongitude = NULL)
  search_in_circle <- !is.null(search_circle)
  #list(latitude = NULL, longitude = NULL, maxradius = NULL, maxradiuskm = NULL)
  if (search_in_box & search_in_circle) stop('cannot search for events in both a box and a circle')

  # Query method Parameters:
  # by default, build_url compacts the list (strips out NULL) with
  # purrr::compact, and we do the same thing in the following below,
  # so put everything in a single list
  #
  # Note that here NULL means unsupported in this script, which could
  # change in the future
  #
  query <- list(# Formats
                format=format,
                # Time
                endtime=endtime,
                starttime=starttime,
                updatedafter=updatedafter,
                # Other
                catalog = catalog,
                contributor = contributor,
                eventid = NULL,
                includeallmagnitudes = NULL,
                includeallorigins = NULL,
                includearrivals = NULL,
                includedeleted = NULL,
                includesuperseded = NULL,
                limit = limit,
                maxdepth = maxdepth,
                maxmagnitude = maxmagnitude,
                mindepth = mindepth,
                minmagnitude = minmagnitude,
                offset = NULL,
                orderby = orderby,
                # Extensions
                alertlevel = alertlevel,
                callback = NULL,
                eventtype=eventtype,
                jsonerror=NULL,
                kmlanimated=NULL,
                kmlcolorby=NULL,
                maxcdi = maxcdi,
                maxgap = NULL,
                maxmmi = maxmmi,
                maxsig = NULL,
                mincdi = mincdi,
                minfelt = minfelt,
                mingap = NULL,
                minsig = NULL,
                nodata = NULL, #default 204
                producttype = producttype,
                productcode = NULL,
                reviewstatus = reviewstatus
                )
  # Tack on Location options
  if (search_in_circle){
    query <- c(query, search_circle)
  } else if (search_in_box) {
    query <- c(query, search_box)
  }

  # strips out non-NULL components, as build_url does
  query <- purrr::compact(query)

  # Path
  method <- match.arg(method, c("query","count","version","contributors","catalogs","application.wadl","application.json"))
  path <- paste0("fdsnws/event/1/", method)

  # Url contents
  ul <- list(scheme="https",
             hostname="earthquake.usgs.gov",
             port=NULL,
             path=path,
             query=query,
             params=NULL,
             fragment=NULL,
             username=NULL,
             password=NULL)
  class(ul) <- c('url','list')

  #<scheme>://<net_loc>/<path>;<params>?<query>#<fragment>
  comrl <- httr::build_url(ul)
  attr(comrl, 'format') <- format
  attr(comrl, 'method') <- method
  class(comrl) <- c(class(comrl), 'comcat_url')
  return(comrl)
}

#' @rdname make_comcat_url
#' @param x object
#' @param verbose logical; should the query by shown?
#' @export
comcat_query <- function(x, ...) UseMethod('comcat_query')

#' @rdname make_comcat_url
#' @method comcat_query comcat_url
#' @export
comcat_query.comcat_url <- function(x, verbose=TRUE, ...){

  if (verbose){
    message(x)
  }

  format <- attr(x, 'format')
  method <- attr(x, 'method')

  values <- if (method == 'count'){
    scan(x)
  } else if (method == 'query') {
    if (format == 'csv'){
      readr::read_csv(x)
    }
  } else {
    .NotYetImplemented()
  }

  return(values)
}

#' @rdname make_comcat_url
#' @method comcat_query default
#' @export
comcat_query.default <- function(...){
  U <- make_comcat_url(...)
  comcat_query(U)
}

#' @title Adaptively adjust ComCat query based on result limits
#'
#' @param ... parameters passed to \code{\link{make_comcat_url}}
#' @param n_segs integer; the number of segments to split if
#' the result is over the 20k limit (enforced by ComCat webservices)
#' @param verbose logical; should messages be printed?
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' aq <- adaptive_comcat_query(starttime='2016-01-01', endtime='2017-01-01')
#' }
adaptive_comcat_query <- function(..., n_segs=NULL, verbose=TRUE){

  Today <- as.Date(Sys.time(), tz='UTC')
  Last_month <- Today - 30

  U <- make_comcat_url(...)
  UC <- .query_to_count(U)

  ul <- httr::parse_url(U)
  Params <- ul[['query']]
  param_names <- names(Params)

  result_limit <- 20e3

  .limit_splitter <- function(counts, n=NULL, params=list()){
      param_names <- names(params)
      has_start <- 'starttime' %in% param_names
      has_end <- 'endtime' %in% param_names
      starttime <- .to_posix(ifelse(has_start, params[['starttime']], Today), to_char=FALSE)
      endtime <- .to_posix(ifelse(has_end, params[['endtime']], Last_month), to_char=FALSE)
      t_seq <- seq(starttime, endtime, length.out = n)
      lapply(seq_len(n - 1), function(j){
        data.frame(Seg=j, Start=t_seq[j], End=t_seq[j + 1])
        })
  }
  segs_set <- is.null(n_segs)
  # Find out how many will be returned from the base query
  message("Checking eq count for full query...")
  if (segs_set){
    Counts <- NA
  } else {
    Counts <- comcat_query(UC)
    if (verbose) message(Counts, " earthquakes associated with this query")
  }

  if (is.na(Counts) | (Counts > result_limit)){
    # if there are too many, we adapt the search by time
    if (!segs_set){
      n_segs <- 2 + (Counts - (Counts %% result_limit))/result_limit
    } else {
      n_segs <- as.integer(n_segs)
      stopifnot(n_segs > 1)
    }
    message('re-formulating the search for ', n_segs, ' time windows')
    Times <- .limit_splitter(Counts, n=n_segs, params=Params)
    Q <- lapply(Times, function(x){
      .query_to_count(U, starttime=x$Start, endtime=x$End)
    })
  } else {
    Q <- list(U)
  }
  return(Q)
}

