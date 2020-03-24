#' @title Search ComCat for basic earthquake data
#'
#' @description
#'  Searches the ComCat database for basic hypocentral data for earthquakes
#'  including origin time, location, magnitude, etc.
#'
#' @details
#'
#' \code{\link{make_comcat_url}} forms a valid url for the given input parameters
#'
#' \code{\link{comcat_query}} posts the url and loads in the results according to \code{'method'}
#'
#' @param starttime,endtime,updatedafter time window parameters
#' @param search_box list containing \code{minlatitude,maxlatitude,minlongitude,maxlongitude} for lat-lon box parameters;
#' note that if this and \code{search_circle} are specificied, the code will throw and error.
#' @param search_circle list containing \code{latitude,longitude,maxradius,maxradiuskm} for circle search parameters;
#' note that if this and \code{search_box} are specificied, the code will throw and error.
#' @param mindepth,maxdepth depth search parameters
#' @param minmagnitude,maxmagnitude magnitude search parameters
#' @param catalog,contributor	catalog and contributor specifications; omitting them gets the "preferred" solution
#' @param reviewstatus 	review status defaults to all or can be automatic or reviewed only
#' @param minmmi,maxmmi Minimum and Maximum values for Maximum Modified Mercalli Intensity reported by ShakeMap.
#' @param mincdi,maxcdi Minimum and Maximum values for Maximum Community Determined Intensity reported by Did You Feel It? forms (DYFI).
#' @param minfelt limit to events with this many DYFI responses
#' @param eventtype the type of event; if \code{eventtype='earthquake'} non-earthquakes
#' are removed from the search. \emph{Warning: there are numerous options (see
#' \url{https://earthquake.usgs.gov/fdsnws/event/1/application.json}, and no arg-checking is done here.}
#' @param alertlevel limit to events with a specific pager alert; checking done if not \code{\link{missing}}
#' @param producttype limits to events with specific product types available; checking done if not \code{\link{missing}}
#' @param method	query or count etc...
#' @param limit integer; the maximum number of events to return -- presently it cannot exceed 20k
#' @param orderby	orering: time-asc, etc...
#' @param format	csv, etc...
#' @param ... additional arguments
#'
#' @return A url with all non-NULL parameters that are specificied; this modifies
#' the \code{\link[httr]{url}} object with an additional attribute for format, making it
#' an object with \code{\link{'comcat_url'}} inheritance.
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
#'
#' # Find out values for different parameters:
#' app <- make_comcat_url(method="application.json")
#' app_values <- comcat_query(app)
#' str(app_values)
#'
#' # get information for a single event with a known identifier
#' u_id <- make_comcat_url( eventid = 'ci14607652')
#' eq_id <- comcat_query(u_id)
#'
#' # see if we can repeat this using the id-based results
#' ot <- eq_id$`time`
#' lat <- eq_id$latitude
#' lon <- eq_id$longitude
#' u_re_id <- make_comcat_url(starttime=ot-30, endtime=ot+30, search_circle=list(latitude=lat, longitude=lon, maxradiuskm=10))
#' eq_re_id <- comcat_query(u_re_id)
#'
#' all.equal(eq_id, eq_re_id)
#' }
#'
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
                            eventid = NULL,
                            reviewstatus = NULL,
                            minmmi = NULL,
                            maxmmi = NULL,
                            mincdi = NULL,
                            maxcdi = NULL,
                            minfelt = NULL,
                            eventtype = 'earthquake',
                            alertlevel,
                            producttype,
                            method = NULL,
                            limit = NULL,
                            orderby = NULL,
                            `format` = NULL,
                            ...
){

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

  if (!is.null(eventid)){
    eventid <- as.character(eventid)
    stopifnot(length(eventid) == 1)
  }

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
    `format`=`format`,
    # Time
    starttime=starttime,
    endtime=endtime,
    updatedafter=updatedafter,
    # Other
    catalog = catalog,
    contributor = contributor,
    eventid =  eventid,
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
    `offset` = NULL,
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
#' @seealso \code{\link{make_adaptive_comcat_url}}
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
    # number of events in search
    # e.g. 10281
    scan(x, what=integer(), quiet = !verbose)
  } else if (method == 'version'){
    # system version
    # e.g., 1.8.1
    base::package_version(scan(x, what=character(), quiet = !verbose))
  } else if (method == 'query') {
    # query result
    # can be multiple types
    if (format == 'csv'){
      suppressMessages(readr::read_csv(x))
    } else if (format == 'geojson'){
      jsonlite::fromJSON(x)
    } else if (format == 'text'){
      suppressMessages(readr::read_delim(x, delim="|"))
    } else if (format == 'kml'){
      rgdal::readOGR(x, verbose = verbose)
    } else if (format %in% c('quakeml','xml')){
      .NotYetImplemented()
    } else {
      stop('unclear how to handle [', format, ']')
    }
  } else if (method == "application.json") {
    # System options
    # e.g.,
    # List of 5
    # $ catalogs      : chr [1:35] "=c" "ak" "at" "atlas" ...
    # $ contributors  : chr [1:23] "admin" "ak" "at" "atlas" ...
    # $ producttypes  : chr [1:49] "associate" "cap" "disassociate" "dyfi" ...
    # $ eventtypes    : chr [1:39] "acoustic noise" "acoustic_noise" "anthropogenic_event" "building collapse" ...
    # $ magnitudetypes: chr [1:32] "2" "4" "fa" "H" ...
    jsonlite::fromJSON(x)
  } else if (method %in% c("catalogs","contributors")) {
    # contributors,catalogs: unstructured xml
    x_x <- XML2R::XML2R(x)
    x_x[,'XML_value']
  } else if (method == "application.wadl") {
    # Unclear how to handle application.wadl: quakeml?
    XML2R::XML2R(x)
  } else {
    stop('unclear how to handle [', method, ']')
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
#' @param refine logical; if the time period is broken up, check for
#' the need to create subwindows. Presently refinement is only once,
#' so results are not (yet) guaranteed.
#' @param verbose logical; should messages be printed?
#'
#' @return data.frame
#' @export
#' @seealso \code{\link{comcat_query}}
#'
#' @examples
#' \dontrun{
#' aq <- make_adaptive_comcat_url(starttime='2016-01-01', endtime='2016-04-01')
#' }
make_adaptive_comcat_url <- function(..., n_segs=NULL, refine=TRUE, verbose=TRUE){

  U <- make_comcat_url(...)
  UC <- convert_to(U, to='count', verbose=FALSE)

  ul <- httr::parse_url(U)
  Params <- ul[['query']]
  param_names <- names(Params)


  result_limit <- 20e3

  if (verbose) message("Checking count for full query...")

  segs_set <- !is.null(n_segs)

  if (segs_set){
    Counts <- NA
  } else {
    # Find out how many will be returned from the base query
    Counts <- comcat_query(UC)
    if (verbose) message(Counts, " earthquakes associated with this query")
  }

  # if there are too many, we adapt the search by time
  #  --> Counts == NA if the user specifies n_seg
  if (is.na(Counts) | (Counts > result_limit)){

    fudge <- 2L
    if (!segs_set){
      n_segs <- 1L + fudge + (Counts - (Counts %% result_limit))/result_limit
    } else {
      n_segs <- as.integer(n_segs)
      stopifnot(n_segs > 1)
    }

    message('re-formulating the search for ', n_segs, ' time windows')
    # generate a list of times for each segment
    Times <- time_limit_splitter(n=n_segs, paramlist=Params, return.list = TRUE, verbose=FALSE)

    ..new_count_url <- function(seg, base_U = U){
      # modify a query url to be counts in a certain timeperiod
      convert_to(base_U, to='count', starttime=seg$Start, endtime=seg$End, verbose=FALSE)
    }

    .process_segment <- function(x){
      # x is a data.frame if segment id, start, and end time
      # .new_count_url returns url with starttime and endtime adjusted
      new_uc <- ..new_count_url(seg = x)
      if (refine){
        rest_counts <- comcat_query(new_uc)
        if (rest_counts > result_limit){
          # refine window with double the number of subwindows
          # note that we do not check to see if subwindows will be under the limit
          seg_Times <- time_limit_splitter(now=x$End, then=x$Start, n = n_segs * 2, return.list = TRUE, verbose=FALSE)
          lapply(seg_Times, ..new_count_url)
        } else {
          # the original window is fine
          list(new_uc)
        }
      } else {
        # assume the original window is fine
        list(new_uc)
      }
    }
    C <- lapply(Times, .process_segment)
  } else {
    # otherwise the base query is fine
    C <- list(U)
  }
  Cu <- unlist(C, recursive = FALSE)
  Q <- lapply(Cu, convert_to, to='query', verbose=FALSE)
  class(Q) <- c(class(Q), 'comcat_url_list')
  return(Q)
}

#' @rdname make_adaptive_comcat_url
#' @method comcat_query comcat_url_list
#' @export
comcat_query.comcat_url_list <- function(x, ...){
  .fun <- function(u, ...){
    message("Trying: ", u)
    cq <- try(comcat_query(u, ...))
    success <- !inherits(cq, 'try-error')
    if (success){
      attr(cq, 'success') <- success
      cq
    } else {
      # use purrr::compact on the resulting list
      warning(u, ' failed\n', cq)
      NULL
    }
  }
  lapply(x, .fun, ...)
}

#' Convert a vector of times to comcat_url objects
#'
#' @details Uses \code{\link{.seq_to_seg}} to generate time-segments
#'
#' @param times vector of times; anything that \code{\link{.to_posix}} can handle
#' @param ... additional parameters sent to \code{\link{make_comcat_url}}
#'
#' @return \code{'comcat_url_list'} object with \code{'comcat_url'} entries
#'
#' @export
times_to_comcat_url <- function(times, ...){
	if (missing(times)){
		Now <- Sys.Date()
		Then <- Now - 7
		times <- seq(Then, Now, by=1)
	} else {
	  nt <- length(na.omit(times))
	  if (nt == 0){
	    stop("no times given")
	  } else if (nt == 1){
	    warning('only one time given!')
	  }
	}
	segtimes <- .seq_to_seg(times, return.list=TRUE)
	Q <- lapply(segtimes, function(seg){
		make_comcat_url(..., starttime=seg$Start, endtime=seg$End)
	})
	class(Q) <- c(class(Q), 'comcat_url_list')
	return(Q)
}
