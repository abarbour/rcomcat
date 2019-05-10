#' Change the method in a comcat_url object
#'
#' @param u \code{\link{comcat_url}} object
#' @param to query-method to convert to; currently only \code{'count'} and \code{'query'}
#' are supported, and if this is left blank the function
#' will choose \code{'count'} if the current query-method is neither of the supported options,
#' or choose the opposite value if the current method is one or the other of the
#' supported options.
#' @param starttime start time of the query; this will supersede and
#' value already found in \code{u}
#' @param endtime end time of the query; this will supersede and
#' value already found in \code{u}
#' @param verbose logical; should messages be displayed?
#' @param to_pos logical; if \code{starttime} and/or \code{endtime}
#' are supplied, should they be converted to the necessary format?
#' @param ... additional arguments
#'
#' @return \code{\link{comcat_url}} object
#' @export
#'
#' @examples
#' u0 <- make_comcat_url(method='query')
#' uc <- convert_to(u0) # convert query to count
#' u <- convert_to(uc) # restore query
#' all.equal(u0,u)
convert_to <- function(u, ...) UseMethod('convert_to')

#' @rdname convert_to
#' @method convert_to comcat_url
#' @export
convert_to.comcat_url <- function(u, to, starttime, endtime, verbose=TRUE, to_pos=FALSE, ...){

	# Right now these are the only options
	# [ ] if more are added, the method of choosing from missing(to)
	# will need to be reconsidered
	method_options <- c('count', 'query')

	# comcat_url objects have method as an attribute
	att <- attributes(u)
	current_method <- att[['method']]

	# decide on a method to convert to
	if (missing(to)){
		# flip methods
	  to_i <- which(method_options != current_method)
	  if (length(to_i)==0) to_i <- 1
		to <- method_options[to_i]
	} else {
		# specified by user
		to <- match.arg(to, method_options)
	}

	already.method <- current_method == to
	if (already.method){
		# nothing to do
		if (verbose) message('url is already a ', to, '-method. choices for "to"-arg are:\t', paste(method_options, collapse=", or "))
		return(u)

	} else {
		# need to convert
		if (verbose) message("Converting from ", current_method, "-method to ", to, "-method")

		# deconstruct the url
		ul <- httr::parse_url(u)

		# update method
		ul[['path']] <-  gsub(current_method, to, ul[['path']], fixed=TRUE)
		# Example:
		# (change)
		#$path
		#[1] "fdsnws/event/1/query"
		# (to)
		#$path
		#[1] "fdsnws/event/1/count"

		# and update attributes
		att[['method']] <- to

		# replace starttime and endtime if specified
		if (!missing(starttime)){
			if (verbose) message('replacing starttime with ', starttime)
			if (to_pos) starttime <- .to_posix(starttime, to_char=TRUE)
			ul[['query']][['starttime']] <- starttime
		}
		if (!missing(endtime)){
			if (verbose) message('replacing endtime with ', starttime)
			if (to_pos) endtime <- .to_posix(endtime, to_char=TRUE)
			ul[['query']][['endtime']] <- endtime
		}

		# (re)build the url object with updated method and attributes
		uc <- httr::build_url(ul)
		attributes(uc) <- att

		return(uc)

	}
}

# Convert a query (search method) to count (search method)
# [ ] convert into method
.query_to_count <- function(u, starttime, endtime, verbose=TRUE){

  stopifnot(inherits(u, 'comcat_url'))

  # set aside attributes and parse u
  att <- attributes(u)
  current_method <- att[['method']]

  ul <- httr::parse_url(u)

  count_method <- "count"
  already.count <- current_method == count_method

  if (!already.count){
    # Make the counts version of this
    ul[['path']] <-  gsub("query", count_method, ul[['path']])
    att[['method']] <- count_method
  } else {
    if (verbose) message('url is already count-method.')
  }

  # replace starttime and endtime if specified
  if (!missing(starttime)){
    if (verbose) message('replacing starttime with ', starttime)
    ul[['query']][['starttime']] <- starttime
  }
  if (!missing(endtime)){
    if (verbose) message('replacing endtime with ', starttime)
    ul[['query']][['endtime']] <- endtime
  }

  # (re)build the url object
  uc <- httr::build_url(ul)
  attributes(uc) <- att

  return(uc)
}

# (optionally) convert to POSIX and then format as needed for ComCat queries
.to_posix <- function(x, verbose=FALSE, to_char=TRUE){
  fm <- "%Y-%m-%dT%H:%M:%S"
  timezone <- "UTC"
  if (verbose) message('Converting ', x, " with [", fm, "]")
  xpos <- if (is.character(x) | inherits(x,'POSIXt')){
    x <- strftime(x, format=fm, usetz=FALSE, tz = timezone)
    xp <- as.POSIXlt(x, format=fm, tz=timezone)
    if (any(is.na(xp))) stop('could not coerce to POSIXlt')
    xp
  } else if (inherits(x,'Date')){
    x
  } else {
    .NotYetImplemented()
  }

  if (to_char){
    format(xpos, format=fm)
  } else {
    xpos
  }
}
