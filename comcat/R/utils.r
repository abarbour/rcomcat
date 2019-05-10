convert_to <- function(u, ...) UseMethod('convert_to')
convert_to.comcat_url <- function(u, to=NULL, starttime, endtime, verbose=TRUE){
  options <- c(count='count', query='query')
  to <- match.arg(to, options)
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
