
.query_to_count <- function(u, starttime, endtime){

  stopifnot(inherits(u, 'comcat_url'))

  # set aside attributes and parse u
  att <- attributes(u)
  ul <- httr::parse_url(u)

  # Make the counts version of this
  count_method <- "count"
  ul[['path']] <-  gsub("query", count_method, ul[['path']])
  att[['method']] <- count_method

  # replace starttime and endtime if desired
  if (!missing(starttime)) ul[['query']][['starttime']] <- starttime
  if (!missing(endtime)) ul[['query']][['endtime']] <- endtime

  # rebuild the object
  uc <- httr::build_url(ul)
  attributes(uc) <- att

  return(uc)
}

.to_posix <- function(x, verbose=FALSE, to_char=TRUE){
  fm <- "%Y-%m-%dT%H:%M:%S"
  timezone <- "UTC"
  if (verbose) message(x)
  xpos <- if (is.character(x) | inherits(x,'POSIXt')){
    x <- strftime(x, format=fm, usetz=FALSE, tz = timezone)
    xp <- as.POSIXlt(x, format=fm, tz=timezone)
    if (any(is.na(xp))) stop('could not coerce to POSIXlt')
    xp
  } else if (inherits(x,'Date')){
    x
  }
  if (to_char){
    format(xpos, format=fm)
  } else {
    xpos
  }
}
