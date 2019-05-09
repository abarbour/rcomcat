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

  .to_posix <- function(x, verbose=FALSE){
    fm <- "%Y-%m-%dT%H:%M:%S"
    timezone <- "GMT"
    if (verbose) message(x)
    xpos <- if (is.character(x) | inherits(x,'POSIXt')){
      x <- strftime(x, format=fm, usetz=FALSE, tz = timezone)
      xp <- as.POSIXlt(x, format=fm, tz=timezone)
      if (any(is.na(xp))) stop('could not coerce to POSIXlt')
      xp
    } else if (inherits(x,'Date')){
      x
    }
    return(format(xpos, format=fm))
  }
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

# @rdname make_comcat_url
# @return data.frame
# @export
#
# @examples
comcat_hypo <- function(...){

    U <- make_comcat_url(...)

    maxeventspersearch <- 20000 # the code also searches for this but I am leaving the default in case that fails
    defaultduration <- 30

    # use orderby=time-asc
    #<scheme>://<net_loc>/<path>;<params>?<query>#<fragment>
    basic <- "https://earthquake.usgs.gov/fdsnws/event/1/" # "http://comcat.cr.usgs.gov/fdsnws/event/1/"
    # basic request for data, always order by time, and use csv format
    basicdata <- paste(basic, "query?", "orderby=time-asc&format=csv", sep = "")
    # basic request for count of data, always order by time
    basiccount <- paste(basic, "count?", "orderby=time-asc", sep = "")

    tfmt <- "%Y-%m-%dT%H:%M:%S"
    fmtz <- "GMT"
	.to_posix <- function(x, fm=tfmt, timezone=fmtz) as.POSIXlt(x, format=fm, tz=timezone)

    # get the maximum number of earthquakes allowed in a search by doing a event count on a small space-time window so it will be quick
    # and using the gosjson format which returns this limit after the count
    # note this search looks for M>=8 earthquakes in the last 10 minutes with hypocenters in a very small, rather inactive area
    # thus it may fail if it ever returns an earthquake.
    Now <- Sys.time()
    Now_pos <- .to_posix(Now)
    tenminutesago <- Now_pos - 10*60
    tenminutesagostring <- strftime(tenminutesago, format = tfmt, tz = fmtz) # convert time to a strong for comcat

    if (FALSE){
		getmaxcount <- paste(basiccount,
			"&format=geojson&minlatitude=39.74881&maxlatitude=39.75012&minlongitude=-105.22078&maxlongitude=-105.21906&minmagnitude=8&starttime=",
			tenminutesagostring,
			sep = ""
		)
		#print(getmaxcount)
		countstr <- readLines(getmaxcount, warn = FALSE)  # retrieve the count
		# get rid of extraneous characters at the start of the line
		countstr <- gsub("^.*wed.:", "", countstr)
		countstr <- gsub("[,}]", "", countstr)
		maxeventspersearch <- as.numeric(countstr)
    }

    # build the request string for limits other than start and end time
    requestlimits <- ""
    if (!is.na(updatedafter)) {
      updatedafterstr <- strftime(updatedafter, format = tfmt)
      requestlimits <- paste(requestlimits, "&updatedafter=", updatedafterstr, sep = "")
    }
    if (!is.na(minlatitude)) requestlimits <- paste(requestlimits, "&minlatitude=", minlatitude, sep = "")
    if (!is.na(maxlatitude)) requestlimits <- paste(requestlimits, "&maxlatitude=", maxlatitude, sep = "")
    if (!is.na(minlongitude)) requestlimits <- paste(requestlimits, "&minlongitude=", minlongitude, sep = "")
    if (!is.na(maxlongitude)) requestlimits <- paste(requestlimits, "&maxlongitude=", maxlongitude, sep = "")
    if (!is.na(latitude)) requestlimits <- paste(requestlimits, "&latitude=", latitude, sep = "")
    if (!is.na(longitude)) requestlimits <- paste(requestlimits, "&longitude=", longitude, sep = "")
    if (!is.na(minradiuskm)) requestlimits <- paste(requestlimits, "&minradiuskm=", minradiuskm, sep = "")
    if ((!is.na(minradius)) & is.na(minradiuskm)) requestlimits <- paste(requestlimits, "&minradius=", minradius, sep = "")
    if (!is.na(maxradiuskm)) requestlimits <- paste(requestlimits, "&maxradiuskm=", maxradiuskm, sep = "")
    if ((!is.na(maxradius)) & is.na(maxradiuskm)) requestlimits <- paste(requestlimits, "&maxradius=", maxradius, sep = "")
    if (!is.na(mindepth)) requestlimits <- paste(requestlimits, "&mindepth=", mindepth, sep = "")
    if (!is.na(maxdepth)) requestlimits <- paste(requestlimits, "&maxdepth=", maxdepth, sep = "")
    if (!is.na(minmagnitude)) requestlimits <- paste(requestlimits, "&minmagnitude=", minmagnitude, sep = "")
    if (!is.na(maxmagnitude)) requestlimits <- paste(requestlimits, "&maxmagnitude=", maxmagnitude, sep = "")
    if (!is.na(catalog)) requestlimits <- paste(requestlimits, "&catalog=", catalog, sep = "")
    if (!is.na(contributor)) requestlimits <- paste(requestlimits, "&contributor=", contributor, sep = "")
    if (!is.na(eventtype)) requestlimits <- paste(requestlimits, "&eventtype=", eventtype, sep = "")
    if (!is.na(reviewstatus)) requestlimits <- paste(requestlimits, "&reviewstatus=", reviewstatus, sep = "")
    if (!is.na(minmmi)) requestlimits <- paste(requestlimits, "&minmmi=", minmmi, sep = "")
    if (!is.na(maxmmi)) requestlimits <- paste(requestlimits, "&maxmmi=", maxmmi, sep = "")
    if (!is.na(mincdi)) requestlimits <- paste(requestlimits, "&mincdi=", mincdi, sep = "")
    if (!is.na(maxcdi)) requestlimits <- paste(requestlimits, "&maxcdi=", maxcd, sep = "")
    if (!is.na(minfelt)) requestlimits <- paste(requestlimits, "&minfelt=", minfelt, sep = "")
    if (!is.na(alertlevel)) requestlimits <- paste(requestlimits, "&alertlevel=", alertlevel, sep = "")
    if (!is.na(mingap)) requestlimits <- paste(requestlimits, "&mingap=", mingap, sep = "")
    if (!is.na(maxgap)) requestlimits <- paste(requestlimits, "&maxgap=", maxgap, sep = "")
    if (!is.na(minsig)) requestlimits <- paste(requestlimits, "&minsig=", minsig, sep = "")
    if (!is.na(maxsig)) requestlimits <- paste(requestlimits, "&maxsig=", maxsig, sep = "")
    if (!is.na(producttype)) requestlimits <- paste(requestlimits, "&producttype=", producttype, sep = "")


    # make sure we have start and end times, if not get defaults
    if (is.na(starttime)) starttime <- Now_pos  - (defaultduration * 24 * 3600) # default is defaultduration days in the past
    if (is.na(endtime)) endtime <- Now_pos + (1 * 24 * 3600) # default is 1 day in the future

    # put times into timepoints array that will get extended as needed to fit the maxeventspersearch limit
    timepoints <- sort(c(starttime, endtime))

    # now build the request strings to do counts for the timespoints and requestlimits and run them to get the counts
    # also do a request that will get the maximum allowed count for a data request
    timepointstrings <- strftime(timepoints, format = tfmt, tz = fmtz)
    nwindows <- length(timepoints) - 1
    countrequests <- rep("", nwindows)
    counts <- rep(0, nwindows)
    countrequests <- basiccount

    if (nchar(requestlimits) > 0) countrequests <- paste0(countrequests, requestlimits)
    countrequests <- paste0(countrequests, "&starttime=", timepointstrings[1])
    countrequests <- paste0(countrequests, "&endtime=", timepointstrings[2])
    # URL request
    counts <- 29544 #scan(countrequests, quiet = TRUE)
    totalevents <- counts

    i <- 1

    # now go into a loop where we split windows with counts > maxeventspersearch
    #too_many <- max(counts) > maxeventspersearch
    while (max(counts) > maxeventspersearch) {
      for (i in 1:nwindows) {
      	print(i)
        if (!is.na(counts[i]))
          if(counts[i] > maxeventspersearch) {
            # split the window from i to i+1, don't do test if counts is NA

            # timepoints gets a new value
            newtimepoint <- midtime(timepoints[i], timepoints[i + 1])
            timepoints <- append(timepoints, newtimepoint, i)
            newtimepointstring <- strftime(newtimepoint, format = tfmt, tz = fmtz)
            timepointstrings <- append(timepointstrings, newtimepointstring, i)

            # counts[i] should now be NA and the one after it should be NA
            counts[i] <- NA
            counts <- append(counts, NA, i)

            # similarly countrequests[i] and [i+1] should be NA
            countrequests[i] <- NA
            countrequests <- append(countrequests, NA, i)

            # build the count requests and get the new counts
            for (j in i:(i + 1)) {
              countrequests[j] <- basiccount
              if (nchar(requestlimits) > 0) countrequests[j] <- paste(countrequests[j], requestlimits, sep = "")
              countrequests[j] <- paste(countrequests[j], "&starttime=", timepointstrings[j], sep = "")
              countrequests[j] <- paste(countrequests[j], "&endtime=", timepointstrings[j + 1], sep = "")
              print(countrequests[j])
              counts[j] <- scan(countrequests[j], quiet = TRUE)
            }

            # and now there is one more window
            nwindows <- nwindows + 1

          } # end of the if loop if a window needs to be split
      } # end of for loop over the number of windows
    } # end of while loop to develop the time windows with less than maxeventspersearch events

	stop()

    # now get the data for each window

    # first develop the requests for each window
    datarequests <- rep("", nwindows)
    for (i in 1:nwindows) {
      datarequests[i] <- basicdata
      if (nchar(requestlimits) > 0) datarequests[i] <- paste(datarequests[i], requestlimits, sep = "")
      datarequests[i] <- paste(datarequests[i], "&starttime=", timepointstrings[i], sep = "")
      datarequests[i] <- paste(datarequests[i], "&endtime=", timepointstrings[i + 1], sep = "")
    }

    # get the data for the first window
    data <- scan(
        datarequests[1],
        list(
          time = "",
          latitude = 0,
          longitude = 0,
          depth = 0,
          mag = 0,
          magtype = "",
          nst = 0,
          gap = 0,
          dmin = 0,
          rms = 0,
          net = "",
          id = "",
          updated = "",
          place = "",
          type = ""
        ),
        sep = ",",
        quote = "\"",
        skip = 1,
        quiet = TRUE
      )

    # if there there are 2 or more windows, then get the data for later windows and append it
    if (nwindows > 1)
      for(i in 2:nwindows) {
        data2 <- scan(
            datarequests[i],
            list(
              time = "",
              latitude = 0,
              longitude = 0,
              depth = 0,
              mag = 0,
              magtype = "",
              nst = 0,
              gap = 0,
              dmin = 0,
              rms = 0,
              net = "",
              id = "",
              updated = "",
              place = "",
              type = ""
            ),
            sep = ",",
            quote = "\"",
            skip = 1,
            quiet = TRUE
          )
        data <- mapply(c, data, data2, SIMPLIFY = FALSE) # mapply applies c (concatenate) to each item (years, mag, location...) in the lists  data and data2
      }

    # transform to a data frame
    dataframe <- data.frame(sapply(data, c))

    # remove non-unique events in case there are events on window boundaries
    dataframe <- unique(dataframe)

    # add POSIXlt format times to the data frame
    rtime <- as.POSIXlt(strptime(dataframe$time, tfmt), tz = "UTC")
    dataframe <- data.frame(dataframe, rtime = rtime)

    # return the data
    return(dataframe)

}


# Calculate the midpoint time between two times
#
# @param early datetime
# @param late datetime
#
# @return datetime
# @export midtime
#
# @examples
# midtime(Sys.time(), Sys.time()-100)
midtime <- function(early, late){
  mid <- early + (difftime(late, early) / 2)
  return(mid)
}
