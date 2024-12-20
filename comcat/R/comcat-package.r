#' ANSS ComCat search
#'
#' @description
#'
#' [INITIAL DOCS -- WILL CHANGE...]
#'
#'  usage: save this file in searchcomcat.R (the name does not actually
#'  matter) these functions only use the basic level R installation and so
#'  no other packages need to be installed or loaded. in your R script or
#'  session do the following: source("searchcomcat.R")  (use the full path
#'  name if it is in another directory) comcatdata <- comcathypocsv()
#'  (this retrieves all earthquakes for the past 30 days into a data frame
#'  called comcatdata) to apply different search criteria see the parameters
#'  listed in the function call below or at
#'  http://comcat.cr.usgs.gov/fdsnws/event/1/, e.g. comcatdata <-
#'  comcathypocsv(minmagnitude=5)   (this will return only M>=5 earthquakes)
#'  comcatdata <- comcathypocsv(alertlevel="red") (this will return only
#'  earthquakes with a PAGER level of red)
#'
#'  this function returns a dataframe (comcatdata in the above examples)
#'  that has columns which are from the ComCat csv format and one additional
#'  column, named rtime, which is the time converted to a POSIXlt format
#'  which can be used for computations in R to explore the output of the
#'  frame try commands such as: colnames(comcatdata)  (will return the names
#'  of the columns) nrow(comcatdata)   (will return the number of events
#'  found by the search)
#'
#'  this script lets you set many parameters but only uses the ones that are
#'  set to something other than NA it has few defaults beyond the ones
#'  applied by Comcat and it does very little parameter checking
#'
#'  ordering is always done by ascending time to make it easier to deal with
#'  the need to search by multiple windows
#'
#'  start, end, and updatedafter times should be given as R internal POSIXlt
#'  classes so that the script can work with them.  See the R function
#'  Sys.time in the base class numeric parameters (e.g. minmagnitude) should
#'  be given as numbers character string parameters (e.g. eventtype or
#'  alertlevel) should be given as strings
#'
#'  time is assumed to be in UTC, the function does not check the time zone
#'  for the times that are given
#'
#'  the defaults it has are: 1: endtime defaults to 1 day in the future from
#'  system time.  This lets you search without worrying that time is passing
#'  as the program does things 2: starttime defaults to defaultduration days
#'  in the past from system time, set below 3: I put in the default
#'  eventtype=earthquake so that it does not return blasts unless you want
#'  them, set to NA if you don't want this applied
#'
#'  parameter checking is limited to: if min or max radius is set in both km
#'  or degrees then the km version takes precedence it makes sure that
#'  starttime is before endtime
#'
#'  if a parameter is not set in the call to the function then it is not
#'  used in the search and Comcat defaults are used see
#'  http://comcat.cr.usgs.gov/fdsnws/event/1/ for more info on parameters
#'  and their defaults
#'
#'  FUNCTION STARTS HERE.  PARAMETERS ARE SHOWN WITH THEIR DEFAULTS AND DO
#'  NOT NEED TO BE GIVEN UNLESS YOU WANT TO CHANGE THEM. NA means Not
#'  Available and the parameter will not be used (except for the defaults on
#'  start and end times)
#'
#' @name comcat
#' @import httr readr
#' @importFrom purrr compact
#' @importFrom jsonlite fromJSON
#' @importFrom XML2R XML2R
# @importFrom rgdal readOGR
#' @importFrom stats na.omit
#' @docType package
NULL
