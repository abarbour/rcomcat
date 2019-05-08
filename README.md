Search the ANSS ComCat comprehensive earthquake catalog
for basic hypocentral data using the csv format
( see https://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php although that documentation is missing the type parameter at the end )

Usage:

First install the package:
From the command line:
`R CMD INSTALL comcat`
From within R, in this directory:
```{r}
install.packages('comcat')
```
or
```{r}
library(devtools)
install_github('abarbour/rcomcat/comcat')
```

Then it can be used at will:
```{r}
library(comcat)
eqdata <- comcat_hypo() #  (this retrieves all earthquakes for the past 30 days into a data frame called comcatdata)
```

To apply different search criteria see the parameters listed in the function
call below or at https://earthquakes.usgs.gov/fdsnws/event/1/, e.g.:

```{r}
eqdata <- comcathypocsv(minmagnitude=5) #  (this will return only M>=5 earthquakes)
eqdata <- comcathypocsv(alertlevel="red") # (this will return only earthquakes with a PAGER level of red)
```

This function returns a dataframe (`eqdata` in the above examples) that has columns which are from the ComCat csv format
and one additional column, named `rtime`, which is the time converted to a
`POSIXlt` format which can be used for computations in R

to explore the output of the frame try commands such as:
```
colnames(eqdata) # (will return the names of the columns)
nrow(eqdata) #  (will return the number of events found by the search)
```

This script lets you set many parameters but only uses the ones that are set to
something other than `NA`

It has few defaults beyond the ones applied by Comcat and it does very little
parameter checking.

Ordering is always done by ascending time to make it easier to deal with the
need to search by multiple windows.

The parameters `starttime`, 
`endtime`, and `updatedafter`
should be given as R internal POSIXlt classes -- or one that can be coerced to
POSIXlt -- so that the script can work with them.  See the R function
`base::Sys.time`.
Times are assumed to be in UTC.

numeric parameters (e.g. minmagnitude) should be given as numbers

character string parameters (e.g. eventtype or alertlevel) should be given as strings

the defaults it has are:

1. `endtime` defaults to 1 day in the future from system time.  This lets you search without worrying that time is passing as the program does things
2. `starttime` defaults to `defaultduration` days in the past from system time, set below
3. Earthquakes only (`eventtype='earthquake'`) so that it does not return blasts unless you want them, set to NA if you don't want this applied

parameter checking is limited to:

if min or max radius is set in both km or degrees then the km version takes precedence

it makes sure that starttime is before endtime


If a parameter is not set in the call to the function then it is not used in
the search and Comcat defaults are used; see
https://earthquakes.usgs.gov/fdsnws/event/1/ for more info on parameters
and their defaults

see the function itself for the full list of parameters in the function call.
Parameters are shown with their defaults and do not need to be given unless you
want to change them.
