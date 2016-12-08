list.of.packages <- c("dplyr", "gmt")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(dplyr)


#' Data for every Buss, Train, Tram and Ferry Service
#'
#' Bla bla bla bla
#' Bla bla bla bla
#' Bla bla bla bla
#' Bla bla bla bla
#'
#' @docType data
#'
#' @usage data(services)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#'
#' @source \href{http://example.com}{Example example}
#'
#' @examples
#' data(services)
#' lines <- services$line

#' Schedule Data for every Buss, Train, Tram and Ferry Stop
#'
#' Bla bla bla bla
#' Bla bla bla bla
#' Bla bla bla bla
#' Bla bla bla bla
#'
#' @docType data
#'
#' @usage data(stops)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#'
#' @source \href{http://example.com}{Example example}
#'
#' @examples
#' data(services)
#' station_index <- services$stationIndex

#' Location and Name Data for every Buss, Train, Tram and Ferry Stations
#'
#' Bla bla bla bla
#' Bla bla bla bla
#' Bla bla bla bla
#' Bla bla bla bla
#'
#' @docType data
#'
#' @usage data(stations)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#'
#' @source \href{http://example.com}{Example example}
#'
#' @examples
#' data(stations)
#' names <- services$name

#' Data For Mapping Services to Dates
#'
#' Bla bla bla bla
#' Bla bla bla bla
#' Bla bla bla bla
#' Bla bla bla bla
#'
#' @docType data
#'
#' @usage data(footnotes)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#'
#' @source \href{http://example.com}{Example example}
#'
#' @examples
#' data(footnotes)
#' date <- footnote$footnoteId





# ############################################################
# ## Functionality starts:
#
# ## Function that finds the n closest bus stops, n=5 as default
#

get_closest_stops <- function(lon, lat, n=5) {
  data(stations) #stations <- get_stations()

  return(

    stations %>%

      mutate(tmp_lon=lon, tmp_lat=lat, dist=gmt::geodist(stationLat,stationLon, tmp_lat, tmp_lon)*1000) %>%
      top_n(n,-dist) %>%
      select(-tmp_lon, -tmp_lat)

  )

}


# ## Function that returns the n next departures from the selected stop, n=5 as default
#

get_next_depts <- function(stId = 2214206, time, n=5) {
  data(stops) # stops <- get_stops()
  data(services) # services <- get_services()
  data(footnotes) # footnotes <- get_services()

  stops %>%
    filter(stationIndex==stId) %>%
    left_join(services,by="serviceId") %>%
    left_join(footnotes, by="footnoteId") %>%
    filter(as.POSIXct(date)==format(time,"%Y-%m-%d")) %>%
    mutate(date_arrival =
                    as.POSIXct(paste(format(time,"%Y-%m-%d"),
                                     sprintf("%04d",
                                             as.integer(sub("^24","00",
                                                            arrival)))),
                               format="%Y-%m-%d %H%M"),
           time_diff=date_arrival-time) %>%
    filter(time_diff > 0) %>%
    top_n(n,-time_diff)

}

#' Get Busses for a given location
#'
#' Returns IDs for the bus stop and expected times for the expected five (default)
#' busses. The function returns both timestamps for the bus arrivals as
#' well as minutes.
#'
#' @param lon numeric containing current longitude coordinate in format WGS86
#' @param lat numeric containing current latitude coordinate in format WGS86
#' @time POSIXct time indicating the moment of the client requesting the data.
#' @details
#' This function was built for an API to serve the HBL Commute App
#' @export


get_buses <- function(lon = 24.77770,
                      lat = 60.17746,
                      time = as.POSIXct("2016-12-07 15:00",
                                        format="%Y-%m-%d %H:%M")) {


  closest_stops <- get_closest_stops(lon,lat)

  depts <- get_next_depts(closest_stops$stationIndex[1], time)

  return(depts)

}
