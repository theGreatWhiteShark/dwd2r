### server-interaction.R - All functions concerned with the
### interaction with FTP server of the DWD

##' @title Get the URLs pointing to the folders at the FTP of the DWD
##'   containing the station data.
##' @description Using a set of different input arguments the user can
##'   specify which folders should be returned.
##'
##' @param period String specifying the period of time covered by the
##'   data, which is accessible through the provided
##'   link. \emph{"recent"} contains only data from the current and
##'   part of the last year. \emph{"historical"} contains the
##'   remainder, and usually the majority, of the recorded
##'   data. Selecting \emph{"both"}, the user can choose to download
##'   the whole time series.
##'
##' @return Character vector.
##'
##' @examples
##' get.dwd.server.url( period = "both" )
##' @author Philipp Mueller
get.dwd.server.url <- function( period = c( "recent", "historical",
                                           "both") ){
  ## Checking of the input                                           
  if ( is.null( period ) ){
    period <- "both"
  }
  period <- match.arg( period )

  ## Static definition of the URLs of the FTP server of the DWD, which
  ## point to folders containing a lot of different station data.
  url.daily.recent <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/"
  url.daily.historical <- "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/"
  
  ## Generating the download URLs
  download.urls <-
    switch( period,
           both = c( url.daily.recent, url.daily.historical ),
           recent = url.daily.recent,
           historical = url.daily.historical )

  return( download.urls )
}
  
## End of server-interaction.R
