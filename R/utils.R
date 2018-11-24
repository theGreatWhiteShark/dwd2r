##' @title Convert different objects into Date
##' @description An auxiliary function to convert the date format used
##'   in the files on the FTP server of the DWD into a format, which
##'   can be handled by the \pkg{lubridate} package.
##'
##'   It contains methods for the classes \emph{numeric},
##'   \emph{integer}, and \emph{character}. But be careful: These
##'   functions were only designed to work with the content of the DWD
##'   server and might result in odd behavior on different files!
##'
##' @param input.data A vector containing the input date.
##' @param origin Day the numerical representation of the date will be
##'   zero. Per default, as used in the POSIX standard, it is the
##'   string "1970-01-01".
##' 
##' @return Vector of \emph{Date} objects of the same length as the
##'   input vector \code{input.data}.
##' 
##' @author Philipp Mueller
convert.date <- function( input.data, origin = "1970-01-01" ){
  UseMethod( "convert.date" )
}
##' @title Convert character objects into Date
##' @description An auxiliary function to convert the date format used
##'   in the files on the FTP server of the DWD into a format, which
##'   can be handled by the \pkg{lubridate} package.
##'
##' @param input.data A character vector containing the input date.
##' @param origin Day the numerical representation of the date will be
##'   zero. Per default, as used in the POSIX standard, it is the
##'   string "1970-01-01".
##' 
##' @return Vector of \emph{Date} objects of the same length as the
##'   input vector \code{input.data}.
##' 
##' @author Philipp Mueller
convert.date.character <- function( input.data, origin = "1970-01-01" ){
  return( as.Date( input.data, format = "%Y%m%d" ) )
}
##' @title Convert integer objects into Date
##' @description An auxiliary function to convert the date format used
##'   in the files on the FTP server of the DWD into a format, which
##'   can be handled by the \pkg{lubridate} package.
##'
##' @param input.data An integer vector containing the input date.
##' @param origin Day the numerical representation of the date will be
##'   zero. Per default, as used in the POSIX standard, it is the
##'   string "1970-01-01".
##' 
##' @return Vector of \emph{Date} objects of the same length as the
##'   input vector \code{input.data}.
##' 
##' @author Philipp Mueller
convert.date.integer <- function( input.data, origin = "1970-01-01" ){
  if ( !is.integer( input.data ) )
    stop( "data provided for convert.date.integer has the wrong format. Class 'integer' is required!" )
  output.data <- as.Date( as.character( input.data ),
                         format = "%Y%m%d" )
  return( output.data )
}
##' @title Convert numerical objects into Date
##' @description An auxiliary function to convert the date format used
##'   in the files on the FTP server of the DWD into a format, which
##'   can be handled by the \pkg{lubridate} package.
##'
##' @param input.data A numerical vector containing the input date.
##' @param origin Day the numerical representation of the date will be
##'   zero. Per default, as used in the POSIX standard, it is the
##'   string "1970-01-01".
##' 
##' @return Vector of \emph{Date} objects of the same length as the
##'   input vector \code{input.data}.
##' 
##' @author Philipp Mueller
convert.date.numeric <- function( input.data, origin = "1970-01-01" ){
  if ( !is.numeric( input.data ) )
    stop( "data provided for convert.date.numeric has the wrong format. Class 'numeric' is required!" )
  output.data <- as.Date( input.data, origin = origin )
  return( output.data )
}
