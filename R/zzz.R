.onLoad <- function( libname, pkgname ){
  present.options <- options()
  dwd2r.options <- list(
      dwd2r.download.path = "~/R/dwd_data/"
  )
  ## Check which of the dwd2r options are set by the user in her local
  ## .Rprofile 
  mismatching.options <- !( names( dwd2r.options ) %in%
                         names( present.options ) )
  if ( any( mismatching.options ) ){
    options( dwd2r.options[ mismatching.options ] )
  }

  invisible()
}
