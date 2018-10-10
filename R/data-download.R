### data-download.R - a set of function to download the content of the
##    FTP server of the DWD
##' @title List all files at a certain path of the FTP server.
##' @description Return a character vector with the full URI of the
##'   individual files stored at the provided URLs.
##'
##' @param url String or character vector pointing to folders at the
##'   FTP of the DWD.
##'
##' @importFrom RCurl getURL
##' @importFrom RCurl url.exists
##'
##' @return Character vector containing the full URI of the individual
##'   files.
##' @author Philipp Mueller
list.files.in.url <- function( url ){
  ## Test the input
  if ( class( url ) != "character" ){
    stop( "The input of 'dwd2r:::list.file.in.url' must be a character vector" )
  }
  ## For some reason there is a severe bug in the RCurl package right
  ## now (version 1.95-4.11) causing a core dump when checking for the
  ## existence of the URLs using lapply. Therefore, we will do it the
  ## old-fashioned way.
  check.existence <- logical( length = length( url ) )
  for ( ll in 1 : length( url ) ){
    ## Avoid being recognized as a bot by the DWD server.
    Sys.sleep( .001 )
    check.existence[ ll ] <- RCurl::url.exists( url[ ll ] )
  }
  if ( !all( check.existence ) ){
    stop( "Server is not reachable. You either entered a wrong URL or have no sufficient connection to the internet." )
  }
  uri.list <-
    paste0( url[ 1 ],
           strsplit( RCurl::getURL(
                                url[ 1 ],
                                ## Follow the links if the FTP wants to
                                ## redirect instead.
                                followlocation = TRUE,
                                ## Only ask for the names, not the
                                ## sizes, dates etc.
                                dirlistonly = TRUE ), '\n' )[[ 1 ]] )
  ## If the provided url argument is a character vector containing
  ## different URLs, process the remaining ones too.
  if ( length( url ) > 1 ){
    for ( uu in ( 2 : length( url ) ) ){
      uri.list <-
        c( uri.list,
          paste0( url[ uu ],
                 strsplit(
                     RCurl::getURL(
                                url[ uu ],
                                ## Follow the links if the FTP wants to
                                ## redirect instead.
                                followlocation = TRUE,
                                ## Only ask for the names, not the
                                ## sizes, dates etc.
                                dirlistonly = TRUE ), '\n' )[[ 1 ]] ) )
    }
  }
  return( uri.list )
}

##' @title Download files from the FTP server of the DWD
##' @description Downloads the content of the supplied URL. It is
##'   intended to work with URLs pointing at folders of the FTP.
##' @details The main reason why to use this function over the
##'   \emph{wget}, which is used internally for the download, is that
##'   it is automatically updates your data.
##'
##'   When downloading content for the first time, the function
##'   reproduces the directory tree of the FTP server of the DWD and
##'   puts the specified files in the corresponding
##'   subfolders. Whenever the function is invoked another time, it
##'   compares the content of the folder in the URL with the content
##'   of the corresponding local folder. All deleted files will be
##'   removed, new files will be downloaded, and 
##'   all the others will stay untouched, which saves a lot of time
##'   when updating your data base.
##' 
##'   The folder, which will contain all the source files downloaded
##'   from the FTP server of the DWD will be set via \code{option(
##'   "dwd2r.download.path" )}. Per default it is set to the
##'   \emph{R/dwd_data} folder in your home. If you wish to change
##'   this default path, you have to override this option in your
##'   \emph{.Rprofile} configuration file in your home. In addition,
##'   the download folder can be supplied manually using the
##'   \strong{download.folder} argument.
##'
##' @param url Either a character vector of one or more URLs to
##'   folders of the FTP server of the DWD or the output of the
##'   \code{\link{get.dwd.ftp.url}} function.
##' @param download.folder Manual override for the default download
##'   folder of the package. If NULL, the default value will be used
##'   instead. Default = NULL.
##' @param quiet Whether or not to verbose the download
##'   procedure. Default = FALSE.
##'
##' @export
##'
##' @return \code{invisible( TRUE )}
##' @author Philipp Mueller
download.content <- function( url, download.folder = NULL,
                             quiet = FALSE ){
  ## Extract the individual files from the provided URL.
  if ( class( url ) == "character" ){
    url.files <- list.files.in.url( url )
  } else if ( class( url ) == "list" &&
              names( url ) %in% c( "data", "meta" ) &&
              class( url$data ) == "character" ){
    ## The format returned by the get.dwd.ftp.url function.
    url.files <- list.files.in.url( url$data )
  } else {
    stop( "Unrecognized format or class of the 'url' argument" )
  }

  ## Folder, which will contain the downloaded content.
  if ( is.null( download.folder ) ){
    download.folder <- getOption( "dwd2r.download.path" )
  }
  if ( class( download.folder ) != "character" ){
    stop( "The argument 'download.folder' must be of class 'character'!" )
  }
  ## Ensure the download.folder end with a '/'
  if ( substring( download.folder,
                 length( charToRaw( download.folder) ),
                 length( charToRaw( download.folder) ) ) != "/" ){
    download.folder <- paste0( download.folder, "/" )
  }

  ## If the folder does not exists yet, create it.
  if ( !dir.exists( download.folder ) ){
    dir.create( download.folder, recursive = TRUE )
  }

  ## The download.folder will be the entry point for the file tree
  ## corresponding to the one in the URL. The whole tree is not
  ## necessary to reproduce so the url.root part will be cut.
  url.root <- 'ftp://ftp-cdc.dwd.de/pub/CDC/'
  if ( !all( url.root ==
             substring( url.files, first = 1, last = 29 ) ) ){
    stop( paste( "The dwd2r package is not intended to work with URLs apart from those pointing to the FTP server of the DWD", url.root ) )
  }
  files.ftp <- substring( url.files, first = 30 )

  ## Get a list of all files in the download.folder.
  files.download.folder <- list.files( download.folder,
                                      include.dirs = TRUE,
                                      recursive = TRUE )

  ## Ignore those files already present in the download folder. But
  ## ignore the description files. They should be updated all the
  ## time. Else the list of stations including their coordinates would
  ## not match the individual files anymore and the importing routines
  ## would fail.
  files.new <-
    files.ftp[ !( files.ftp %in%
                  files.download.folder[
                      -c( grep(
                           'Beschreibung|BESCHREIBUNG|DESCRIPTION',
                           files.download.folder ) ) ] ) ]

  ## Discard those files of the subfolder not present at the FTP
  ## server anymore.
  subfolders <- unique( dirname( files.ftp ) )
  for ( ss in subfolders ){
    ## Ensure the existence of all subfolders
    if ( !dir.exists( paste0( download.folder, ss ) ) ){
      dir.create( paste0( download.folder, ss ), recursive = TRUE )
    }
    ## Lot recursive this time.
    files.subfolder <-
      list.files( paste0( download.folder, ss ), include.dirs = TRUE )
    ## Files, which are not present at the FTP server anymore
    files.outdated <- files.subfolder[
        !( files.subfolder %in%
           basename( files.ftp[ dirname( files.ftp ) == ss ] ) ) ]
    unlink( paste0( download.folder, ss, '/', files.outdated ) )
  }

  ## Download the new files.
  for ( ff in files.new ){
    utils::download.file( url = paste0( url.root, ff ),
                         method = "wget", quiet = quiet,
                         destfile = paste0( download.folder, ff ) )
    ## Wait in order to not be recognized as a bot by the server of
    ## the DWD
    Sys.sleep( .00001 )
  }
  
  ## Ensure the subfolders returned by the function end with a '/'
  for ( ss in ( 1 : length( subfolders ) ) ){
    if ( substring( subfolders[ ss ],
                   length( charToRaw( subfolders[ ss ] ) ),
                   length( charToRaw( subfolders[ ss ] ) ) ) != "/" ){
      subfolders[ ss ] <- paste0( subfolders[ ss ], "/" )
    }
  }
  
  return( subfolders )
}
## End of data-download.R
