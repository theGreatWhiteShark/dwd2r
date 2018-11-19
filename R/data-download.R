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
##' @param debug If TRUE is enables verbose messages of the individual
##'   downloads. Default = FALSE.
##'
##' @export
##'
##' @return Returns a character vector containing all directories data
##'   was written to terminated by a '/'.
##' @author Philipp Mueller
download.content <- function( url, download.folder = NULL,
                             quiet = FALSE, debug = FALSE ){
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

  ## Maximum number of tries per file.
  number.of.download.tries <- 10
  ## Download the new files.
  if ( !quiet ){
    cat( paste( "\nStart downloading", length( files.new ),
               "files...\n" ) )
  }
  for ( ff.idx in 1 : length( files.new ) ){
    if ( !quiet && ( ff.idx %% 5 ) == 0 ){
      cat( paste( "\r   Downloading file", ff.idx, "of",
                 length( files.new ) ) )
    }
    ff <- files.new[ ff.idx ]
    wget.try <- try(
        utils::download.file(
                   url = paste0( url.root, ff ),
                   method = "wget", quiet = !debug,
                   destfile = paste0( download.folder, ff ) ),
        silent = !debug )
    ## In case the download did fail because the connection was
    ## refused or wget failed to authenticate, just try again. Usually
    ## these problems are temporary.
    if ( class( wget.try ) == "try-error" ){
      current.try <- 1
      while ( current.try < number.of.download.tries &&
             class( wget.try ) == "try-error" ){
               wget.try <- try(
                   utils::download.file(
                              url = paste0( url.root, ff ),
                              method = "wget", quiet = !debug,
                              destfile = paste0( download.folder,
                                                ff ),
                              extra = c( "--tries 5" ) ),
                   silent = !debug )
               current.try <- current.try + 1
             }
      ## Display a warning in case one file couldn't be downloaded at
      ## all.
      if ( class( wget.try ) == "try-error" ){
        warning( paste( "The file", paste0( url.root, ff ),
                       "could not be downloaded!" ) )
      }
    }
    ## Wait in order to not be recognized as a bot by the server of
    ## the DWD
    Sys.sleep( .001 )
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
##' @title Downloads data of the DWD
##' @description Downloads daily weather data from observation
##'   stations in Germany and extracts minimum and maximum temperature
##'   as well as precipitation data.
##'
##' @details The download will be done using 'wget'. Per default the
##'   dwd2r.download.path variable from the \code{getOption(
##'   "dwd2r.download.path" )} will be used to set the download
##'   path. Since this function will check the files already present
##'   it's strongly recommended to use the save.downloads
##'   options. Whenever this function is invoked again only updated
##'   files will be downloaded which saves a lot of traffic and
##'   time. The csv.export option can be used to export the time
##'   series into a data type file making it available outside of R
##'   too. In addition, the geographic positions of the individual
##'   stations will be extracted and saved as well. They are needed
##'   for the leaflet module of the climex shiny app. CAUTION: since
##'   this procedure takes a while its run in parallel on all cores of
##'   your machine!
##' @param save.downloads If TRUE the downloaded .zip files are stored
##'   in download.folder/downloads_dwd. Else they will be deleted
##'   after the extracting. Default = TRUE.
##' @param url If the user wants to avoid the guided selection.
##' @param csv.export If TRUE creates an additional folder containing
##'   .csv files with the individual station data. Using this the data
##'   can be used outside of R too. Default = FALSE.
##' @param download.folder Specifies the data will be stored and
##'   downloaded too. It is advised to store it in the path stored in
##'   the \code{options( "dwd2r.download.path" )}, which is also used
##'   for importing the saved data. You can overwrite its default
##'   value of "~/R/dwd_data/" by adding \code{options(
##'   dwd2r.download.path = "PATH" )} to your .Rprofile path in your
##'   home.
##' @param batch.choices Numerical vector containing the numbers,
##'   which corresponds to the choices in the interactive mode. If
##'   NULL, the choices will be done interactively. Default = NULL.
##' @param quiet Whether or not to display the output generated when
##'   downloading the content. Default = FALSE.
##' @param debug If TRUE is enables verbose messages of the individual
##'   downloads performed within \code{download.content}. Default =
##'   FALSE.
##' 
##' @export
##'
##' @examples
##' ## Downloading the aggregated collection of the daily measured
##' ## climatic data of the DWD into a local folder, create .csv files
##' ## from all the time series, and store the files (to be able to do
##' ## a diff at a latter point in time).
##' download.data.dwd( save.downloads = TRUE, csv.export = TRUE,
##'                    download.folder = "./local/folder",
##'                    batch.choices  = c( 1, 1, 5, 1 ) )
##' 
##' @importFrom xts xts
##' 
##' @return invisible( TRUE )
##' 
##' @author Philipp Mueller 
download.data.dwd <- function( save.downloads = TRUE,
                              csv.export = FALSE,
                              url = NULL,
                              download.folder = NULL,
                              batch.choices = NULL,
                              quiet = FALSE, debug = FALSE ){
  ## The folder to put all the temporary files of the dwd2r package in
  ## is set in the options(). To modify it, overwrite the options(
  ## dwd2r.download.path ) in the .Rprofile file in your home
  ## directory
  if ( is.null( download.folder ) ){
    download.folder <- getOption( "dwd2r.download.path" )
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
  
  ## Retrieve the download URLs, extract the URLs to the individual
  ## files, and download them.
  if ( is.null( url ) ){
    url.full.content <- get.dwd.ftp.url( batch.choices = batch.choices )
  }
  if ( !is.null( url.full.content$meta ) ){
    warning(
        "Data sources containing meta information are not supported yet!" )
  }
  ## URLs pointing to the actual data.
  url <- url.full.content$data
  ## Download the files. The subfolders variable will contain the
  ## subfolders of download.folder, which contain the downloaded
  ## data.
  cat( 'Download data from\n' )
  lapply( url, function( uu ) cat( paste0( '- ',uu, '\n' ) ) )
  subfolders <- download.content(
      url = url, download.folder = download.folder, quiet = quiet )
  
  ## Get a list of all files, which were part of the download.
  ## In the classical, aggregated way of providing the data the DWD
  ## splits the time series and provides all data more recent than
  ## approximately one year in the *recent* folder and everything else
  ## in the *historical* folder. These ones are treated separately.
  if ( length( grep( "recent", subfolders ) ) != 0 ){
    indices.recent <- grep( "recent", subfolders )
    files.recent <- list.files( paste0(
        download.folder, subfolders[ indices.recent ] ) )
  } else {
    files.recent <- indices.recent <- NULL
  }
  if ( length( grep( "historical", subfolders ) ) != 0 ){
    indices.historical <- grep( "historical", subfolders )
    files.historical <- list.files( paste0(
        download.folder, subfolders[ indices.historical ] ) )
  } else {
    files.historical <- indices.historical <- NULL
  }
  ## Get all other files
  if ( length( subfolders ) > ( length( indices.recent ) +
                                length( indices.historical ) ) ){
    ## The recent and historical ones do not cover all choices
    message( "not just recent and historical" )
    indices.diverse <-
      seq( 1 : length( subfolders ) )[
          -which( seq( 1 : length( subfolders ) ) %in%
                 c( indices.recent, indices.historical ) ) ]
    files.diverse <- list.files( paste0(
        download.folder, subfolders[ -c( indices.recent,
                                        indices.historical ) ] ) )
  } else {
    indices.diverse <- NULL
    files.diverse <- NULL
  }

  ## Always download the latest description file. Else the algorithm
  ## would fail importing recently added stations.
  if ( !is.null( files.recent ) ){
    file.description.recent <-
      grep( "Beschreibung", files.recent, value = TRUE )
    if ( length( file.description.recent ) > 1 ){
      warning(
          "More than one recent 'Beschreibung' file found. Algorithm will misbehave!" )
    }
    if ( length( indices.recent ) > 1 ){
      warning(
          "More than one folder containing the recent measurements. The algorithm will not handle this properly!" )
    }
    utils::download.file(
               url = paste0( url[ indices.recent ],
                            file.description.recent ),
               destfile = paste0( download.folder,
                                 subfolders[ indices.recent ],
                                 file.description.recent ),
               method = "wget", quiet = !debug )
  } else {
    file.description.recent <- NULL
  }
  if ( !is.null( files.historical ) ){
    file.description.historical <-
      grep( "Beschreibung", files.historical, value = TRUE )
    if ( length( file.description.historical ) > 1 ){
      warning(
          "More than one recent 'Beschreibung' file found. Algorithm will misbehave!" )
    }
    if ( length( indices.historical ) > 1 ){
      warning(
          "More than one folder containing the recent measurements. The algorithm will not handle this properly!" )
    }
    utils::download.file(
               url = paste0( url[ indices.historical ],
                            file.description.historical ),
               destfile = paste0( download.folder,
                                 subfolders[ indices.historical ],
                                 file.description.historical ),
               method = "wget", quiet = !debug )
  } else {
    file.description.historical <- NULL
  }
  if ( !is.null( files.diverse ) ){
    file.description.diverse <-
      grep( "Beschreibung", files.diverse, value = TRUE )
    if ( length( file.description.diverse ) > 1 ){
      warning(
          "More than one diverse 'Beschreibung' file found. Algorithm will misbehave!" )
    }
    if ( length( indices.recent ) > 1 ){
      warning(
          "More than one folder containing the diverse measurements. The algorithm will not handle this properly!" )
    }
    utils::download.file(
               url = paste0( url[ indices.diverse ],
                            file.description.diverse ),
               destfile = paste0( download.folder,
                                 subfolders[ indices.diverse ],
                                 file.description.diverse ),
               method = "wget", quiet = quiet )
  } else {
    file.description.diverse <- NULL
  }

  ## Use some magic to derive the prefix for the file names the data
  ## will be saved in. Tested and devised for the aggregated daily
  ## climate data in Germany (batch.choices=c(1,1,5,1))
  url.split <- strsplit( url[ 1 ], '/' )[[ 1 ]]
  prefix.file.name <- paste( url.split[ which( url.split == "CDC" ) +
                                        c( 1, 2, 3 ) ],
                            collapse = "_" )

  conversion.climate(
      files.list = list(
          recent = 
            if ( any( is.null( files.recent ) ) ){
              NULL
              } else {
                   paste0( download.folder,
                          subfolders[ indices.recent ],
                          files.recent ) },
          historical = 
            if ( any( is.null( files.historical ) ) ){
              NULL
              } else {
                   paste0( download.folder,
                          subfolders[ indices.historical ],
                          files.historical ) },
          diverse = 
            if ( any( is.null( files.diverse ) ) ){
              NULL
              } else {
                   paste0( download.folder,
                          subfolders[ indices.diverse ],
                          files.diverse ) } ),
      files.description.list = list(
          recent =
            if ( any( is.null( files.recent ) ) ){
              NULL
              } else {
                   paste0( download.folder,
                          subfolders[ indices.recent ],
                          file.description.recent ) },
          historical =
            if ( any( is.null( files.historical ) ) ){
              NULL
              } else {
                   paste0( download.folder,
                          subfolders[ indices.historical ],
                          file.description.historical ) },
          diverse = 
            if ( any( is.null( files.diverse ) ) ){
              NULL
              } else {
                paste0( download.folder,
                       subfolders[ indices.diverse ],
                       file.description.diverse ) } ),
      csv.export = csv.export,
      prefix.file.name = prefix.file.name,
      download.folder = download.folder, quiet = quiet )
                        
  ## If required, delete all files downloaded during this session.
  if ( !save.downloads ){
    lapply( paste0( download.folder, subfolders ), function( ss )
      unlink( ss, recursive = TRUE ) )
  }
  invisible( TRUE )
}
## End of data-download.R
