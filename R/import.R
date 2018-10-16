##' @title Downloads data of the DWD
##' @description Downloads daily weather data from observation
##'   stations in Germany and extracts minimum and maximum temperature
##'   as well as precipitation data.
##'
##' @details 
##' @param files.list Named list of character vectors containing
##'   absolute paths pointing towards the .zip files downloaded from
##'   the DWD FTP server. The names correspond to \emph{recent},
##'   \emph{historical}, and \emph{diverse}.
##' @param files.description.list Named list of the corresponding
##'   description files. One for each element in \strong{files.list}.
##' @param csv.export If TRUE creates an additional folder containing
##'   .csv files with the individual station data. Using this the data
##'   can be used outside of R too. Default = FALSE.
##' @param download.folder This folder will be used to unpack and
##'   extract the .zip archives in. It does not have to be the same as
##'   the one containing the downloaded content.
##' @param quiet Whether or not to display the output generated when
##'   downloading the content. Default = FALSE.
##' 
##' @importFrom xts xts
##' 
##' @return invisible setwd()
##' 
##' @author Philipp Mueller
conversion.climate <- function( files.list, files.description.list,
                               csv.export = FALSE, download.folder,
                               quiet = FALSE ){
  ## Reading the content of the file containing the description of the
  ## station data.
  if ( !is.null( files.description.list$recent ) ){
    file.description.recent <-
      utils::read.table( files.description.list$recent,
                        header = FALSE, sep = "\t",
                        stringsAsFactors = FALSE,
                        encoding = "UTF-8", skip = 2 )
    ## Split it into a list to make efficient use of the grep command
    file.description.recent <-
      split( file.description.recent,
            seq( nrow( file.description.recent ) ) )
  } else {
    file.description.recent <- NULL
  }
  
  if ( !is.null( files.description.list$historical ) ){
    file.description.historical <-
      utils::read.table( files.description.list$historical,
                        header = FALSE, sep = "\t",
                        stringsAsFactors = FALSE,
                        encoding = "UTF-8", skip = 2 )
    ## Split it into a list to make efficient use of the grep command
    file.description.historical <- split(
        file.description.historical,
        seq( nrow( file.description.historical ) ) )
  } else {
    file.description.historical <- NULL
  }

  if ( !is.null( files.description.list$diverse ) ){
    warning(
        "The other folder structures apart from recent and historical are not supported yet." )
  }

  ## Merge the content of the two description files into one object
  ## and ensure proper encoding.
  file.description <- c( file.description.recent,
                        file.description.historical[
                            !( file.description.historical %in%
                              file.description.recent ) ] )
  file.description <- lapply( file.description, function( x )
    gsub( "\xfc", "\uFC", x ) )
  file.description <- lapply( file.description, function( x )
    gsub( "\xf6", "\uF6", x ) ) 
  file.description <- lapply( file.description, function( x )
    gsub( "\xe4", "\uE4", x ) )  
  file.description <- lapply( file.description, function( x )
    gsub( "\xdf", "\uDF", x ) )  
  file.description <- lapply( file.description, function( x )
    gsub( "\U3e63643c", "\uDC", x ) )  
  file.description <- lapply( file.description, function( x )
    gsub( "\U3e36643c", "\uD6", x ) )      
  
  ## Extract a vector of all unique station IDs seen in the .zip files
  list.station.ids <- as.list( unique( c(
      Reduce( c, lapply( files.list$recent[
                                        grep( ".zip", files.list$recent ) ],
                        function( x )
                          strsplit( x, "_" )[[ 1 ]][
                              length( strsplit( x, "_" )[[ 1 ]] ) - 1 ]
                        ) ),
      Reduce( c, lapply( files.list$historical[
                                        grep( ".zip", files.list$historical ) ],
                        function( x )
                          strsplit( x, "_" )[[ 1 ]][
                              length( strsplit( x, "_" )[[ 1 ]] ) - 3 ]
                        ) ) ) ) )

  ## Ensure the auxiliary folders for the extraction of the content do
  ## exist.
  download.folder.recent <- paste0( download.folder, "tmpRecent/" )
  if ( !dir.exists( download.folder.recent ) ){
    dir.create( download.folder.recent, recursive = TRUE )
  }
  download.folder.historical <- paste0( download.folder,
                                       "tmpHistorical/" )
  if ( !dir.exists( download.folder.historical ) ){
    dir.create( download.folder.historical, recursive = TRUE )
  }

  ## browser()
  extract.content.climate(
      station.id = list.station.ids[[ 1 ]],
      files.list = list( recent = files.list$recent,
                        historical = files.list$historical ),
      download.folder.recent = download.folder.recent,
      download.folder.historical = download.folder.recent )

  ## Clean up and delete the auxiliary folders used in extraction.
  unlink( download.folder.recent, recursive = TRUE )
  unlink( download.folder.historical, recursive = TRUE )

  ## This lists will contain all the final station data
  ## I will only extract the maximum, minimum temperature and the
  ## precipitation but it is easily extendable to all other columns
  stations.temp.max.xts <- stations.temp.min.xts <-
    stations.prec.xts <- list()
  ## For each data type a separate list containing the corresponding
  ## data of all stations will be generated
  ## This takes some time. But unfortunately it can't be parallelized
  ## easily since all thread read and write to the same folders. So no
  ## "progress bar" either
  for ( dd in 1 : length( data.type ) ){
    print( paste( "parsing", data.type[ dd ], "data..." ) )
    assign( paste0( "stations.", data.type[ dd ] ),
           lapply( list.station.ids, function( x )
             extract.content( x, data.columns[ dd ] ) ) )
  }
  ## assigning the stations names
  station.extracts <- parallel::mclapply( list.station.ids, function( x )
    extract.station.names( x, file.description ),
    mc.cores = parallel::detectCores( logical = FALSE ) )
  station.names <- Reduce( c, lapply( station.extracts,
                                     function( x ) x[[ 1 ]] ) )
  station.positions.aux <- Reduce( rbind, lapply( station.extracts,
                                                 function( x )
                                                   x[[ 2 ]] ) )
  station.positions <- data.frame(
      longitude = station.positions.aux[ , 1 ],
      latitude = station.positions.aux[ , 2 ],
      altitude = station.positions.aux[ , 3 ],
      name = station.names )
  ## Ordering the stations according to their names in alphabetical
  ## order
  station.positions <- station.positions[ order( station.names ), ]
  ## assigning the names of the stations.
  ## this new assignment take in the order of 25ms in total
  for ( ss in paste0( "stations.", data.type ) ){
    ## Get the variable of the string ss, do a operation on it and
    ## reassign it to the same name
    tmp <- get( ss )
    names( tmp ) <- station.names
    ## Ordering the stations according to their names in alphabetical
    ## order
    tmp <- tmp[ order( station.names ) ]
    assign( ss, tmp )
  }
  ## writing the data to dat files
  if ( csv.export ){
    ## creating a distinct folder for all the different data types
    for ( dd in data.type ){
      if ( !dir.exists( paste0( "data_dwd/", dd ) ) )
        dir.create( paste0( "data_dwd/", dd ) )
      data.temp <- get( paste0( "stations.", dd ) )
      for ( ll in 1 : length( data.temp ) )
        utils::write.table( data.frame(
                   date = index( data.temp[[ ll ]] ),
                   value = data.temp[[ ll ]], row.names = NULL ),
                   file = paste0( "data_dwd/", dd, "/",
                                 gsub( "/", "-",
                                      names( data.temp )[ ll ],
                                      fixed = TRUE ), ".csv" ),
                   sep = ",", row.names = FALSE )
    }
  }   
  ## delete all folders
  if ( !save.downloads ){
    unlink( "./recent/", recursive = TRUE )
    unlink( "./historical/", recursive = TRUE )
  }
  data.name <- data.type
  ## restore the input value of the selected data types
  if ( all( data.name == c( "temp.max", "temp.min", "prec" ) ) )
    data.name <- "default"
  ## save the extracted data
  save( list = c( paste0( "stations.", data.type ),
                 "station.positions" ),
       file = paste0( "./dwd_",
                     gsub( ".", "-", paste( data.name, 
                                           collapse = "_" ),
                          fixed = TRUE ),
                     ".RData" ) )
  setwd( old.dir )
  invisible()
}

##' @title Unzips files and extracts the content of DWD source files
##'   for a single station.
##' @description Reads in the content of both the recent and
##'   historical files corresponding to a single station, combines
##'   its data, and converts it into a format R can handle.
##' @details The station is specified by its \emph{station.id}. The
##'   extraction will take place in either
##'   \strong{download.folder.recent} or
##'   \strong{download.folder.historical}. Before extraction all its
##'   content will be removed.
##'
##' @param station.id String specifying a station.
##' @param files.list Named list of character vectors containing
##'   absolute paths pointing towards the .zip files downloaded from
##'   the DWD FTP server. The names correspond to \emph{recent} and
##'   \emph{historical}.
##' @param download.folder.recent Folder, which will contain the
##'   extracted recent archive.
##' @param download.folder.historical Folder, which will contain the
##'   extracted historical archive.
##'
##' @return A list of \strong{xts}-class objects where each element of
##'   the list corresponds to one data column in the data files.
##' @author Philipp Mueller
extract.content.climate <- function( station.id, files.list,
                                    download.folder.recent,
                                    download.folder.historical ){
  ## Compatibility
  if ( length( station.id ) > 1 ){
    stop( "Please provide only one station ID!" )
  }
  ## Delete the content of the auxiliary folders
  unlink( list.files( download.folder.recent ), recursive = TRUE )
  unlink( list.files( download.folder.historical ), recursive = TRUE )
  ## Obtain the index corresponding to the station ID.
  index.recent <- grep( station.id, files.list$recent )
  if ( length( index.recent ) > 1 ){
    stop( "More than one recent station matches the station ID!" )
  }
  index.historical <- grep( station.id, files.list$historical )
  if ( length( index.historical ) > 1 ){
    stop( "More than one historical station matches the station ID!" )
  }
  if ( length( index.recent ) == 0 &&
       length( index.historical ) == 0 ){
    message( paste( "No data at all could be found for station ID",
                   station.id ) )
    return( NULL )
  }
    
  ## If the zip file can not be extracted properly the unzip function
  ## will just raise a warning. It has to be converted to an error
  old.warning.level <- getOption( "warn" )
  options( warn = 2 )
  ## Extract the data
  if ( length( index.recent ) > 0 ){
    try.unzip <- try( utils::unzip( 
      files.list$recent[ index.recent ],
      exdir = download.folder.recent ), silent = TRUE )
    if ( class( try.unzip ) == "try-error" ){
      stop( paste( "Unable to extract the recent content of station",
                  station.id ) )
    }
  }
  if ( length( index.historical ) > 0 ){
    try.unzip <- try( utils::unzip(
      files.list$historical[ index.historical ],
      exdir = download.folder.historical ), silent = TRUE )
    if ( class( try.unzip ) == "try-error" ){
      stop( paste( "Unable to extract the historical content of station",
                  station.id ) )
    }
  }
  ## Revert the altering of the warning level
  options( warn = old.warning.level )

  ## Will contain lists of xts-class objects representing the time
  ## series extracted from the files.
  content.list <- list()
  ## Extract the content from all "produkt_klima_*" files.
  if ( length( index.recent ) > 0 ){
    content.files <- grep(
        "produkt", paste0( download.folder.recent,
                          list.files( download.folder.recent ) ),
        value = TRUE )
    if ( length( content.files ) == 0 ){
      warning( paste( "No recent produkt_* files found for station",
                     station.id ) )
    } else {
      ## Append the content of the individual files to the content
      ## list.
      content.list <-
        c( content.list,
          lapply( content.files, function( ff )
            import.file.content.climate( ff ) ) )
    }
  }
  if ( length( index.historical ) > 0 ){
    content.files <- grep(
        "produkt", paste0( download.folder.historical,
                          list.files( download.folder.historical ) ),
        value = TRUE )
    if ( length( content.files ) == 0 ){
      warning( paste( "No historical produkt_* files found for station",
                     station.id ) )
    } else {
      ## Append the content of the individual files to the content
      ## list.
      content.list <-
        c( content.list,
          lapply( content.files, function( ff )
            import.file.content.climate( ff ) ) )
    }
  }

  ## Check whether all lists are compatible and feature the same
  ## amount of climatological quantities
  if ( !all( Reduce( c, lapply( 1 : length( content.list ),
                               function( ll )
                                 names( content.list[[ 1 ]] ) ==
                                 names( content.list[[ ll ]] ) )
                    ) ) ){
    warning( paste( "extracted content does not match for station",
                   station.id ) )
  }
  ## Merge all time series concerning one climatological variable into
  ## one and provide them as a list of class xts elements.
  ## Start with the first list of results and add all data of the
  ## latter, which was not part of the former one.
  result.list <- content.list[[ 1 ]]
  if ( length( content.list ) > 1 ){
    for ( ll in 2 : length( content.list ) ){
      result.list <- lapply( 1 : length( result.list ), function( rr ){
        ## Add all time stamps, which are not present yet.
        if ( all( is.na( content.list[[ ll ]][[ rr ]] ) ) ){
          ## The c.xts function will complain if one of the objects
          ## solely consists of NAs.
          suppressWarnings({
            res <- c( result.list[[ rr ]],
                     content.list[[ ll ]][[ rr ]][
                         !index( content.list[[ ll ]][[ rr ]] ) %in%
                         index( result.list[[ rr ]] ) ] ) })
        } else {
            res <- c( result.list[[ rr ]],
                     content.list[[ ll ]][[ rr ]][
                         !index( content.list[[ ll ]][[ rr ]] ) %in%
                         index( result.list[[ rr ]] ) ] )
        }
        return( res ) } )
    }
  }
  ## The name got lost during the merging of the content.
  names( result.list ) <- names( content.list[[ 1 ]] )
  return( result.list )
}


extract.station.names <- function( station.id, file.description ){
  ## The station ID is placed in the first column and the date of
  ## the beginning of the observation period is placed in the second
  ## column. With just one line between the first and second column
  ## and the necessity of the second to start with either a 1 or a 2
  ## the station ID can be extracted uniquely.
  ## The conversion to numeric and back is necessary to delete zeros
  line.raw <- grep( paste0( station.id, " [1,2]" ),
                   file.description, value = TRUE )
  ## The extraction of the name fails, because the DWD has not yet
  ## added the station to the Description file holding of the
  ## stations meta data. This can happen (already happened to me).
  if ( length( line.raw ) == 0 ){
    ## Show a warning and recommend downloading the newest data.
    warning( "Some of the station data are not contained in the overall description file. Please make sure you have the most recent data!" )
    ## Worst case, just delete the corresponding file and download
    ## the new one.
    return( list( "none", c( 0, 0, 0 ) ) )
  }
  ## The name can consist of multiple words. In the previous column
  ## there is a digit and in the next the county consisting of one
  ## word (including a minus)
  line.full <- unlist( strsplit( line.raw, " " ) )
  line <- as.character( line.full[ line.full != "" ] )
  line.last.digit <- utils::tail( grep( "[[:digit:]]", line ), 1 )
  line.last.word <- utils::tail( grep( "[[:alpha:]]", line ), 1 )
  station.name <- line[ line.last.digit + 1 ]
  if ( line.last.word - 1 > line.last.digit + 1 ){
    ## the stations name consists of more than one word
    for ( ww in ( line.last.digit + 2 ) : ( line.last.word - 1 ) )
      station.name <- paste( station.name, line[ ww ] )
  }                             
  ## in the 9th, 8th, 7th entries the stations coordinates are
  ## residing
  return( list( station.name,
               c( as.numeric( line[ line.last.digit ] ),
                 as.numeric( line[ line.last.digit - 1 ] ),
                 as.numeric( line[ line.last.digit - 2 ] ) ) ) )
}

##' @title Imports the content of a single produkt file into R
##' @description It does the actual conversion of the format used by
##'   the DWD to the \pkg{xts} class. This function is intend to be
##'   used for aggregated stations data.
##'
##' @param file.path Path to a single produkt_* file contained in the
##'   zip archives of the DWD.
##'
##' @return A time series of class \pkg{xts}.
##' @author Philipp Mueller
import.file.content.climate <- function( file.path ){
  ## Sometimes there is a single delimiter symbol in the last line.
  ## This causes the read.table function to throw a warning and, if
  ## the file to read just consists of one line, to fail. Therefore,
  ## it has to be avoided by checking how many characters are present
  ## in the last line.
  file.content <- readLines( file.path )
  if ( nchar( file.content[ length( file.content ) ] ) < 10 ){
    ## only the last line with the potential delimiter will be
    ## omitted. Minus two, because we have to omit the header as well.
    file.data <-
      utils::read.table(
                 file.path, header = TRUE, sep = ";",
                 nrows = ( length( file.content ) - 2 ) )
  } else {
    file.data <- utils::read.table( file.path, header = TRUE,
                                   sep = ";" )
  }
  ## Converting the data into a list of xts-class objects. Each data
  ## column will constitute an element in the list. Since the last
  ## column only contains the "eor" (end of row) delimiters, it will
  ## be skipped.
  results.list <- lapply( seq( 3, ncol( file.data ) - 1 ),
                         function( rr ){
                           xts( file.data[ , rr ],
                               order.by = convert.date.integer(
                                   file.data[ , 2 ] ) ) } )
  ## Artifacts in the DWD data base are stored as -999. These will be
  ## converted to NA (not available).
  results.list <- lapply( results.list, function( ll ){
    ll[ ll == -999 ] <- NA
    return( ll ) } )

  ## Use the header of the content file to name the list containing
  ## the results.
  names( results.list ) <- names( file.data )[
      seq( 3, ncol( file.data ) - 1 ) ]
  return( results.list )
}

##' @title Load a data file into R
##' @description Searches the \emph{~/R/dwd_data/} directory or a
##'   specified folder for .RData files recursively and displays the
##'   user its findings for her to choose one of them.
##' @details In order to use the data with the \pkg{climex} package,
##'   it should be of class \pkg{xts} or of lists of class \pkg{xts}
##'   objects.
##'
##'   You can use the \pkg{dwd2r} package to download and use the
##'   daily station data provided by the German weather service.
##'
##' @param download.folder Specifies the folder in which the function
##'   will look for .RData files recursively. Per default the
##'   \emph{R/dwd_data/} directory in your home folder will be
##'   used. You can overwrite this behavior by setting \code{options(
##'   dwd2r.download.path = "PATH" )} in your \emph{.Rprofile} path in
##'   your home.
##' @param envir Environment the data will be attached to. If not
##'   specified, the data will be loaded to the environment the
##'   function is called from. Default = NULL.
##' @family import
##'  
##' @export
##' @return Returns invisible but attaches the chosen .RData file to
##'   the specified R environment.
##' @author Philipp Mueller
source.data <- function( download.folder = NULL, envir = NULL ){
  ## The folder to put all the temporary files of the dwd2r
  ## package in is set in the options(). To modify it,
  ## overwrite the options( dwd2r.download.path ) in the .Rprofile
  ## file in your home directory
  if ( is.null( download.folder ) ){
    download.folder <- getOption( "dwd2r.download.path" )
  }

  ## Extract all .RData objects contained in the download path.
  data.path <- list.files( download.folder, pattern = ".RData",
                          recursive = TRUE )
  ## Obtain the size of the file in MB
  data.size <- rep( NA, length( data.path ) )
  for ( dd in 1 : length( data.path ) ){
    data.size[ dd ] <- file.size(
        paste0( download.folder, data.path[ dd ] ) )/ 1024^2
  }

  ## Print the user a compilation of all found objects.
  cat( '\nImporting data into your R session.\n\n' )
  cat( paste0( '\tData files found the folder ', download.folder,
              ':\n\n' ) )
  cat( '   size:\tpath:\n' )
  for ( dd in 1 : length( data.path ) ){
    if ( data.size[ dd ] < 100 ){
      cat( paste0( dd, '. ', round( data.size[ dd ], digits = 2 ),
                  '\t\t', data.path[ dd ], '\n' ) )
    } else {
      cat( paste0( dd, '. ', round( data.size[ dd ], digits = 2 ),
                  '\t', data.path[ dd ], '\n' ) )
    }
  }
  cat( '\n\n' )
  cat(
      'Please select one file by entering the corresponding number.\n' )
  data.selection <- readline( 'Selection: ' )

  print( paste( "Loading file",
               data.path[ as.numeric( data.selection ) ], "..." ) )

  if ( is.null( envir ) ) {
    load( file = paste0( download.folder,
                        data.path[ as.numeric( data.selection ) ] ),
         envir = parent.frame() )
  } else {
    load( file = paste0( download.folder,
                        data.path[ as.numeric( data.selection ) ] ),
         envir = envir )
  } 
  invisible( )
}
## End of import.R
