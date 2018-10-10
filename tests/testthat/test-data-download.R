### test-data-download.R - listing of files at the FTP server and the
###   download of the actual content.
library( dwd2r )
context( "Checking the functions handling the access and download of the actual content" )

test_that( "the 'list.files.in.url' function takes the right input and produces the right output.", {
  expect_error( dwd2r:::list.files.in.url( list() ) )
  expect_error( dwd2r:::list.files.in.url( "ladida" ) )
  expect_equal( class(
      dwd2r:::list.files.in.url( 'ftp://ftp-cdc.dwd.de/pub/CDC/' ) ),
      "character" )
  expect_true( length(
      dwd2r:::list.files.in.url( 'ftp://ftp-cdc.dwd.de/pub/CDC/' ) )
      > 1 ) } )

test_that( "the 'download.content' function throws some errors.", {
  expect_error( dwd2r:::list.files.in.url( list() ) )
  expect_error( dwd2r:::list.files.in.url( "ladida" ) ) } )

## Temporary folder to download some data into.
tmp.dir.name <- "./download-test-funny-name-nobody-would-ever-choose"
dir.create( tmp.dir.name )
url.test <-
  "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/phenology/annual_reporters/vine/recent/"
test_that( "the setup of the download test still works", {
  expect_true( RCurl::url.exists( url.test ) ) } )
test_that( "there are still six files to download", {
  expect_equal(
      length( strsplit( RCurl::getURL( url.test ),
                       split = '\n' )[[ 1 ]] ), 6 ) } )
test_that( "'download.content' actually downloads content", {
  expect_equal( class(
      download.content( url.test, download.folder = tmp.dir.name,
                       quiet = TRUE ) ), "character" )
  expect_equal( length( list.files( tmp.dir.name,
                                   recursive = TRUE ) ), 6 )
  expect_equal( unique( dirname( list.files( tmp.dir.name,
                                            recursive = TRUE ) ) ),
               "observations_germany/phenology/annual_reporters/vine/recent" ) } )
test_that( "'download.content' does not add content during the second run", {
  expect_equal( class(
      download.content( url.test, quiet = TRUE,
                       download.folder =  tmp.dir.name ) ),
      "character" )
  expect_equal( length( list.files( tmp.dir.name,
                                   recursive = TRUE ) ), 6 )
  expect_equal( unique( dirname( list.files( tmp.dir.name,
                                            recursive = TRUE ) ) ),
               "observations_germany/phenology/annual_reporters/vine/recent" ) } )
test_that( "'download.content' has the right output", {
  expect_equal( download.content(
      url.test, download.folder = tmp.dir.name, quiet = TRUE ),
  "observations_germany/phenology/annual_reporters/vine/recent/" ) } )
              
## Create a new file in the folder and simulate an outdated file not
## present at the FTP server anymore
aux <- file.create( paste0(
    tmp.dir.name,
    "/observations_germany/phenology/annual_reporters/vine/recent/",
    'test' ) )
test_that( "'download.content' removes outdated files", {
  expect_equal( length( list.files( tmp.dir.name,
                                   recursive = TRUE ) ), 7 )
  expect_equal( class(
      download.content( url.test, quiet = TRUE,
                       download.folder =  tmp.dir.name ) ),
      "character" )
  expect_equal( length( list.files( tmp.dir.name,
                                   recursive = TRUE ) ), 6 ) } )
## Cleaning up
unlink( tmp.dir.name, TRUE )
## End of test-data-download.R
