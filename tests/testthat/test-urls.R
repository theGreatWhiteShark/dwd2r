### test-urls.R - Tests whether the individual URLs to the FTP server
###   are reachable.
library( dwd2r )
context( "Checking whether all the individual download URLs are still reachable." )

test_that( "the observation data of Germany is still reachable.", {
  expect_true( all(
      Reduce( c, lapply( cat.dwd.ftp.url()$observations.germany,
                        function( uu ){
                          RCurl::url.exists( uu )
                          Sys.sleep( .00001 ) } ) ) ) ) } )

test_that( "the observation data around the globe is still reachable.", {
  expect_true( all(
      Reduce( c, lapply( cat.dwd.ftp.url()$observations.global,
                        function( uu ){
                          RCurl::url.exists( uu )
                          Sys.sleep( .00001 ) } ) ) ) ) } )
test_that( "the grid data of Germany is still reachable.", {
  expect_true( all(
      Reduce( c, lapply( cat.dwd.ftp.url()$grid.germany,
                        function( uu ){
                          RCurl::url.exists( uu )
                          Sys.sleep( .00001 ) } ) ) ) ) } )
test_that( "the grid data of Europe is still reachable.", {
  expect_true( all(
      Reduce( c, lapply( cat.dwd.ftp.url()$grid.europe,
                        function( uu ){
                          RCurl::url.exists( uu )
                          Sys.sleep( .00001 ) } ) ) ) ) } )
test_that( "the derived data of Germany is still reachable.", {
  expect_true( all(
      Reduce( c, lapply( cat.dwd.ftp.url()$derived.germany,
                        function( uu ){
                          RCurl::url.exists( uu )
                          Sys.sleep( .00001 ) } ) ) ) ) } )
test_that( "the regional averages of Germany are still reachable.", {
  expect_true( all(
      Reduce( c, lapply( cat.dwd.ftp.url()$regional.averages.germany,
                        function( uu ){
                          RCurl::url.exists( uu )
                          Sys.sleep( .00001 ) } ) ) ) ) } )
test_that( "the cat.dwd.ftp.url function has the same output format",{
  expect_equal( class( cat.dwd.ftp.url() ), "list" )
  expect_equal( names( cat.dwd.ftp.url() ),
               c( "observations.germany", "observations.global",
                 "grid.germany", "grid.europe", "derived.germany",
                 "regional.averages.germany" ) )
  expect_equal( unique( Reduce( c, lapply( cat.dwd.ftp.url(),
                                          class ) ) ),
               "character" ) } )
Sys.sleep( 2 )
test_that( "the batch mode of get.dwd.ftp.url does work", {
  expect_equal( get.dwd.ftp.url( batch.choices =
                                   c( 1, 1, 5, 1 ) )$data,
               c(
  "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/historical/", 
  "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate/daily/kl/recent/" ) )
  expect_true( is.null( get.dwd.ftp.url( batch.choices =
                                           c( 1, 1, 5, 1 ) )$meta ) )
  expect_error( get.dwd.ftp.url( batch.choices = c( 1, 8, 5, 1 ) ) )
})
### End of test-urls.R
