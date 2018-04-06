### test-urls.R - Tests whether the individual URLs to the FTP server
###   are reachable.
library( dwd2r )
context( "Checking whether all the individual download URLs are still reachable." )

test_that( "the observation data of Germany is still reachable.", {
  expect_true( all(
      Reduce( c, lapply( cat.dwd.ftp.url()$observations.germany,
                        RCurl::url.exists ) ) ) ) } )
test_that( "the observation data around the globe is still reachable.", {
  expect_true( all(
      Reduce( c, lapply( cat.dwd.ftp.url()$observations.global,
                        RCurl::url.exists ) ) ) ) } )
test_that( "the grid data of Germany is still reachable.", {
  expect_true( all(
      Reduce( c, lapply( cat.dwd.ftp.url()$grid.germany,
                        RCurl::url.exists ) ) ) ) } )
test_that( "the grid data of Europe is still reachable.", {
  expect_true( all(
      Reduce( c, lapply( cat.dwd.ftp.url()$grid.europe,
                        RCurl::url.exists ) ) ) ) } )
test_that( "the derived data of Germany is still reachable.", {
  expect_true( all(
      Reduce( c, lapply( cat.dwd.ftp.url()$derived.germany,
                        RCurl::url.exists ) ) ) ) } )
test_that( "the regional averages of Germany are still reachable.", {
  expect_true( all(
      Reduce( c, lapply( cat.dwd.ftp.url()$regional.averages.germany,
                        RCurl::url.exists ) ) ) ) } )
test_that( "the cat.dwd.ftp.url function has the same output format",{
  expect_equal( class( cat.dwd.ftp.url() ), "list" )
  expect_equal( names( cat.dwd.ftp.url() ),
               c( "observations.germany", "observations.global",
                 "grid.germany", "grid.europe", "derived.germany",
                 "regional.averages.germany" ) )
  expect_equal( unique( Reduce( c, lapply( cat.dwd.ftp.url(),
                                          class ) ) ),
               "character" ) } ) 
### End of test-urls.R
