library( dwd2r )
context("Testing the import and conversion capabilities of the package" )

## Since some files will be stored on disk and cleaned up afterwards,
## we want to perform the following actions in a clean environment.
dir.name <- "conversion-folder-funny-name-nobody-would-choose"
dir.create( dir.name )
test_that( "conversion.climate writes out the converted data", {
  expect_true(
      dwd2r:::conversion.climate(
          files.list =
            list( recent = file.path(
                      system.file( "inst", package = "dwd2r" ), "res",
                      "produkt_potsdam_recent_03987_mock.zip" ),
                 historical = file.path(
                      system.file( "inst", package = "dwd2r" ), "res",
                   "produkt_03987_potsdam_historical_mock.zip" ) ),
          files.description.list =
            list( recent = file.path(
                      system.file( "inst", package = "dwd2r" ), "res",
                    "KL_Tageswerte_Beschreibung_mock.txt" ) ),
          download.folder = dir.name,
          csv.export = TRUE, quiet = TRUE,
          prefix.file.name = "xts",
          time.series.format = "xts",
          use.geospatial.position.format = TRUE ) )
  expect_true(
      dwd2r:::conversion.climate(
          files.list =
            list( recent = file.path(
                      system.file( "inst", package = "dwd2r" ), "res",
                      "produkt_potsdam_recent_03987_mock.zip" ),
                 historical =file.path(
                      system.file( "inst", package = "dwd2r" ), "res",
                     "produkt_03987_potsdam_historical_mock.zip" ) ),
          files.description.list =
            list( recent = file.path(
                      system.file( "inst", package = "dwd2r" ), "res",
                    "KL_Tageswerte_Beschreibung_mock.txt" ) ),
          download.folder = dir.name,
          csv.export = TRUE, quiet = TRUE,
          prefix.file.name = "df",
          time.series.format = "data.frame",
          use.geospatial.position.format = FALSE ) )
  expect_equal( list.files( dir.name ),
               c( "csv", "dwd_df_cloud-cover-mean.RData",
                 "dwd_df_precipitation-form.RData",
                 "dwd_df_precipitation-height.RData",
                 "dwd_df_pressure-mean.RData",
                 "dwd_df_quality-general.RData",
                 "dwd_df_relative-humidity-mean.RData",
                 "dwd_df_snow-depth.RData",
                 "dwd_df_sunshine-duration.RData",
                 "dwd_df_temperature-2m-max.RData",
                 "dwd_df_temperature-2m-min.RData",
                 "dwd_df_temperature-5cm-min.RData",
                 "dwd_df_temperature-mean.RData",
                 "dwd_df_vapour-pressure-mean.RData",
                 "dwd_df_wind-gust-max.RData",
                 "dwd_df_wind-gust-mean.RData",
                 "dwd_df_wind-gust-quality.RData",
                 "dwd_xts_cloud-cover-mean.RData",
                 "dwd_xts_precipitation-form.RData",
                 "dwd_xts_precipitation-height.RData",
                 "dwd_xts_pressure-mean.RData",
                 "dwd_xts_quality-general.RData",
                 "dwd_xts_relative-humidity-mean.RData",
                 "dwd_xts_snow-depth.RData",
                 "dwd_xts_sunshine-duration.RData",
                 "dwd_xts_temperature-2m-max.RData",
                 "dwd_xts_temperature-2m-min.RData",
                 "dwd_xts_temperature-5cm-min.RData",
                 "dwd_xts_temperature-mean.RData",
                 "dwd_xts_vapour-pressure-mean.RData",
                 "dwd_xts_wind-gust-max.RData",
                 "dwd_xts_wind-gust-mean.RData",
                 "dwd_xts_wind-gust-quality.RData" ) )
  expect_equal( list.files( file.path( dir.name, 'csv' ) ),
               c( "df", "xts" ) )
  expect_equal(
      list.files( file.path( dir.name, 'csv', 'xts' ) ),
      c( "cloud.cover.mean", "precipitation.form",
        "precipitation.height", "pressure.mean", "quality.general",
        "relative.humidity.mean", "snow.depth", "sunshine.duration",
        "temperature.2m.max", "temperature.2m.min",
        "temperature.5cm.min", "temperature.mean",
        "vapour.pressure.mean", "wind.gust.max", "wind.gust.mean",
        "wind.gust.quality" ) )
  expect_equal(
      list.files( file.path( dir.name, 'csv', 'df' ) ),
      c( "cloud.cover.mean", "precipitation.form",
        "precipitation.height", "pressure.mean", "quality.general",
        "relative.humidity.mean", "snow.depth", "sunshine.duration",
        "temperature.2m.max", "temperature.2m.min",
        "temperature.5cm.min", "temperature.mean",
        "vapour.pressure.mean", "wind.gust.max", "wind.gust.mean",
        "wind.gust.quality" ) )
  ## Check whether all these subfolders do contain actual .csv files.
  expect_equal(
      Reduce( sum, lapply( list.files(
                       file.path( dir.name, 'csv', 'xts' ),
                       full.names = TRUE ),
                          function( pp )
                            grep( "Potsdam.csv", list.files( pp ) ))),
      length( list.files( file.path( dir.name, 'csv', 'xts' ) ) ) )
  expect_equal(
      Reduce( sum, lapply( list.files(
                       file.path( dir.name, 'csv', 'df' ),
                       full.names = TRUE ),
                          function( pp )
                            grep( "Potsdam.csv", list.files( pp ) ))),
      length( list.files( file.path( dir.name, 'csv', 'df' ) ) ) )
})

## All loaded files will be attached to a clean environment to prevent
## collisions with existing objects.
test.environment <- new.env( parent = .GlobalEnv )
test_that( "the xts .RData written by conversion.climate is of the right format", {
  expect_equal( {
    load( file.path( dir.name, "dwd_xts_temperature-2m-max.RData" ),
         envir = test.environment )
    ls( envir = test.environment ) },
    c( "dwd.temperature.2m.max", "station.positions" ) )
  expect_equal(
      class( get( "dwd.temperature.2m.max",
                 envir = test.environment ) ),
      "list" )
  expect_equal(
      length( get( "dwd.temperature.2m.max",
                 envir = test.environment ) ), 1 )
  expect_true(
      xts::is.xts( get( "dwd.temperature.2m.max",
                       envir = test.environment )[[ 1 ]] ) )
  expect_equal(
      length( get( "dwd.temperature.2m.max",
                  envir = test.environment )[[ 1 ]] ), 45977 )
  expect_true(
      any( class( get( "station.positions",
                      envir = test.environment ) ) ==
           "SpatialPointsDataFrame" ) )
  expect_equal(
      as.numeric(
          sp::coordinates( get( "station.positions",
                               envir = test.environment ) ) ),
      c( 13.0622, 52.3813 ) )
  expect_equal(
      get( "station.positions", envir = test.environment )@data,
      data.frame( id = 3987L, name = "Potsdam", altitude = 81 ) )
})
test.environment <- new.env( parent = .GlobalEnv )
test_that( "the data.frame .RData written by conversion.climate is of the right format", {
  expect_equal( {
    load( file.path( dir.name, "dwd_df_temperature-2m-max.RData" ),
         envir = test.environment )
    ls( envir = test.environment ) },
    c( "dwd.temperature.2m.max", "station.positions" ) )
  expect_equal(
      class( get( "dwd.temperature.2m.max",
                 envir = test.environment ) ),
      "list" )
  expect_equal(
      length( get( "dwd.temperature.2m.max",
                 envir = test.environment ) ), 1 )
  expect_equal(
      class( get( "dwd.temperature.2m.max",
                 envir = test.environment )[[ 1 ]] ),
      "data.frame" )
  expect_equal(
      dim( get( "dwd.temperature.2m.max",
                  envir = test.environment )[[ 1 ]] ), c( 45977, 2 ) )
  expect_true(
      class( get( "station.positions",
                 envir = test.environment ) ) == "data.frame" )
  expect_equal(
      get( "station.positions", envir = test.environment ),
      data.frame( id = 3987L, name = "Potsdam", longitude = 13.0622,
                 latitude = 52.3813, altitude = 81 ) )
})
test_that( "the .csv written by conversion.climate is of the right format", {
  expect_equal( {
    data.xts <- read.csv2(
        file.path( dir.name, "csv", "xts", "temperature.2m.max",
                  "Potsdam.csv" ), header = TRUE, sep=',' )
    class( data.xts ) }, "data.frame" )
  expect_equal( dim( data.xts ), c( 45977, 2 ) )
  expect_equal( {
    data.df <- read.csv2(
        file.path( dir.name, "csv", "df", "temperature.2m.max",
                  "Potsdam.csv" ), header = TRUE, sep=',' )
    class( data.df ) }, "data.frame" )
  expect_equal( dim( data.df ), c( 45977, 2 ) )
  expect_true( all( data.xts == data.df ) )
})
unlink( dir.name, recursive = TRUE )
