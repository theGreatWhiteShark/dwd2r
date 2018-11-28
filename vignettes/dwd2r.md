The German weather service (DWD) provides a large set of
climatological data under the following [terms of
use](ftp://ftp-cdc.dwd.de/pub/CDC/Terms_of_use.txt). This is an
awesome starting point for data-driven software or research. But, as
usual, it is only the starting point. The observational data itself is
provided for all individual stations in separate files and split into
the most recent and a historical part. This is of course quite
convenient in case you want to update your data sets but it's up to
the user to combine both parts for each and every station. Once again,
the retrieval, cleaning, and conversion of the data will take up a
significant portion of our time. 

To ease or even overcome the burden of downloading all the different
station data, to import them into **R**, and to convert them into an
usable format, I wrote the software package
[dwd2r](https://github.com/theGreatWhiteShark/dwd2r).

## Installation

To install the package, you have to clone the repository using a
command line/terminal,

``` bash
git clone https://gitlab.com/theGreatWhiteShark/dwd2r
```

open a **R** shell in the newly created folder,

``` bash
cd dwd2r
R
```

and install the package using `devtools`.

``` R
devtools::install()
```

## Features and usage

#### Convenient interface to the FTP server

First of all, the user has to decide which data to download from the
FTP server. This can be done (inside **R**) in two different ways. Per
default a command line user interface will guide the user through the
hierarchy of the FTP server, prints the folder structure of the
current level into the **R** shell, and prompts for a selection of one of
those options. All data located in the selected terminal node will be
downloaded afterwards. 

``` R
dwd.download()
```

You also can circumvent the user interface and provide all the choices
you would select via the input argument `batch.choices`. The following
call will, e.g., download the aggregated, daily observation data
throughout German.

``` R
dwd.download( batch.choices = c( 1, 1, 5, 1 ) )
```

As a second way, you can use the listing of all folders on the FTP
server returned by the `dwd2r:::cat.dwd.ftp.url()` function and use
commands like `grep()` to select only the URLs you are interested
in. These can be provided to the `dwd.download()` function using the
`url` input argument. This code snippet will download the same data as
the former one.

``` R
dwd.download( 
  url = grep( "daily/kl", 
              dwd2r:::cat.dwd.ftp.url()$observations.germany,
              value=TRUE ) )
```


After downloading the selected data the `dwd.download()` function will
handle its conversion internally and stores the results in _.RData_
files.

#### What does the downloaded data looks like? 

For the choices made in the last section a large set of _.zip_
archives will be downloaded with each of them containing one table
holding the aggregated data for either the recent or the historical
part of the measurements performed at a single station. Aggregate
means we have about 17 columns with one corresponding to the time and
date of the measurement and the others to different climatological
quantities. To obtain, e.g., the daily accumulated precipitation data
of a single station, one first has to look up the ID of the station,
find both the historical and recent archive containing its data, and,
finally, extract the data from the right columns and join them
together.


#### What does the final data looks like?

The **dwd2r** package will split the aggregated data into different
lists named according to the corresponding quantity. E.g,
*dwd.temperature.2m.max* will contain the daily maximum temperature
measurements taken two meters above the ground. The lists contain
separate entries for each station and are named accordingly. The
elements, or the actual measurements, contain both the historical and
recent part of the series, are ordered properly, and can be provided
in two different formats. Per default
[xts](https://github.com/joshuaulrich/xts)-class objects will be
generated. But the user can also choose to have _data.frame_-class
objects by setting the `time.series.format` input argument of
`dwd.download()` to `"data.frame"`. These data frames will contain two
columns: _date_, of class _Date_ holding the individual dates of the
observations, and _value_, of class _numeric_ holding the actual
measurements.

Along with these lists an addition object containing the geographic
metadata of the stations will be generated as well. It will either be
a _SpatialPointsDataFrame_ provided by the
[sp](https://github.com/edzer/sp/) package (default) or a _data.frame_
when setting the `use.geospatial.position.format` input argument of
`dwd.download()` to `FALSE`. Each row of this object will contain the
longitude, latitude, altitude, and the corresponding name of a
single station.

For each climatological quantity a separate _.RData_ file will be
generated containing both the list holding the measured data as well
as the metadata object.


#### Customization

Per default all data will be stored in the *R/dwd_data* directory in
your home. If you want to change this behavior, you can hand over a
string specifying the path to your favored destination using the
`download.folder` argument of the `dwd.download()` function.

As an alternative you can also overwrite the global option **dwd2r**
uses to store its download path in. Just add the following line to
the _.Rprofile_ file in your home directory.

``` R
options( dwd2r.download.path = "PATH" )
```

All downloads will now be stored in the _PATH_ directory.


#### Loading the converted data

Since `dwd.download()` might generate quite a bunch of data files and
the user might already have a lot of them on her machine, the
**dwd2r** package ships with the helper function `source.data()`. It
lists all _.RData_ files including their size found in the folder the
global option `dwd2r.download.path` is pointing to.

#### Additional features

The `dwd.download()` function is also able to export all station data
into _.csv_ files. You might wonder why this would be a benefit
compared to the data set provided by the FTP server. For one, the
recent and historical part of the series are already combined. Also
the DWD uses a value of `-999` for missing data points, which will be
replaced by the more **R**-friendly `NA` values and the date format is
less obscure.

Another very important aspect of the `dwd.download()` function is that
it stored the downloaded _.zip_ archives (per default) and only
downloads those files during future calls who's time stamps have
changed. This way you can very easily update your data set without
downloading the whole content anew.


## Known bugs and issues

For now the internal functions handling the conversion of the data
have been only tested for the particular choice of data presented in
the code snippets earlier on. It might fail with a different set of
choices. But supporting another format is not a big deal and even if I
don't find myself in need to implemented it, do not hesitate to open
an issue to ask for it.

Also when you run the tests using `devtools::test()` you sometimes
receive errors like "URL could not be found on server" or
similar. This is not a real bug in the code but is caused by the
server of the DWD itself. It has some measures to detect bots and
blocking their IP address for some seconds. I already implemented some
counter measures and most of the time we can download huge sets of
data from the server without it noticing. But every now and then
**dwd2r** gets blocked and you have to wait some 10-20 seconds until
it does work fine again.
