# Features

- Convenient interface to the vast climatological [data
  base](ftp://ftp-cdc.dwd.de/pub/CDC/) of the German weather service
  (DWD)
- Command line interface to navigate through the FTP server's content
  tree
- Automated conversion of the downloaded time series into either
  _data.frame_ or _xts_-class objects
- All data corresponding to a single climatological observable
  will be collected in a list named according to the station name
- Meta-information of all stations will be collected and provided as
  either a _data.frame_ or a _SpatialPointsDataFrame_ (from the **sp**
  package) containing the geographical location and the altitude of each
  station
- All extracted data will be saved in **R**-compatible _.RData_ files
  and, if desired, exported to _.csv_ files as well
- Caches the downloaded files and only obtains new or altered ones
  when the download is started again
  
# Installation

You have to clone the repository using a command line/terminal,

``` bash
git clone https://gitlab.com/theGreatWhiteShark/dwd2r
```

open a **R** shell in the newly created folder,

``` bash
cd dwd2r
R
```

and install the package using the `devtools` package.

``` R
devtools::install()
```

# Usage

The main interface to the package is the `dwd.download` function. You
can either use it interactively by calling 

``` R
dwd.download()
```

or by supplying your choices in the selection as a function
argument. E.g., the following call will download and convert the
aggregated, daily observation data for all stations within Germany.

``` R
dwd.download( batch.choices = c( 1, 1, 5, 1 ) )
```

**NOTE:** For now the internal functions handling the conversion of
the data have been only tested for this particular choice of data. It
might fail with a different set of choices.

Once you downloaded some data, you can load them into **R** using
either the `load()` function directly or by

``` R
source.data()
```

This function will list all _.RData_ files in your download directory,
displays them and their size in your command line, and let's you
choose one of the files using a command line user interface.


# Customization

Per default all data downloaded via the **dwd2r** package will be
stored in the *R/dwd_data* directory in your home. If you want to
change this behavior, you can hand over a string specifying the
path to your favored destination using the _download.folder_ argument
of the `dwd.download()` and `source.data()` function. 

As an alternative you can also overwrite the global option **dwd2r**
uses to store its download path in. Just add the following lines to
the _.Rprofile_ file in your home directory.

``` R
options( dwd2r.download.path = "PATH" )
```

All downloads will now be stored in the _PATH_ directory.

---

### License (source code & package)

This package is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License, version 3, as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.  See the GNU
General Public License for more details.

A copy of the GNU General Public License, version 3, is available at
<http://www.r-project.org/Licenses/GPL-3>

### License (remote data)

The terms of usage of the data provided by the German weather service
are included in [German](res/Nutzungsbedingungen_German.txt) and in
[English](res/Terms_of_use.txt).
