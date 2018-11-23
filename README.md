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
