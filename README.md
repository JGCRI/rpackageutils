![R-CMD-check](https://github.com/JGCRI/rpackageutils/workflows/R-CMD-check/badge.svg)
![test-coverage](https://github.com/JGCRI/rpackageutils/workflows/test-coverage/badge.svg)
[![codecov](https://codecov.io/gh/JGCRI/rpackageutils/branch/master/graph/badge.svg)](https://codecov.io/gh/JGCRI/rpackageutils)
[![DOI](https://zenodo.org/badge/260550796.svg)](https://zenodo.org/badge/latestdoi/260550796)

# rpackageutils
Common utilities used  in R modeling software packages

## Installation

The easiest way to install `rpackageutils` is using `install_github` from the
`devtools` package.  
```R
devtools::install_github('JGCRI/rpackageutils')
```
This will get you the latest stable development version of the model.
If you wish to install a specific release, you can do so by specifying
the desired release as the `ref` argument to this function call.  
Current and past releases are listed in the
[release table](https://github.com/JGCRI/rpackageutils/releases).

## Functionality

Current features:
- `download_unpack_zip()`:  Download and unpack example zipped file with .zip extension to a user-specified location
- `download_file()`:  Download file and name and save it to a user-specified location
- `fetch_unpack_data()`:  Download and unpack example data supplement from Zenodo that matches the current installed distribution
- `remote_read()`:  Read in a file in table format from a remote source. Currently this function only works with .csv and .txt files.
