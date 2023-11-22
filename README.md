# public.ctn0094extra


## Introduction
These are helper functions for the CTN-0094 data package `public.ctn0094data` and supplemental engineered / derived data sets.

### Installing Dependencies
Because this is the "Extra" package which supplements the information in the primary `public.ctn0094data::` package, we must have the most recent version of this primary data package built first. This package is on [CRAN](https://CRAN.R-project.org) at <https://CRAN.R-project.org/package=public.ctn0094data>, so you can install this data package with:
``` r
install.packages("public.ctn0094data")
```

If you have access to the GitHub repository for the development version of this data package, use the [remotes](https://CRAN.R-project.org/package=remotes) package to install the data package from GitHub:
```r
remotes::install_github('https://github.com/CTN-0094/public.ctn0094data')
```

To build this package from "zipped" source file on your local machine, run the following code (make sure to set the correct version and the path for your machine):
```r
install.packages(
  '/Users/gabrielodom/Desktop/public.ctn0094data_0.9.0.tar.gz',
  repos = NULL,
  type = 'source'
)
```


### Installing this Package
Once you have the `public.ctn0094data` package downloaded and installed on your computer, you can install this package. For now, use the `remotes` package to install this package directly from GitHub via
```r
remotes::install_github('https://github.com/CTN-0094/public.ctn0094extra')
```

Once `public.ctn0094data` and `public.ctn0094extra` have been released on CRAN, you will be able to install this package with:
``` r
# Do not run yet
# install.packages("public.ctn0094extra")
```


## Workflow
In order to recreate the data sets in this package, first build this package (`public.ctn0094extra::`) and then run the following scripts in order, **rebuilding** the package and **restarting** your R session after each script:

1. `inst/scripts/create_inductDelay_20220308.R`
2. `inst/scripts/create_visitImputed_20220308.R`
3. `inst/scripts/create_weeklyOpioidPattern_20220308.R`
4. `inst/scripts/create_weeklyTLFBPattern_20220511.R`
5. `inst/scripts/create_raceEthnicity_20220816.R`
