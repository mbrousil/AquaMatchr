# AquaMatchr

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![R-CMD-check](https://github.com/AquaSat/AquaMatchr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AquaSat/AquaMatchr/actions/workflows/R-CMD-check.yaml) [![Project Status: WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

<!-- badges: end -->

A package to assist with downloads, matchups, and analyses using [AquaMatch](https://aquasat.github.io/AquaMatch_documentation/) data products.
Provides methods for downloading and joining harmonized *in-situ* AquaMatch datasets with lakeSR, siteSR, and riverSR datasets.

As of July 2026 the following *in-situ* and surface reflectance (SR) AquaMatch datasets have been published:

**In situ**:  

+ [Chlorophyll *a*](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1756.2)
+ [Dissolved organic carbon](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1809.1)
+ [Secchi disk depth](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1856.1)
+ [Total suspended solids](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.2048.2)
+ [Colored dissolved organic matter](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.2380.1)

**SR**:  

+ [lakeSR](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.2114.1)
+ [siteSR](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.2254.1)

**Non-AquaMatch**:

+ [RiverSR](https://zenodo.org/records/4304567), product of [John Gardner's](https://github.com/johngardner87) lab


<br>

---

## Install from GitHub

```r
remotes::install_github(repo = "AquaSat/AquaMatchr")
```

<br>

If you would like to access the vignettes in the package we suggest installing via the following method, though it will take several minutes longer:
```r
remotes::install_github(repo = "AquaSat/AquaMatchr", build_vignettes = TRUE)
```

<br>

---

## Quick example

As a quick example, you can download the AquaMatch colored dissolved organic matter dataset like this:
``` r
library(AquaMatchr)

# Download one or more parameters in list format into your environment
cdom <- download_parameters(parameters = "cdom", version = "newest")
#> ℹ Colored dissolved organic matter recommended citation: Brousil, M.R., K.A. Ryan, R.M. Cory, M.F. Meyer, and M.R. Ross. 2026. AquaMatch Colored Dissolved Organic Matter from Water Quality Portal ~1995-2025 ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/c77dd74f42580c52f6bbb549778c827f. Accessed 2026-07-17.

# Pull the data from the list as a tibble/data.frame and view the first few rows
head(cdom[["cdom"]])
#> # A tibble: 6 × 33
#>   parameter OrganizationIdentifier MonitoringLocationId…¹ MonitoringLocationTy…²
#>   <chr>     <chr>                  <chr>                  <chr>                 
#> 1 Absorban… 11113300               11113300-00J-PRB       River/Stream          
#> 2 Absorban… 11113300               11113300-00J-PRB       River/Stream          
#> 3 Absorban… 11113300               11113300-00J-PRB       River/Stream          
#> 4 Absorban… 11113300               11113300-00J-PRB       River/Stream          
#> 5 Absorban… 11113300               11113300-00J-PRB       River/Stream          
#> 6 Absorban… 11113300               11113300-00J-PRB       River/Stream          
#> # ℹ abbreviated names: ¹​MonitoringLocationIdentifier,
#> #   ²​MonitoringLocationTypeName
#> # ℹ 29 more variables: ResolvedMonitoringLocationTypeName <chr>,
#> #   ActivityStartDate <date>, ActivityStartTime.Time <time>,
#> #   ActivityStartTime.TimeZoneCode <chr>, harmonized_tz <chr>,
#> #   harmonized_local_time <dttm>, harmonized_utc <dttm>,
#> #   ActivityStartDateTime <dttm>, harmonized_top_depth_value <dbl>, …
```
<sup>Created on 2026-07-17 with [reprex v2.1.1](https://reprex.tidyverse.org)</sup>

<br>

---

## Example vignette

The package contains a few vignettes. For a general overview of an example workflow in the package you can try the one below. Note: This requires you to have installed the package using `build_vignettes = TRUE` as shown above.
```r
vignette("example-workflow", package = "AquaMatchr")
```

<br>

--- 

## Citation

Please use the following citation when citing `AquaMatchr` in your work:

> Brousil M, Ross M (2026). _AquaMatchr: Download, Join, and Analyze AquaMatch Data Products_. R package version 0.0.0.9000.

**BibTeX:**
```bibtex
 @Manual{,
    title = {AquaMatchr: Download, Join, and Analyze AquaMatch Data Products},
    author = {Matthew Brousil and Matthew Ross},
    year = {2026},
    note = {R package version 0.0.0.9000},
  }
```

You can run `citation("AquaMatchr")` to get the most up-to-date citation when using the package.

<br>

---

## Help and contributions

We welcome any feedback you have while using the package! Please use a [reproducible example](https://reprex.tidyverse.org/) when submitting an Issue. 

If you would like to contribute, please refer to the [Contributing Guidelines](.github/CONTRIBUTING.md) and [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md).


<br>

---

## Additional documentation

Bookdown websites documenting the AquaMatch *in-situ* and SR data products are available at the following locations:

+ In situ: https://aquasat.github.io/AquaMatch_documentation/
+ Surface reflectance: https://aquasat.github.io/AquaMatch_lakeSR/index.html

