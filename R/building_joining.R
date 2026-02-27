#' Title
#'
#' @returns
#' @export
#'
#' @examples
#'
build_sr <- function(){



}

#' Join AquaMatch WQP dataset to siteSR
#'
#' @description
#' A function that creates a join between a specified AquaMatch harmonized Water
#' Quality Portal (WQP) dataset and the siteSR data product.
#'
#' @details
#' Matches a single AquaMatch harmonized WQP product to the siteSR product. Options
#' for the AquaMatch component are [chlorophyll *a*](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=1756&revision=2)
#' ("chla"), [dissolved organic carbon](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1809.1)
#' ("doc"), [Secchi disk depth](https://portal.edirepository.org/nis/mapbrowse?scope=edi&identifier=2048&revision=2)
#' ("sdd"), or [total suspended solids](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.2048.2)
#' ("tss").
#'
#'
#' @param wqp_path Path to the file (.csv or .feather) storing the AquaMatch
#' parameter data to be joined. Should be data from a single parameter (e.g.,
#' chlorophyll *a*), as would be saved after using the `download_parameters()`
#' function.
#' @param siteSR_path Path to the file (.feather) storing the stacked version of
#' the siteSR dataset (either DSWE1 or DSWE1a). This is the equivalent of the
#' direct output of the `build_sr()` function when run with siteSR data.
#' @param time_window A string indicating the amount of time on either side of the
#' in-situ measurements that should be used to match to siteSR overpass times, for
#' example: "2 days", "72 hours". Defaults to "5 days".
#'
#' @returns
#' @export
#'
#' @examples
#'
match_siteSR_to_WQP <- function(wqp_path, siteSR_path, time_window = "5 days"){



}
