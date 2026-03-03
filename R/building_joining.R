#' Build and save siteSR or lakeSR products from downloaded files
#'
#' @details
#' Reads and stacks siteSR or lakeSR files into a single object, then optionally
#' exports them to a single local file. The user can provide a vector of filenames
#' to the `sr_files` argument, which will then be used as the input files. If this
#' argument is not used, then the value of `which_sr` will be used to infer the
#' filenames based on the default outputs of `download_siteSR()` or `download_lakeSR()`.
#'
#' If a file export is requested (i.e., `save` is TRUE), then the file is exported
#' with a standard name to a user-specified location. Filenames are `sitesSR_stack.feather`
#' for siteSR and `lakeSR_stack.feather` for lakeSR.
#'
#' @param which_sr String. Options are "siteSR" or "lakeSR", indicating which of
#' the two SR products should be built.
#' @param sr_location String. Path to location where SR files are being stored.
#' @param sr_files Optional. A vector of filenames (five at most) with siteSR or
#' lakeSR files, like would be saved when runing `download_lakeSR()` or `download_siteSR()`.
#' @param save Logical. Should the built SR dataset be saved locally? Defaults to TRUE.
#' @param save_location String. If save == TRUE, the path to the folder where the
#' output file should be saved.
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
build_sr <- function(which_sr, sr_location, sr_files = NULL, save = TRUE, save_location){

  # Confirm correct use of SR tag
  if(!(which_sr == "lakeSR" | which_sr == "siteSR")){
    stop("Input for which_sr argument is not valid. Must be 'lakeSR' or 'siteSR'.")
  }

  # Confirm file existence
  if(!is.null(sr_files)){
    if(!all(file.exists(file.path(sr_location, sr_files)))) {
      stop("Some or all files in sr_files were not detected in sr_location.")
    }
  }

  # Confirm file contents (i.e., col names)
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
#' \dontrun{
#'
#' }
match_siteSR_to_WQP <- function(wqp_path, siteSR_path, time_window = "5 days"){

  lubridate::duration(time_window)

}
