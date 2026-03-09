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
#' @param sr_location String. Path to location where SR files are saved.
#' @param algal_mask Logical. If TRUE, the algal mask version of the dataset (DSWE1a)
#' will be expected in `sr_location`. Otherwise DSWE1 is expected (i.e., FALSE).
#' @param sr_files Optional. A vector of filenames (five at most) with siteSR or
#' lakeSR files, like would be saved when running `download_lakeSR()` or `download_siteSR()`.
#' Should *not* include the directory provided in `sr_location`.
#' @param save Logical. Should the built SR dataset be saved locally? Defaults to TRUE.
#' @param save_location String. If save == TRUE, the path to the folder where the
#' output file should be saved. If a name ending in ".feather" is provided as part
#' of the path then this is the name that the file will be saved with. Otherwise,
#' a default name will be used by the function. It will always be a .feather file.
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
build_sr <- function(which_sr, sr_location, algal_mask, sr_files = NULL,
                     save = TRUE, save_location){
  # Confirm correct use of SR tag
  if(!(which_sr == "lakeSR" | which_sr == "siteSR")){
    stop("Input for which_sr argument is not valid. Must be 'lakeSR' or 'siteSR'.")
  }

  # Confirm correct use of algal_mask
  if(!is.logical(algal_mask)){
    stop("Input for algal_mask argument is not a logical value. Must be TRUE or FALSE.")
  }

  # Potential default SR path names
  # String for SR + mask combo
  input_string <- paste0(
    which_sr,
    "_",
    # T/F algal mask
    switch(
      as.character(algal_mask),
      "TRUE" = "DSWE1a",
      "FALSE" = "DSWE1"
    )
  )

  # Get default filename vectors to use
  default_filenames <- switch(
    input_string,
    "siteSR_DSWE1" = c("siteSR_Landsat4_DSWE1_2025-06-06.feather",
                       "siteSR_Landsat5_DSWE1_2025-06-06.feather",
                       "siteSR_Landsat7_DSWE1_2025-06-06.feather",
                       "siteSR_Landsat8_DSWE1_2025-06-06.feather",
                       "siteSR_Landsat9_DSWE1_2025-06-06.feather"),
    "siteSR_DSWE1a" = c("siteSR_Landsat4_DSWE1a_2025-06-06.feather",
                        "siteSR_Landsat5_DSWE1a_2025-06-06.feather",
                        "siteSR_Landsat7_DSWE1a_2025-06-06.feather",
                        "siteSR_Landsat8_DSWE1a_2025-06-06.feather",
                        "siteSR_Landsat9_DSWE1a_2025-06-06.feather"),
    "lakeSR_DSWE1" = c("lakeSR_Landsat4_DSWE1_2025-06-04.feather",
                       "lakeSR_Landsat5_DSWE1_2025-06-04.feather",
                       "lakeSR_Landsat7_DSWE1_2025-06-04.feather",
                       "lakeSR_Landsat8_DSWE1_2025-06-04.feather",
                       "lakeSR_Landsat9_DSWE1_2025-06-04.feather"),
    "lakeSR_DSWE1a" = c( "lakeSR_Landsat4_DSWE1a_2025-06-04.feather",
                         "lakeSR_Landsat5_DSWE1a_2025-06-04.feather",
                         "lakeSR_Landsat7_DSWE1a_2025-06-04.feather",
                         "lakeSR_Landsat8_DSWE1a_2025-06-04.feather",
                         "lakeSR_Landsat9_DSWE1a_2025-06-04.feather")
  )

  # Confirm file existence, standardize their names once confirmed
  # 1. If file path vector was provided
  if(!is.null(sr_files)){
    if(!all(file.exists(file.path(sr_location, sr_files)))) {
      stop("Some or all files in sr_files were not detected in sr_location.")
    } else{
      file_list <- file.path(sr_location, sr_files)
    }
    # 2. If file path vector wasn't provided (expect default names)
  } else if(is.null(sr_files)){
    if(!all(file.exists(file.path(sr_location, default_filenames)))){
      stop("Some or all expected SR files were not detected in sr_location.")
    } else{
      file_list <- file.path(sr_location, default_filenames)
    }
  }

  message("A series of large files will now be imported. It may take several minutes.")

  # Read files and stack
  sr_list <- purrr::map(.x = file_list,
                        .f = arrow::read_feather) %>%
    dplyr::bind_rows()

  # Now export if requested by user
  if(save){

  }

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
