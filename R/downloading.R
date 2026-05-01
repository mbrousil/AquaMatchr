
#' Download one or more AquaMatch harmonized data products from EDI
#'
#' @description
#' A function to facilitate downloading AquaMatch harmonized data products from
#' the Environmental Data Initiative (EDI). It accepts one or more shortened
#' parameter names, downloads the corresponding dataset(s) from EDI, and
#' returns them to the user as list items.
#'
#' @note
#' Returned datasets will be large (hundreds of MB in size), so users with limited
#' computer memory may wish to run a separate function call for each dataset
#' they want to download. These datasets will also be loaded into memory, meaning
#' that they are not currently saved permanently to the user's computer. We
#' suggest using `readr::write_csv()` or `feather::write_feather()` to save
#' permanent local copies.
#'
#' @param parameters A character vector containing one or more strings of AquaMatch
#' parameters to download (written as abbreviations). Options are currently
#' "chla" for chlorophyll *a*, "doc" for dissolved organic carbon, "sdd" for
#' Secchi disk depth, and "tss" for total suspended solids.
#' @param version Either "newest" or an integer corresponding to the data package
#' version to use. Note that in its current form, this function uses a single
#' value for every parameter requested. So, if `c("chla", "doc", "sdd")` is provided
#' to `parameters` and a `2` is provided to `version`, the second version of each
#' dataset will be requested, whether or not it actually exists.
#'
#' @return A named list where each item is a tibble containing a dataset.
#' @export
#'
#' @importFrom cli cli_alert_info cli_abort
#' @importFrom purrr map walk
#' @importFrom EDIutils read_data_package_citation read_data_entity_names read_data_entity list_data_package_revisions
#' @importFrom readr read_csv write_csv
#'
#' @examples
#' # Downloads the most recent version of the Secchi disk depth dataset
#' sdd_test <- download_parameters(parameters = "sdd")
#' # This is a list with a single item
#' length(sdd_test)
#' # Extract data frame from the list:
#' sdd <- sdd_test$sdd
download_parameters <- function(parameters, version = "newest"){

  # Metadata on each parameter's EDI identifier and the entity name of its
  # harmonized data product
  param_metadata <- dplyr::tribble(
    ~param, ~identifier, ~entity_name,
    "chla",  1756,        "chla_harmonized_final",
    "doc",   1809,        "doc_harmonized_final",
    "sdd",   1856,        "sdd_harmonized_final",
    "tss",   2048,        "tss_harmonized_final"
  )

  # Make sure parameters contains intended options
  if(!all(parameters %in% unique(param_metadata$param))){
    cli::cli_abort(
      paste0(
        "The provided input for the parameters argument does not match the ",
        "available options. Please check case and spelling.")
    )
  }

  # Keep what we need
  param_selection <- param_metadata %>%
    dplyr::filter(param %in% parameters)

  # For each param, read, message citation, and save in list
  split(param_selection, f = param_selection$param) %>%
    purrr::map(.x = .,
               .f = ~{
                 # EDI package ID
                 param_id <- construct_id(identifier = .x$identifier, version = version)

                 # Suggest citation
                 param_name <- switch(.x$param,
                                      "chla" = "Chlorophyll",
                                      "doc" = "Dissolved organic carbon",
                                      "sdd" = "Secchi disk depth",
                                      "tss" = "Total suspended solids")

                 param_citation <- EDIutils::read_data_package_citation(packageId = param_id)

                 cli::cli_alert_info("{.strong {param_name}} recommended citation: {.emph {param_citation}}")

                 # EDI entity ID (specific file to download)
                 entity_id <- EDIutils::read_data_entity_names(packageId = param_id) %>%
                   dplyr::filter(entityName == .x$entity_name) %>%
                   dplyr::pull(entityId)

                 # Read in data as raw bytes
                 raw_bytes <- EDIutils::read_data_entity(packageId = param_id,
                                                         entityId = entity_id)
                 # Parse
                 readr::read_csv(raw_bytes, show_col_types = FALSE)
               })
}


#' Download riverSR dataset from Zenodo
#'
#' @description
#' A function to facilitate downloading of the [riverSR](https://doi.org/10.5281/zenodo.4304567) data product from Zenodo.
#' It is mostly a wrapper around `zen4R::download_zenodo()`.
#'
#' @note
#' The downloaded file will be large (>13 GB in size), so users will need to
#' make sure that they have appropriate available storage for the file.
#'
#' @param save_location A string containing the path to the folder where the dataset
#'  should be saved.
#' @param timeout_length The number of seconds to allow for the download. Defaults
#' to 4000 based on tests with the riverSR dataset, but can be adjusted as needed.
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_abort
#' @return A character string containing the local file path to the downloaded RiverSR dataset. Returned invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' download_riverSR(save_location = "~/Downloads/")
#' }
download_riverSR <- function(save_location, timeout_length = 4000){

  # Warning to user
  cli::cli_alert_info(
    "The size of this file is large (>13GB) so the download will take some time."
  )

  zen4R::download_zenodo(path = save_location,
                         doi = "10.5281/zenodo.4304567",
                         files = "riverSR_usa_v1.1.feather",
                         timeout = timeout_length)

  # Confirm file saved and report back
  out_file <- file.path(save_location, "riverSR_usa_v1.1.feather")

  if(file.exists(out_file)){
    cli::cli_alert_success("Download complete.")
    cli::cli_alert_info(
      paste0(
        "RiverSR recommended citation: John Gardner, Xiao Yang, Simon Topp, Matthew Ross,",
        " Tamlin Pavelsky, & Elizabeth Altenau. (2020). River Surface Reflectance",
        " Database (RiverSR) (v1.1.0) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.4304567.",
        " Accessed {Sys.Date()}."
      )
    )
    return(invisible(out_file))
  } else {
    cli::cli_abort("Output file cannot be found.")
  }
}


#' Download siteSR dataset from EDI
#'
#' @description
#' A function to facilitate downloading of the siteSR data product from
#' the Environmental Data Initiative (EDI).
#'
#' @details
#' Downloads the following components of the [siteSR dataset](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.2254.1):
#' * lakeSR handoff coefficients (.csv)
#' * siteSR sites list (.csv)
#' * siteSR data from Landsat 4 (.feather)
#' * siteSR data from Landsat 5 (.feather)
#' * siteSR data from Landsat 7 (.feather)
#' * siteSR data from Landsat 8 (.feather)
#' * siteSR data from Landsat 9 (.feather)
#'
#' @note
#' The downloaded datasets will be large (several GB in size in total), so users
#' will need to make sure that they have appropriate available storage for the files.
#'
#' @param save_location A string containing the path to the folder where the dataset
#' should be saved.
#' @param algal_mask Logical. If TRUE, the algal mask version of the dataset (DSWE1a)
#' will be downloaded. Otherwise DSWE1 is used (i.e., FALSE).
#' @param version Either "newest" or an integer corresponding to the data package
#' version to use.
#' @param ask Logical. Should the user be asked before downloading and overwriting
#' siteSR files that already exist locally?
#'
#' @return A named character vector containing the local file paths for all
#' downloaded siteSR datasets. Returned invisibly.
#' @export
#'
#' @importFrom purrr map walk
#' @importFrom EDIutils read_data_package_citation read_data_entity_names read_data_entity list_data_package_revisions
#' @importFrom readr read_csv write_csv
#' @importFrom cli cli_alert_info cli_alert_success cli_abort
#'
#' @examples
#' \dontrun{
#' download_siteSR(save_location = "~/Downloads/", algal_mask = FALSE)
#' }

download_siteSR <- function(save_location, algal_mask = FALSE, version = "newest",
                            ask = TRUE){

  # siteSR EDI ID
  site_sr_id <- construct_id(identifier = 2254, version = version)

  # Filenames to be used for dswe1 and 1a, respectively:
  dswe1_names <- c(
    "siteSR_Landsat4_DSWE1_2025-06-06.feather",
    "siteSR_Landsat5_DSWE1_2025-06-06.feather",
    "siteSR_Landsat7_DSWE1_2025-06-06.feather",
    "siteSR_Landsat8_DSWE1_2025-06-06.feather",
    "siteSR_Landsat9_DSWE1_2025-06-06.feather"
  )

  dswe1a_names <- c(
    "siteSR_Landsat4_DSWE1a_2025-06-06.feather",
    "siteSR_Landsat5_DSWE1a_2025-06-06.feather",
    "siteSR_Landsat7_DSWE1a_2025-06-06.feather",
    "siteSR_Landsat8_DSWE1a_2025-06-06.feather",
    "siteSR_Landsat9_DSWE1a_2025-06-06.feather"
  )

  # No algal mask (DWSE1)
  if(!algal_mask){

    if(ask == TRUE){
      # Check if any files with the standard names are already present in the save
      # location:
      if(any(file.exists(file.path(save_location, dswe1_names)))) {
        user_decision <- ask_user(algal_mask = FALSE, which_sr = "siteSR")

        # Act on input
        if (user_decision == "yes") {
          cli::cli_alert_info("Proceeding with download.")
        } else {
          cli::cli_abort("Cancelled by user.")
        }
      }
    }
    # Get EDI entity names
    dl_entities <- EDIutils::read_data_entity_names(packageId = site_sr_id) %>%
      dplyr::filter(grepl(pattern = "DSWE = 1\\)$", x = entityName))

    # With algal mask (DSWE1a)
  } else if(algal_mask){

    if(ask == TRUE){
      # Check if any files with the standard names are already present in the save
      # location:
      if(any(file.exists(file.path(save_location, dswe1a_names)))) {
        user_decision <- ask_user(algal_mask = TRUE, which_sr = "siteSR")

        # Act on input
        if (user_decision == "yes") {
          cli::cli_alert_info("Proceeding with download.")
        } else {
          cli::cli_abort("Cancelled by user.")
        }
      }
    }
    # Get EDI entity names
    dl_entities <- EDIutils::read_data_entity_names(packageId = site_sr_id) %>%
      dplyr::filter(grepl(pattern = "DSWE = 1a", x = entityName))
  }

  cli::cli_alert_info("This is a series of large downloads. It will take several minutes.")

  # For each param, read, message citation, and save in list
  dl_list <- split(dl_entities, f = dl_entities$entityName) %>%
    purrr::map_chr(.x = .,
                   .f = ~{
                     out_name <- switch(
                       .x$entityName,
                       "siteSR data from Landsat 4, DSWE filter for confident water (DSWE = 1)" = "siteSR_Landsat4_DSWE1_2025-06-06.feather",
                       "siteSR data from Landsat 4, DSWE filter for confident water and algal mask (DSWE = 1a)" = "siteSR_Landsat4_DSWE1a_2025-06-06.feather",
                       "siteSR data from Landsat 5, DSWE filter for confident water (DSWE = 1)" = "siteSR_Landsat5_DSWE1_2025-06-06.feather",
                       "siteSR data from Landsat 5, DSWE filter for confident water and algal mask (DSWE = 1a)" = "siteSR_Landsat5_DSWE1a_2025-06-06.feather",
                       "siteSR data from Landsat 7, DSWE filter for confident water (DSWE = 1)" = "siteSR_Landsat7_DSWE1_2025-06-06.feather",
                       "siteSR data from Landsat 7, DSWE filter for confident water and algal mask (DSWE = 1a)" = "siteSR_Landsat7_DSWE1a_2025-06-06.feather",
                       "siteSR data from Landsat 8, DSWE filter for confident water (DSWE = 1)" = "siteSR_Landsat8_DSWE1_2025-06-06.feather",
                       "siteSR data from Landsat 8, DSWE filter for confident water and algal mask (DSWE = 1a)" = "siteSR_Landsat8_DSWE1a_2025-06-06.feather",
                       "siteSR data from Landsat 9, DSWE filter for confident water (DSWE = 1)" = "siteSR_Landsat9_DSWE1_2025-06-06.feather",
                       "siteSR data from Landsat 9, DSWE filter for confident water and algal mask (DSWE = 1a)" = "siteSR_Landsat9_DSWE1a_2025-06-06.feather"
                     )

                     # Read in data as raw bytes
                     raw_bytes <- EDIutils::read_data_entity(packageId = site_sr_id,
                                                             entityId = .x$entityId)
                     # Parse
                     temp_file <- arrow::read_feather(raw_bytes)

                     arrow::write_feather(
                       x = temp_file,
                       sink = file.path(save_location, out_name)
                     )

                     cli::cli_alert_success("Downloaded {.val {(.x$entityName)}} as {.file {out_name}}.")

                     # Return the path to the loop
                     return(file.path(save_location, out_name))
                   })

  # Clean and store filenames
  names(dl_list) <- basename(dl_list)

  # Download site list
  # Check if a site info file with the standard name is already present in the save
  # location:
  sites_filename <- "siteSR_collated_WQP_NWIS_sites_with_NHD_info_2025-06-04.csv"
  sites_out_name <- file.path(save_location, sites_filename)

  if(ask == TRUE){
    if(any(file.exists(sites_out_name))) {
      user_decision <- ask_user(algal_mask = FALSE,
                                which_sr = "generic",
                                file_message = "siteSR site information")
      # Act on input
      if (user_decision == "yes") {
        cli::cli_alert_info("Proceeding with download.")
      } else {
        cli::cli_abort("Cancelled by user.")
      }
    }
  }

  dl_sites <- EDIutils::read_data_entity_names(packageId = site_sr_id) %>%
    dplyr::filter(entityName == "siteSR sites list") %>%
    dplyr::pull(entityId)

  # Read in data as raw bytes
  raw_site_bytes <- EDIutils::read_data_entity(packageId = site_sr_id,
                                               entityId = dl_sites)
  # Parse
  suppressMessages({
    temp_site_file <- readr::read_csv(raw_site_bytes)
  })
  readr::write_csv(
    x = temp_site_file,
    file = sites_out_name
  )

  cli::cli_alert_success("Downloaded siteSR sites list as {.file {sites_filename}}.")

  # Download handoffs
  handoff_filename <- "lakeSR_collated_handoffs_GEEv2025-02-12_QAv2025-06-04.csv"
  handoff_out_name <- file.path(save_location, handoff_filename)

  if(ask == TRUE){
    # Check if a handoff coefficient file with the standard name is already present
    # in the save location:
    # (Note that handoffs are part of the lakeSR product, not siteSR)
    if(any(file.exists(handoff_out_name))) {
      user_decision <- ask_user(algal_mask = FALSE,
                                which_sr = "generic",
                                file_message = "handoff coefficient")

      # Act on input
      if (user_decision == "yes") {
        cli::cli_alert_info("Proceeding with download.")
      } else {
        cli::cli_abort("Cancelled by user.")
      }
    }
  }

  # lakeSR EDI ID, for handoffs only
  lake_sr_id <- construct_id(identifier = 2114, version = "newest")

  dl_handoff <- EDIutils::read_data_entity_names(packageId = lake_sr_id) %>%
    dplyr::filter(entityName == "lakeSR handoff coefficients") %>%
    dplyr::pull(entityId)

  # Read in data as raw bytes
  raw_site_bytes <- EDIutils::read_data_entity(packageId = lake_sr_id,
                                               entityId = dl_handoff)
  # Parse
  suppressMessages({
    temp_handoff_file <- readr::read_csv(raw_site_bytes)
  })
  readr::write_csv(
    x = temp_handoff_file,
    file = handoff_out_name
  )

  cli::cli_alert_success("Downloaded handoff coefficients as {.file {handoff_filename}}.")

  # Suggest citation
  cli::cli_alert_info(
    "siteSR recommended citation: {EDIutils::read_data_package_citation(packageId = site_sr_id)}"
  )

  # Combine all paths into a single named vector
  all_downloaded_paths <- c(
    dl_list,
    "siteSR_site_list" = sites_out_name,
    "handoffs" = handoff_out_name
  )

  return(invisible(all_downloaded_paths))
}


#' Download lakeSR dataset from EDI
#'
#' @description
#' A function to facilitate downloading of the lakeSR data product from
#' the Environmental Data Initiative (EDI).
#'
#' @details
#' Downloads the following components of the [lakeSR dataset](https://portal.edirepository.org/nis/mapbrowse?packageid=edi.2114.1):
#' * lakeSR handoff coefficients (.csv)
#' * lakeSR sites list (.csv)
#' * lakeSR data from Landsat 4 (.feather)
#' * lakeSR data from Landsat 5 (.feather)
#' * lakeSR data from Landsat 7 (.feather)
#' * lakeSR data from Landsat 8 (.feather)
#' * lakeSR data from Landsat 9 (.feather)
#'
#' @note
#' The downloaded datasets will be large (several GB in size in total), so users
#' will need to make sure that they have appropriate available storage for the files.
#'
#' @param save_location A string containing the path to the folder where the dataset
#' should be saved.
#' @param algal_mask Logical. If TRUE, the algal mask version of the dataset (DSWE1a)
#' will be downloaded. Otherwise DSWE1 is used (i.e., FALSE).
#' @param version Either "newest" or an integer corresponding to the data package
#' version to use.
#' @param ask Logical. Should the user be asked before downloading and overwriting
#' siteSR files that already exist locally?
#'
#' @return A named character vector containing the local file paths for all
#' downloaded datasets. Returned invisibly.
#' @export
#'
#' @importFrom purrr map walk
#' @importFrom EDIutils read_data_package_citation read_data_entity_names read_data_entity list_data_package_revisions
#' @importFrom readr read_csv write_csv
#' @importFrom cli cli_alert_info cli_alert_success cli_abort
#'
#' @examples
#' \dontrun{
#' download_lakeSR(save_location = "~/Downloads/", algal_mask = FALSE)
#' }
download_lakeSR <- function(save_location, algal_mask = FALSE, version = "newest",
                            ask = TRUE){

  # lakeSR EDI ID
  lake_sr_id <- construct_id(identifier = 2114, version = version)

  # Filenames to be used for dswe1 and 1a, respectively:
  dswe1_names <- c(
    "lakeSR_Landsat4_DSWE1_2025-06-04.feather",
    "lakeSR_Landsat5_DSWE1_2025-06-04.feather",
    "lakeSR_Landsat7_DSWE1_2025-06-04.feather",
    "lakeSR_Landsat8_DSWE1_2025-06-04.feather",
    "lakeSR_Landsat9_DSWE1_2025-06-04.feather"
  )

  dswe1a_names <- c(
    "lakeSR_Landsat4_DSWE1a_2025-06-04.feather",
    "lakeSR_Landsat5_DSWE1a_2025-06-04.feather",
    "lakeSR_Landsat7_DSWE1a_2025-06-04.feather",
    "lakeSR_Landsat8_DSWE1a_2025-06-04.feather",
    "lakeSR_Landsat9_DSWE1a_2025-06-04.feather"
  )

  # No algal mask (DWSE1)
  if(!algal_mask){

    if(ask == TRUE){
      # Check if any files with the standard names are already present in the save
      # location:
      if(any(file.exists(file.path(save_location, dswe1_names)))) {
        user_decision <- ask_user(algal_mask = FALSE, which_sr = "lakeSR")

        # Act on input
        if (user_decision == "yes") {
          cli::cli_alert_info("Proceeding with download.")
        } else {
          cli::cli_abort("Cancelled by user.")
        }
      }
    }
    # Get EDI entity names
    dl_entities <- EDIutils::read_data_entity_names(packageId = lake_sr_id) %>%
      dplyr::filter(grepl(pattern = "DSWE = 1\\)$", x = entityName))

    # With algal mask (DSWE1a)
  } else if(algal_mask){

    if(ask == TRUE){
      # Check if any files with the standard names are already present in the save
      # location:
      if(any(file.exists(file.path(save_location, dswe1a_names)))) {
        user_decision <- ask_user(algal_mask = TRUE, which_sr = "lakeSR")

        # Act on input
        if (user_decision == "yes") {
          cli::cli_alert_info("Proceeding with download.")
        } else {
          cli::cli_abort("Cancelled by user.")
        }
      }
    }
    # Get EDI entity names
    dl_entities <- EDIutils::read_data_entity_names(packageId = lake_sr_id) %>%
      dplyr::filter(grepl(pattern = "DSWE = 1a", x = entityName))
  }

  cli::cli_alert_info("This is a series of large downloads. It will take several minutes.")

  # For each param, read, message citation, and save in list
  dl_list <- split(dl_entities, f = dl_entities$entityName) %>%
    purrr::map_chr(.x = .,
                   .f = ~{
                     out_name <- switch(
                       .x$entityName,
                       "lakeSR data from Landsat 4, DSWE filter for confident water (DSWE = 1)" = "lakeSR_Landsat4_DSWE1_2025-06-04.feather",
                       "lakeSR data from Landsat 4, DSWE filter for confident water and algal mask (DSWE = 1a)" = "lakeSR_Landsat4_DSWE1a_2025-06-04.feather",
                       "lakeSR data from Landsat 5, DSWE filter for confident water (DSWE = 1)" = "lakeSR_Landsat5_DSWE1_2025-06-04.feather",
                       "lakeSR data from Landsat 5, DSWE filter for confident water and algal mask (DSWE = 1a)" = "lakeSR_Landsat5_DSWE1a_2025-06-04.feather",
                       "lakeSR data from Landsat 7, DSWE filter for confident water (DSWE = 1)" = "lakeSR_Landsat7_DSWE1_2025-06-04.feather",
                       "lakeSR data from Landsat 7, DSWE filter for confident water and algal mask (DSWE = 1a)" = "lakeSR_Landsat7_DSWE1a_2025-06-04.feather",
                       "lakeSR data from Landsat 8, DSWE filter for confident water (DSWE = 1)" = "lakeSR_Landsat8_DSWE1_2025-06-04.feather",
                       "lakeSR data from Landsat 8, DSWE filter for confident water and algal mask (DSWE = 1a)" = "lakeSR_Landsat8_DSWE1a_2025-06-04.feather",
                       "lakeSR data from Landsat 9, DSWE filter for confident water (DSWE = 1)" = "lakeSR_Landsat9_DSWE1_2025-06-04.feather",
                       "lakeSR data from Landsat 9, DSWE filter for confident water and algal mask (DSWE = 1a)" = "lakeSR_Landsat9_DSWE1a_2025-06-04.feather"
                     )
                     # Read in data as raw bytes
                     raw_bytes <- EDIutils::read_data_entity(packageId = lake_sr_id,
                                                             entityId = .x$entityId)
                     # Parse
                     temp_file <- arrow::read_feather(raw_bytes)

                     arrow::write_feather(
                       x = temp_file,
                       sink = file.path(save_location, out_name)
                     )

                     cli::cli_alert_success("Downloaded {.val {(.x$entityName)}} as {.file {out_name}}.")

                     # Return the path to the loop
                     return(file.path(save_location, out_name))
                   })

  # Clean and store filenames
  names(dl_list) <- basename(dl_list)

  # Download poi list
  lakes_filename <- "lakeSR_poi_with_flags_2025-02-12.csv"
  lakes_out_name <- file.path(save_location, lakes_filename)

  if(ask == TRUE){
    # Check if a lake info file with the standard name is already present in the
    # save location:
    if(any(file.exists(lakes_out_name))) {
      user_decision <- ask_user(algal_mask = FALSE,
                                which_sr = "generic",
                                file_message = "lakeSR lake information")

      # Act on input
      if (user_decision == "yes") {
        cli::cli_alert_info("Proceeding with download.")
      } else {
        cli::cli_abort("Cancelled by user.")
      }
    }
  }
  dl_lakes <- EDIutils::read_data_entity_names(packageId = lake_sr_id) %>%
    dplyr::filter(entityName == "lakeSR sites list") %>%
    dplyr::pull(entityId)

  # Read in data as raw bytes
  raw_lake_bytes <- EDIutils::read_data_entity(packageId = lake_sr_id,
                                               entityId = dl_lakes)
  # Parse
  suppressMessages({
    temp_lake_file <- readr::read_csv(raw_lake_bytes)
  })
  readr::write_csv(
    x = temp_lake_file,
    file = lakes_out_name
  )

  cli::cli_alert_success("Downloaded lakeSR sites list as {.file {lakes_filename}}.")

  # Download handoffs
  handoff_filename <- "lakeSR_collated_handoffs_GEEv2025-02-12_QAv2025-06-04.csv"
  handoff_out_name <- file.path(save_location, handoff_filename)

  if(ask == TRUE){
    # Check if a handoff coefficient file with the standard name is already present
    # in the save location:
    if(any(file.exists(handoff_out_name))) {
      user_decision <- ask_user(algal_mask = FALSE,
                                which_sr = "generic",
                                file_message = "handoff coefficient")

      # Act on input
      if (user_decision == "yes") {
        cli::cli_alert_info("Proceeding with download.")
      } else {
        cli::cli_abort("Cancelled by user.")
      }
    }
  }
  dl_handoff <- EDIutils::read_data_entity_names(packageId = lake_sr_id) %>%
    dplyr::filter(entityName == "lakeSR handoff coefficients") %>%
    dplyr::pull(entityId)

  # Read in data as raw bytes
  raw_site_bytes <- EDIutils::read_data_entity(packageId = lake_sr_id,
                                               entityId = dl_handoff)
  # Parse
  suppressMessages({
    temp_handoff_file <- readr::read_csv(raw_site_bytes)
  })
  readr::write_csv(
    x = temp_handoff_file,
    file = handoff_out_name
  )

  cli::cli_alert_success("Downloaded handoff coefficients as {.file {handoff_filename}}.")

  # Suggest citation
  cli::cli_alert_info(
    "lakeSR recommended citation: {EDIutils::read_data_package_citation(packageId = lake_sr_id)}"
  )

  # Combine all paths into a single named vector
  all_downloaded_paths <- c(
    dl_list,
    "lakeSR_site_list" = lakes_out_name,
    "handoffs" = handoff_out_name
  )

  return(invisible(all_downloaded_paths))
}
