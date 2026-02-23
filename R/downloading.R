#' Construct EDI identifier from user inputs
#'
#' @description
#' A helper function that serves mostly to automate getting the newest version
#' of an Environmental Data Initiative package ID. Also tests that the output
#' ID links to an actual package.
#'
#' @param identifier The accession number corresponding to an EDI package
#' @param version Either "newest" or an integer corresponding to the data package version
#'
#' @returns A string containing the [data package identifier](https://edirepository.org/resources/the-data-package#identifiers-of-a-data-package) for an EDI data package
#'
#' @examples
#' # Get the ID for the latest version of the chlorophyll a data product
#' construct_id(identifier = 1756, version = "newest")
#'
#' # Get the ID for the first version of the chlorophyll a data product
#' construct_id(identifier = 1756, version = "1")

construct_id <- function(identifier, version){

  # If the user indicates they want the newest version of the pub, ask EDI for it:
  if(version == "newest") {

    revision <- EDIutils::list_data_package_revisions(
      scope = "edi",
      identifier = identifier,
      # Most recent version of published data
      filter = "newest"
    )

    # Put together the ID from the above info:
    package_id <- paste("edi", identifier, revision, sep = ".")

    # Confirm that this ID works and export it if it does
    test_id <- try(EDIutils::read_data_entity_names(package_id), silent = TRUE)
    if(inherits(test_id, "try-error")) {
      stop("The package ID does not seem to exist in EDI. Please check that the identifier and version are accurate.")
    } else {
      return(package_id)
    }

    # If not "newest" and the input is numeric, provide that as version number:
  } else if(grepl(pattern = "[0-9]", x = version)) {

    revision <- version

    package_id <- paste("edi", identifier, revision, sep = ".")

    test_id <- try(EDIutils::read_data_entity_names(package_id), silent = TRUE)
    if(inherits(test_id, "try-error")) {
      stop("The package ID does not seem to exist in EDI. Please check that the identifier and version are accurate.")
    } else {
      return(package_id)
    }

    # If something unexpected, say so:
  } else {

    stop("Unexpected input to version argument. Please use 'newest' or an integer value.")

  }

}

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
#' @returns A named list where each item is a tibble containing a dataset.
#' @export
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
    stop("The provided input for the parameters argument does not match the available options. Please check case and spelling.")
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
                 message(
                   switch(.x$param,
                          "chla" = "Chlorophyll",
                          "doc" = "Dissolved organic carbon",
                          "sdd" = "Secchi disk depth",
                          "tss" = "Total suspended solids"),
                   " recommended citation: ",
                   EDIutils::read_data_package_citation(packageId = param_id)
                 )

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


#' Download RiverSR dataset from Zenodo
#'
#' @description
#' A function to facilitate downloading of the RiverSR data product from Zenodo.
#' It is mostly a wrapper around `zen4R::download_zenodo()`.
#'
#' @param save_path A string containing the path to the folder where the dataset
#'  should be saved.
#' @param timeout_length The number of seconds to allow for the download. Defaults
#' to 4000 based on tests with the RiverSR dataset, but can be adjusted as needed.
#'
#' @returns The path to the downloaded file.
#' @export
#'
#' @examples
#' \dontrun{
#' download_RiverSR(save_path = "~/Downloads/")
#' }
download_RiverSR <- function(save_path, timeout_length = 4000){

  # Warning to user
  message(
    "The size of this file is large (>13GB) so the download will take some time."
  )

  zen4R::download_zenodo(path = save_path,
                         doi = "10.5281/zenodo.4304567",
                         files = "riverSR_usa_v1.1.feather",
                         timeout = timeout_length)

  # Confirm file saved and report back
  out_file <- file.path(save_path, "riverSR_usa_v1.1.feather")

  if(file.exists(out_file)){
    message(
      "RiverSR recommended citation: John Gardner, Xiao Yang, Simon Topp, Matthew Ross, Tamlin Pavelsky, & Elizabeth Altenau. (2020). River Surface Reflectance Database (RiverSR) (v1.1.0) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.4304567. ",
      "Accessed ", Sys.Date(), "."
    )
    return(out_file)
  } else {
    stop("Output file cannot be found.")
  }
}


#' Ask user about redundant downloads
#'
#' If planned downloads already exist locally this function is used to prompt
#' the user to decide (i.e., "yes"/"no") whether the download should continue
#' and the existing files should be overwritten.
#'
#' @param algal_mask Logical. Indicates whether DSWE1 or DSWE1a was requested.
#' @param which_sr String. Options are "LakeSR", "SiteSR", or "generic". Generic
#' indicates that the function is being used for something other than the main
#' SiteSR or LakeSR data and allows custom messaging.
ask_user <- function(algal_mask, which_sr, file_message) {

  if(!(which_sr == "LakeSR" | which_sr == "SiteSR" | which_sr == "generic")){
    stop("Input for which_sr argument is not valid. Must be 'LakeSR', 'SiteSR', or 'generic'.")
  }

  # LakeSR / SiteSR functionality
  if(which_sr %in% c("LakeSR", "SiteSR")){
    algal_status <- if_else(
      condition = algal_mask,
      true = "DSWE1a",
      false = "DSWE1"
    )

    # Text to show user
    user_prompt <- cat(
      "One or more files for the ", which_sr, " version ",
      algal_status,
      " appear to already exist in the download location.\n",
      "Would you like to continue downloading and overwrite them? [yes/no]",
      sep = ""
    )

    # Ask user if they want to continue & check for valid response
    while (TRUE) {
      user_input <- readline(prompt = user_prompt)
      # Convert response to lower and no whitespace
      user_input <- tolower(trimws(user_input))
      if (user_input == "yes" || user_input == "no") {
        return(user_input)
      } else {
        cat("Invalid input. Please enter 'yes' or 'no'.\n")
      }
    }
  }

  # Generic functionality
  if(which_sr == "generic"){
    # Text to show user
    user_prompt <- cat(
      "The ", file_message, " file appears to already exist in the download location.\n",
      "Would you like to continue downloading and overwrite it? [yes/no]",
      sep = ""
    )

    # Ask user if they want to continue & check for valid response
    while (TRUE) {
      user_input <- readline(prompt = user_prompt)
      # Convert response to lower and no whitespace
      user_input <- tolower(trimws(user_input))
      if (user_input == "yes" || user_input == "no") {
        return(user_input)
      } else {
        cat("Invalid input. Please enter 'yes' or 'no'.\n")
      }
    }
  }

}


#' Download SiteSR dataset from EDI
#'
#' @description
#' A function to facilitate downloading of the SiteSR data product from
#' the Environmental Data Initiative (EDI).
#'
#' @param save_path A string containing the path to the folder where the dataset
#' should be saved.
#' @param algal_mask Logical. If TRUE, the algal mask version of the dataset (DSWE1a)
#' will be downloaded. Otherwise DSWE1 is used (i.e., FALSE).
#' @param version Either "newest" or an integer corresponding to the data package
#' version to use.
#'
#' @returns
#' @export
#'
#' @examples
download_SiteSR <- function(save_path, algal_mask = FALSE, version = "newest"){

  # SiteSR EDI ID
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

    # Check if any files with the standard names are already present in the save
    # location:
    if(any(file.exists(file.path(save_path, dswe1_names)))) {
      user_decision <- ask_user(algal_mask = FALSE, which_sr = "SiteSR")

      # Act on input
      if (user_decision == "yes") {
        message("Proceeding with download.")
      } else {
        stop("Cancelled by user.", call. = FALSE)
      }
    }

    dl_entities <- EDIutils::read_data_entity_names(packageId = site_sr_id) %>%
      filter(grepl(pattern = "DSWE = 1\\)$", x = entityName))

    # With algal mask (DSWE1a)
  } else if(algal_mask){

    # Check if any files with the standard names are already present in the save
    # location:
    if(any(file.exists(file.path(save_path, dswe1_names)))) {
      user_decision <- ask_user(algal_mask = TRUE, which_sr = "SiteSR")

      # Act on input
      if (user_decision == "yes") {
        message("Proceeding with download.")
      } else {
        stop("Cancelled by user.", call. = FALSE)
      }
    }

    dl_entities <- EDIutils::read_data_entity_names(packageId = site_sr_id) %>%
      filter(grepl(pattern = "DSWE = 1a", x = entityName))
  }

  message("This is a series of large downloads. It will take several minutes.")

  # For each param, read, message citation, and save in list
  dl_list <- split(dl_entities, f = dl_entities$entityName) %>%
    purrr::walk(.x = .,
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
                    sink = file.path(save_path, out_name)
                  )

                  message(
                    "Downloaded ",
                    .x$entityName,
                    " as ",
                    out_name,
                    "."
                  )
                })


  # Check if a site info file with the standard name is already present in the save
  # location:
  sites_filename <- "siteSR_collated_WQP_NWIS_sites_with_NHD_info_2025-06-04.csv"
  sites_out_name <- file.path(save_path, sites_filename)
  if(any(file.exists(sites_out_name))) {
    user_decision <- ask_user(algal_mask = FALSE,
                              which_sr = "generic",
                              file_message = "SiteSR site information")

    # Act on input
    if (user_decision == "yes") {
      message("Proceeding with download.")
    } else {
      stop("Cancelled by user.", call. = FALSE)
    }

    dl_sites <- EDIutils::read_data_entity_names(packageId = site_sr_id) %>%
      filter(entityName == "siteSR sites list") %>%
      pull(entityId)

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

    message(
      "Downloaded siteSR sites list as ",
      sites_filename,
      "."
    )


  }


  # Suggest citation
  message(
    "SiteSR recommended citation: ",
    EDIutils::read_data_package_citation(packageId = site_sr_id)
  )

}


#' Download LakeSR dataset from EDI
#'
#' @description
#' A function to facilitate downloading of the LakeSR data product from
#' the Environmental Data Initiative (EDI).
#'
#' @param save_path A string containing the path to the folder where the dataset
#' should be saved.
#' @param algal_mask Logical. If TRUE, the algal mask version of the dataset (DSWE1a)
#' will be downloaded. Otherwise DSWE1 is used (i.e., FALSE).
#' @param version Either "newest" or an integer corresponding to the data package
#' version to use.
#'
#' @returns
#' @export
#'
#' @examples
download_LakeSR <- function(save_path, algal_mask = FALSE, version = "newest"){

  # LakeSR EDI ID
  lake_sr_id <- construct_id(identifier = 2114, version = version)

  # Filenames to be used for dswe1 and 1a, respectively:
  dswe1_names <- c(
    "LakeSR_Landsat4_DSWE1_2025-06-04.feather",
    "LakeSR_Landsat5_DSWE1_2025-06-04.feather",
    "LakeSR_Landsat7_DSWE1_2025-06-04.feather",
    "LakeSR_Landsat8_DSWE1_2025-06-04.feather",
    "LakeSR_Landsat9_DSWE1_2025-06-04.feather"
  )

  dswe1a_names <- c(
    "LakeSR_Landsat4_DSWE1a_2025-06-04.feather",
    "LakeSR_Landsat5_DSWE1a_2025-06-04.feather",
    "LakeSR_Landsat7_DSWE1a_2025-06-04.feather",
    "LakeSR_Landsat8_DSWE1a_2025-06-04.feather",
    "LakeSR_Landsat9_DSWE1a_2025-06-04.feather"
  )

  # No algal mask (DWSE1)
  if(!algal_mask){

    # Check if any files with the standard names are already present in the save
    # location:
    if(any(file.exists(file.path(save_path, dswe1_names)))) {
      user_decision <- ask_user(algal_mask = FALSE, which_sr = "LakeSR")

      # Act on input
      if (user_decision == "yes") {
        message("Proceeding with download.")
      } else {
        stop("Cancelled by user.", call. = FALSE)
      }
    }

    dl_entities <- EDIutils::read_data_entity_names(packageId = lake_sr_id) %>%
      filter(grepl(pattern = "DSWE = 1\\)$", x = entityName))

    # With algal mask (DSWE1a)
  } else if(algal_mask){

    # Check if any files with the standard names are already present in the save
    # location:
    if(any(file.exists(file.path(save_path, dswe1_names)))) {
      user_decision <- ask_user(algal_mask = TRUE, which_sr = "LakeSR")

      # Act on input
      if (user_decision == "yes") {
        message("Proceeding with download.")
      } else {
        stop("Cancelled by user.", call. = FALSE)
      }
    }

    dl_entities <- EDIutils::read_data_entity_names(packageId = lake_sr_id) %>%
      filter(grepl(pattern = "DSWE = 1a", x = entityName))
  }

  message("This is a series of large downloads. It will take several minutes.")

  # For each param, read, message citation, and save in list
  dl_list <- split(dl_entities, f = dl_entities$entityName) %>%
    purrr::walk(.x = .,
                .f = ~{
                  print(.x$entityName)
                  out_name <- switch(
                    .x$entityName,
                    "LakeSR data from Landsat 4, DSWE filter for confident water (DSWE = 1)" = "LakeSR_Landsat4_DSWE1_2025-06-04.feather",
                    "LakeSR data from Landsat 4, DSWE filter for confident water and algal mask (DSWE = 1a)" = "LakeSR_Landsat4_DSWE1a_2025-06-04.feather",
                    "LakeSR data from Landsat 5, DSWE filter for confident water (DSWE = 1)" = "LakeSR_Landsat5_DSWE1_2025-06-04.feather",
                    "LakeSR data from Landsat 5, DSWE filter for confident water and algal mask (DSWE = 1a)" = "LakeSR_Landsat5_DSWE1a_2025-06-04.feather",
                    "LakeSR data from Landsat 7, DSWE filter for confident water (DSWE = 1)" = "LakeSR_Landsat7_DSWE1_2025-06-04.feather",
                    "LakeSR data from Landsat 7, DSWE filter for confident water and algal mask (DSWE = 1a)" = "LakeSR_Landsat7_DSWE1a_2025-06-04.feather",
                    "LakeSR data from Landsat 8, DSWE filter for confident water (DSWE = 1)" = "LakeSR_Landsat8_DSWE1_2025-06-04.feather",
                    "LakeSR data from Landsat 8, DSWE filter for confident water and algal mask (DSWE = 1a)" = "LakeSR_Landsat8_DSWE1a_2025-06-04.feather",
                    "LakeSR data from Landsat 9, DSWE filter for confident water (DSWE = 1)" = "LakeSR_Landsat9_DSWE1_2025-06-04.feather",
                    "LakeSR data from Landsat 9, DSWE filter for confident water and algal mask (DSWE = 1a)" = "LakeSR_Landsat9_DSWE1a_2025-06-04.feather"
                  )
                  print(out_name)
                  # Read in data as raw bytes
                  raw_bytes <- EDIutils::read_data_entity(packageId = lake_sr_id,
                                                          entityId = .x$entityId)
                  # Parse
                  temp_file <- arrow::read_feather(raw_bytes)

                  arrow::write_feather(
                    x = temp_file,
                    sink = file.path(save_path, out_name)
                  )
                })

  # Download sites list


  # Suggest citation
  message(
    "LakeSR recommended citation: ",
    EDIutils::read_data_package_citation(packageId = lake_sr_id)
  )

  return(dl_list)
}
