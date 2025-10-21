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

  # For each param, read and save in list
  split(param_selection, f = param_selection$param) %>%
    purrr::map(.x = .,
               .f = ~{
                 # EDI package ID
                 param_id <- construct_id(identifier = .x$identifier, version = version)

                 # EDI entity ID (specific file to download)
                 entity_id <- EDIutils::read_data_entity_names(packageId = param_id) %>%
                   dplyr::filter(entityName == .x$entity_name) %>%
                   dplyr::pull(entityId)

                 # Read in data as raw bytes
                 raw_bytes <- EDIutils::read_data_entity(packageId = param_id,
                                                         entityId = entity_id)
                 # Parse
                 readr::read_csv(raw_bytes)
               })
}
