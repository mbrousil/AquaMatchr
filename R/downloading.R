#' Construct EDI identifier from user inputs
#'
#' @description
#' A helper function that serves mostly to automate getting the newest version
#' of an Environmental Data Initiative package ID. Also tests that the output
#' ID links to an actual package.
#'
#' @param identifier The accession number corresponding to an EDI package
#' @param version Either "newest" or an integer corresponding to the package version
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

    stop("Unexpected input to version argument. Please use 'newest' or a numeric value.")

  }

}


download_chlorophyll <- function(version = "newest"){

}
