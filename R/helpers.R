#' Helper to allow testing of interactive prompts
#' @keywords internal
#' @noRd
get_user_input <- function(prompt = "") {
  readline(prompt = prompt)
}

#' Ask user about redundant downloads
#'
#' If planned downloads already exist locally this function is used to prompt
#' the user to decide (i.e., "yes"/"no") whether the download should continue
#' and the existing files should be overwritten.
#'
#' @param algal_mask Logical. Indicates whether DSWE1 or DSWE1a was requested.
#' @param which_sr String. Options are "lakeSR", "siteSR", or "generic". Generic
#' indicates that the function is being used for something other than the main
#' siteSR or lakeSR data and allows custom messaging.
#' @param file_message String. A custom file name option for "generic" mode.
#' @keywords internal
ask_user <- function(algal_mask, which_sr, file_message) {
  # Check for valid input
  valid_options <- c("lakeSR", "siteSR", "generic")
  if (!(which_sr %in% valid_options)) {
    cli::cli_abort("Input for {.arg which_sr} is not valid. Must be 'lakeSR', 'siteSR', or 'generic'.")
  }

  # Build the options for text to show user
  if (which_sr %in% c("lakeSR", "siteSR")) {
    algal_status <- if (algal_mask) "DSWE1a" else "DSWE1"
    description <- paste0(
      "One or more files for the {.val {which_sr}} version {.val {algal_status}}",
      " appear to already exist in the download location. Would you like to",
      " continue downloading and overwrite?"
    )
  } else {
    description <- paste0(
      "The {.val {file_message}} file appears to already exist in",
      " the download location. Would you like to continue downloading and overwrite?"
    )
  }

  question <- "[yes/no] > "

  # Ask user if they want to continue & check for valid response

  # Print the description separately bc of phantom cursor problems
  cli::cli_alert_warning(description)

  while (TRUE) {
    user_input <- get_user_input(prompt = question)
    # Safety check for case and white space
    user_input <- tolower(trimws(user_input))

    if (user_input %in% c("yes", "no")) {
      return(user_input)
    } else{
      cli::cli_alert_info("Invalid input. Please enter 'yes' or 'no'.")
    }
  }
}

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
#' @return A string containing the [data package identifier](https://edirepository.org/resources/the-data-package#identifiers-of-a-data-package) for an EDI data package
#' @keywords internal
#' @importFrom cli cli_abort
#' @examples
#' \dontrun{
#' # Get the ID for the latest version of the chlorophyll a data product
#' construct_id(identifier = 1756, version = "newest")
#'
#' # Get the ID for the first version of the chlorophyll a data product
#' construct_id(identifier = 1756, version = "1")
#' }
construct_id <- function(identifier, version){

  # Check authentication before attempting any EDI queries
  check_edi_auth()

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
      cli::cli_abort(
        paste0(
          "The package ID does not seem to exist in EDI.",
          " Please check that the {.arg identifier} and {.arg version} are accurate."
        )
      )
    } else {
      return(package_id)
    }

    # If not "newest" and the input is numeric, provide that as version number:
  } else if(grepl(pattern = "[0-9]", x = version)) {

    revision <- version

    package_id <- paste("edi", identifier, revision, sep = ".")

    test_id <- try(EDIutils::read_data_entity_names(package_id), silent = TRUE)
    if(inherits(test_id, "try-error")) {
      cli::cli_abort(
        paste0(
          "The package ID does not seem to exist in EDI.",
          " Please check that the {.arg identifier} and {.arg version} are accurate."
        )
      )
    } else {
      return(package_id)
    }

    # If something unexpected, say so:
  } else {

    cli::cli_abort(
      "Unexpected input to {.arg version} argument. Please use {.val newest} or an integer value."
    )

  }

}

#' Generate checksum for local file
#'
#' @param file_path The path to the local file
#' @param algo The checksum algorithm to use. Defaults to "sha1".
#'
#' @returns A single checksum string for the file at `file_path`
#' @noRd
generate_local_checksum <- function(file_path, algo = "sha1"){
  digest::digest(object = file_path, algo = algo, file = TRUE)
}

#' Return the ID of a single EDI entity by name
#'
#' @param package_id EDI data package ID to search in
#' @param entity_name Name of the entity whose ID should be returned
#'
#' @return The entity ID of the single entity named `entity_name` in
#'   `package_id`. Aborts if zero or multiple entities match, e.g. if the
#'   contents of the EDI package have changed.
#' @noRd
pull_entity_id <- function(package_id, entity_name) {
  entity_ids <- EDIutils::read_data_entity_names(packageId = package_id) %>%
    dplyr::filter(entityName == entity_name) %>%
    dplyr::pull(entityId)

  if (length(entity_ids) != 1L) {
    cli::cli_abort(c(
      "Expected exactly one EDI entity named {.val {entity_name}} in package",
      "{.val {package_id}}, but found {length(entity_ids)}.",
      "i" = "The contents of the EDI package may have changed."
    ), call = NULL)
  }

  entity_ids
}

#' Validate and normalize the update argument
#'
#' @param update Character. How updates to existing files should be handled.
#'   One of "ask", "always", "never", or "if_changed".
#'
#' @return The validated update option, lowercased and whitespace-trimmed.
#' @noRd
check_update_arg <- function(update) {
  if (!is.character(update) || length(update) != 1L || is.na(update)) {
    cli::cli_abort(
      "{.arg update} must be one of 'ask', 'always', 'never', or 'if_changed'.",
      call = NULL
    )
  }

  update <- tolower(trimws(update))

  if (!update %in% c("ask", "always", "never", "if_changed")) {
    cli::cli_abort(
      "{.arg update} must be one of 'ask', 'always', 'never', or 'if_changed'.",
      call = NULL
    )
  }

  update
}

#' Download an EDI entity with a checksum sidecar
#'
#' @description
#' Reuse or replace a local file according to user specified `update`, and record
#' checksums alongside files written by `download_to_path`.
#'
#' @details
#' The sidecar is saved as `<download_filepath>.edi.rds`. It records the EDI
#' entity checksum and a separate checksum of the locally saved file. These
#' checksums may differ when the download callback parses and rewrites the data.
#' An existing file without a readable, matching sidecar is replaced under
#' `update = "if_changed"`. A missing file is downloaded in every mode.
#' The caller handles interactive prompts; this helper does not accept the
#' `"ask"` update option.
#'
#' The file is downloaded to a temporary location in the destination directory
#' and moved into place once the download finishes, so an interrupted download
#' does not leave a partial file at the destination.
#'
#' @param download_filepath The path to the local output file
#' @param update Character. One of `"always"`, `"never"`, or `"if_changed"`:
#'   `"always"` replaces the file; `"never"` keeps an existing file without
#'   checking its contents; `"if_changed"` reuses it only when the sidecar
#'   confirms that the selected EDI entity and the local file are unchanged
#' @param package_id EDI data package ID containing the selected entity
#' @param entity_id ID of the selected entity within `package_id`
#' @param entity_key Stable label identifying the entity across package revisions
#' @param download_to_path A function accepting one output path. It must download
#'   the selected entity and write the local file to that path
#'
#' @return Invisibly, a list with `path` (the local file path) and `downloaded`
#'   (`TRUE` if written on this call; `FALSE` if an existing file was reused).
#' @noRd
#'
#' @examples
#' \dontrun{
#' out_path <- file.path(tempdir(), "sites_list.csv")
#' site_sr_id <- "edi.2254.1"
#' entity_key <- "siteSR sites list"
#' entity_id <- "9d53e6da60d4cef8ff0d59493c84aee6"
#'
#' download_w_sidecar(
#'   download_filepath = out_path,
#'   update = "always",
#'   package_id = site_sr_id,
#'   entity_id = entity_id,
#'   entity_key = entity_key,
#'   download_to_path = function(out_path) {
#'     # Raw EDI data
#'     raw_bytes <- EDIutils::read_data_entity(site_sr_id, entity_id)
#'     # Into csv
#'     readr::write_csv(readr::read_csv(raw_bytes), out_path)
#'   }
#' )
#'
#' # Check contents
#' dir(dirname(out_path))
#' }
download_w_sidecar <- function(download_filepath, update,
                               package_id, entity_id, entity_key,
                               download_to_path){
  # Check
  if (!update %in% c("always", "never", "if_changed")) {
    cli::cli_abort(
      "{.arg update} must be one of 'always', 'never', or 'if_changed'.",
      call = NULL
    )
  }

  sidecar <- paste0(download_filepath, ".edi.rds")
  file_is_present <- file.exists(download_filepath) && !dir.exists(download_filepath)

  # If user specifies "never" and the file exists then we can skip
  if (update == "never" && file_is_present) {
    return(invisible(list(path = download_filepath, downloaded = FALSE)))
  }

  remote_checksum <- NULL

  # For "if_changed", compare the current remote entity to the sidecar
  if (update == "if_changed" && file_is_present) {

    remote_checksum <- EDIutils::read_data_entity_checksum(
      packageId = package_id, entityId = entity_id
    )
    remote_checksum <- tolower(trimws(remote_checksum))

    # Reuse the file only when the sidecar confirms that the remote entity
    # and the local copy are both unchanged
    cache_is_current <- sidecar_is_current(
      download_filepath = download_filepath,
      entity_key = entity_key,
      remote_checksum = remote_checksum
    )

    if (cache_is_current) {
      return(invisible(list(path = download_filepath, downloaded = FALSE)))
    }
  }

  # Download remote entity and create a sidecar for all other cases
  if (is.null(remote_checksum)) {
    remote_checksum <- EDIutils::read_data_entity_checksum(
      packageId = package_id, entityId = entity_id
    )
  }
  remote_checksum <- tolower(trimws(remote_checksum))

  dir.create(dirname(download_filepath), recursive = TRUE, showWarnings = FALSE)

  # Download to a temporary file in the destination directory and move it into
  # place only once the write finishes, so an interrupted download never
  # leaves a partial file at the final path
  temp_path <- tempfile(
    pattern = paste0(".", basename(download_filepath), "."),
    tmpdir = dirname(download_filepath)
  )
  on.exit(unlink(temp_path), add = TRUE)

  download_to_path(temp_path)

  if (!file.exists(temp_path)) {
    cli::cli_abort(
      "{.arg download_to_path} did not create a file at the temporary download location.",
      call = NULL
    )
  }
  if (!file.rename(temp_path, download_filepath)) {
    cli::cli_abort(
      "Could not move the downloaded file into place at {.file {download_filepath}}.",
      call = NULL
    )
  }

  # Construct the sidecar metadata
  metadata <- list(
    entity_key = entity_key,
    package_id = package_id,
    entity_id = entity_id,
    remote_checksum = remote_checksum,
    local_checksum = generate_local_checksum(download_filepath),
    checked_at = Sys.time()
  )
  saveRDS(metadata, sidecar)

  invisible(list(path = download_filepath, downloaded = TRUE))
}


#' Get Arrow schemas for datasets that will be joined
#'
#' @description A helper function that generates `arrow::schema()` objects to
#' prevent errors when joining datasets using \pkg{arrow} and \pkg{duckdb}
#' tools.
#'
#' @param dataset A character string indicating which schema to return. Options
#' are "wqp", "siteSR", or "sitelist".
#' @return An `arrow::schema` object.
#' @keywords internal
#' @noRd
get_arrow_schema <- function(dataset = c("wqp", "siteSR", "sitelist")) {
  dataset <- match.arg(dataset)

  switch(dataset,

         # WQP schema --------------------------------------------------------------
         "wqp" = arrow::schema(
           parameter = arrow::string(),
           OrganizationIdentifier = arrow::string(),
           MonitoringLocationIdentifier = arrow::string(),
           MonitoringLocationTypeName = arrow::string(),
           ResolvedMonitoringLocationTypeName = arrow::string(),

           ActivityStartDate = arrow::date32(),
           ActivityStartTime.Time = arrow::string(),
           ActivityStartTime.TimeZoneCode = arrow::string(),
           harmonized_tz = arrow::string(),
           harmonized_local_time = arrow::timestamp(timezone = "UTC"),
           harmonized_utc = arrow::timestamp(timezone = "UTC"),
           ActivityStartDateTime = arrow::string(),

           harmonized_top_depth_value = arrow::float64(),
           harmonized_top_depth_unit = arrow::string(),
           harmonized_bottom_depth_value = arrow::float64(),
           harmonized_bottom_depth_unit = arrow::string(),
           harmonized_discrete_depth_value = arrow::float64(),
           harmonized_discrete_depth_unit = arrow::string(),

           depth_flag = arrow::string(),
           mdl_flag = arrow::string(),
           approx_flag = arrow::string(),
           greater_flag = arrow::string(),
           tier = arrow::string(),
           field_flag = arrow::string(),
           misc_flag = arrow::string(),

           subgroup_id = arrow::string(),
           harmonized_row_count = arrow::float64(),
           harmonized_units = arrow::string(),
           harmonized_value = arrow::float64(),
           harmonized_value_cv = arrow::float64(),

           lat = arrow::float64(),
           lon = arrow::float64(),
           datum = arrow::string()
         ),

         # sitesSR schema ----------------------------------------------------------
         "siteSR" = arrow::schema(
           siteSR_id = arrow::string(),
           dswe_filter = arrow::string(),
           mission = arrow::string(),
           sat_id = arrow::string(),
           date = arrow::date32(),
           huc2 = arrow::string(),

           # Medians
           med_Aerosol = arrow::float64(),
           med_Blue = arrow::float64(),
           med_Green = arrow::float64(),
           med_Red = arrow::float64(),
           med_Nir = arrow::float64(),
           med_Swir1 = arrow::float64(),
           med_Swir2 = arrow::float64(),
           med_SurfaceTemp = arrow::float64(),
           min_SurfaceTemp = arrow::float64(),

           # Standard deviations
           sd_Aerosol = arrow::float64(),
           sd_Blue = arrow::float64(),
           sd_Green = arrow::float64(),
           sd_Red = arrow::float64(),
           sd_Nir = arrow::float64(),
           sd_Swir1 = arrow::float64(),
           sd_Swir2 = arrow::float64(),
           sd_SurfaceTemp = arrow::float64(),

           # Means
           mean_Aerosol = arrow::float64(),
           mean_Blue = arrow::float64(),
           mean_Green = arrow::float64(),
           mean_Red = arrow::float64(),
           mean_Nir = arrow::float64(),
           mean_Swir1 = arrow::float64(),
           mean_Swir2 = arrow::float64(),
           mean_SurfaceTemp = arrow::float64(),

           # Pixel counts
           pCount_dswe_gt0 = arrow::int32(),
           pCount_dswe1 = arrow::int32(),
           pCount_dswe3 = arrow::int32(),
           pCount_dswe1a = arrow::int32(),

           # Proportions & other floats
           prop_clouds = arrow::float64(),
           prop_hillShadow = arrow::float64(),
           mean_hillShade = arrow::float64(),

           # Identifiers & flags
           source = arrow::string(),
           flag_temp_min = arrow::string(),
           flag_temp_max = arrow::string()
         ),

         # siteSR site list --------------------------------------------------------
         "sitelist" = arrow::schema(
           siteSR_id = arrow::string(),
           org_id = arrow::string(),
           loc_id = arrow::string(),
           harmonized_site_type = arrow::string(),
           WGS84_Latitude = arrow::float64(),
           WGS84_Longitude = arrow::float64(),
           source = arrow::string(),
           HUCEightDigitCode = arrow::string(),
           site_tp_cd = arrow::string(),
           MonitoringLocationTypeName = arrow::string(),
           assigned_HUC = arrow::string(),
           flag_HUC8 = arrow::string(),

           # Waterbody attributes
           wb_nhd_id = arrow::string(),
           wb_gnis_id = arrow::string(),
           wb_gnis_name = arrow::string(),
           wb_fcode = arrow::string(),
           wb_areasqkm = arrow::float64(),
           wb_nhd_source = arrow::string(),
           dist_to_shore = arrow::float64(),
           dist_to_wb = arrow::float64(),
           flag_wb = arrow::string(),

           # Flowline attributes
           fl_nhd_id = arrow::string(),
           fl_gnis_id = arrow::string(),
           fl_gnis_name = arrow::string(),
           fl_fcode = arrow::string(),
           fl_stream_order = arrow::string(),
           fl_nhd_source = arrow::string(),
           dist_to_fl = arrow::float64(),
           flag_fl = arrow::string(),

           # Site intersections and shoreline flags
           number_int_wb = arrow::float64(),
           number_int_fl = arrow::float64(),
           flag_optical_shoreline = arrow::string(),
           flag_thermal_TM_shoreline = arrow::string(),
           flag_thermal_ETM_shoreline = arrow::string(),
           flag_thermal_TIRS_shoreline = arrow::string()
         )
  )
}

#' Check if an Arrow Dataset has the required columns based on a formal schema
#' @param dataset The Arrow Dataset/Table to check.
#' @param target_schema The Arrow Schema object (from `get_arrow_schema()`).
#' @param file_label  String. Name that should be used when referring to the file
#' in messages.
#' @keywords internal
#' @importFrom cli cli_abort
check_cols <- function(dataset, target_schema, file_label = "Input file") {

  # Extract column names from the formal schema
  required_cols <- names(target_schema)
  # Get names actually present in the user's file
  actual_cols <- names(dataset)
  # Check for mismatches
  missing <- setdiff(required_cols, actual_cols)

  if (length(missing) > 0) {
    # {cli} package will return bulleted lists with the syntax below:
    cli::cli_abort(c(
      "{.val {file_label}} is missing these expected columns:",
      "x" = "{missing}",
      "i" = "Please confirm that the correct file has been provided."
    ), call = NULL)
  }
  return(invisible(TRUE))
}

#' Validate time_window argument for DuckDB SQL syntax
#'
#' @param time_window A string like "5 days" or "72 hours".
#' @return invisible(TRUE) if valid, otherwise throws an error.
#' @keywords internal
#' @importFrom cli cli_abort
check_time_window <- function(time_window) {
  # Ensure it is a single character string first
  if (!is.character(time_window) || length(time_window) != 1) {
    cli::cli_abort(c(
      "The {.arg time_window} argument must be a single character string.",
      "i" = "Example: {.val 5 days} or {.val 72 hours}"
    ), call = NULL)
  }

  # Define valid DuckDB interval units
  valid_units <- c(
    "second", "seconds", "minute", "minutes", "hour", "hours", "day", "days",
    "week", "weeks", "month", "months", "year", "years"
  )

  # Clean and split the string by space (extras trimmed)
  clean_str <- trimws(tolower(time_window))
  parsed_time <- strsplit(clean_str, "\\s+")[[1]]

  # Validate the components
  is_valid_format <- length(parsed_time) == 2
  is_numeric_val <- suppressWarnings(!is.na(as.numeric(parsed_time[1])))
  is_valid_unit <- parsed_time[2] %in% valid_units

  # Throw a clean error if any check fails
  if (!is_valid_format || !is_numeric_val || !is_valid_unit) {
    cli::cli_abort(c(
      "Invalid {.arg time_window} format.",
      "x" = "You provided: {.val {time_window}}",
      "i" = "Please provide a number followed by a space and a valid unit.",
      "i" = "Examples: {.val 5 days} or {.val 72 hours}"
    ), call = NULL)
  }

  return(invisible(TRUE))
}

#' Check EDI authentication status
#'
#' Verifies that the user has an active EDI API key or token set in their
#' environment before triggering API requests that require permissions.
#'
#' @keywords internal
#' @importFrom cli cli_abort
check_edi_auth <- function() {
  # Check for active API key or legacy tokens
  has_key <- Sys.getenv("EDI_API_KEY") != ""
  has_token <- Sys.getenv("EDI_TOKEN") != "" || Sys.getenv("AUTH_TOKEN") != ""

  if (!has_key && !has_token) {
    cli::cli_abort(
      c(
        "x" = "Authentication is required to query or download data from EDI.",
        "i" = "Please authenticate before calling this function:",
        "*" = "Option 1: Run {.code EDIutils::login()}",
        "*" = "Option 2: Set your API key via {.code Sys.setenv(EDI_API_KEY = 'your_key')}",
        "i" = "To store it permanently, add {.code EDI_API_KEY='your_key'} to your {.file .Renviron} file.",
        "i" = "For more details on EDI authentication, see: {.url https://docs.ropensci.org/EDIutils/}"
      ),
      call = NULL
    )
  }

  return(invisible(TRUE))
}

#' Infer Arrow file format from a file path's extension
#'
#' @param path A character string containing a file path.
#' @param allowed Optional character vector of allowed formats. Options are
#'   "csv", "feather", and "parquet". Defaults to all three.
#'
#' @return A length-one character string: "csv", "feather", or "parquet".
#' @keywords internal
#' @importFrom cli cli_abort
infer_arrow_format <- function(path, allowed = c("csv", "feather", "parquet")) {
  ext <- sub("^.*\\.", "", tolower(basename(path)))

  format <- switch(
    ext,
    "csv" = "csv",
    "feather" = "feather",
    "parquet" = "parquet",
    NA_character_
  )

  # Unrecognized/missing extension
  if (is.na(format)) {
    cli::cli_abort(
      c("The file {.file {path}} does not have a supported extension.",
        "i" = "Please use a {.val .csv}, {.val .feather}, or {.val .parquet} file."),
      call = NULL
    )
  }

  # Recognized extension, but not one this argument accepts
  if (!format %in% allowed) {
    cli::cli_abort(
      c("The file {.file {path}} has an unsupported format for this argument.",
        "i" = "Please provide one of: {.val {allowed}}."),
      call = NULL
    )
  }

  format
}

#' Check whether a checksum sidecar indicates an unchanged local file
#'
#' @param download_filepath Path to the local file
#' @param entity_key Stable label identifying the remote entity
#' @param remote_checksum Current remote checksum of the entity
#' @param sidecar_suffix Suffix appended to `download_filepath` to locate the
#'   sidecar file
#' @param algo Checksum algorithm used for the local file. Must match the
#'   algorithm used when the sidecar was written.
#'
#' @return TRUE if the sidecar exists and records matching `entity_key` and
#'   `remote_checksum` values plus an unmodified local file; FALSE otherwise.
#' @noRd
sidecar_is_current <- function(download_filepath, entity_key, remote_checksum,
                               sidecar_suffix = ".edi.rds", algo = "sha1") {
  sidecar <- paste0(download_filepath, sidecar_suffix)

  metadata <- if (file.exists(sidecar)) {
    tryCatch(readRDS(sidecar), error = function(e) NULL)
  } else {
    NULL
  }

  remote_matches <- identical(
    tolower(metadata$remote_checksum),
    tolower(trimws(remote_checksum))
  )

  is.list(metadata) &&
    identical(metadata$entity_key, entity_key) &&
    remote_matches &&
    identical(metadata$local_checksum,
              generate_local_checksum(download_filepath, algo = algo))
}

#' Fetch file names and checksums from a Zenodo record
#'
#' @param doi String. DOI of a published Zenodo record.
#'
#' @return A named character vector of lowercase MD5 checksums (hex strings)
#'   with the record's file names as names.
#' @noRd
pull_zenodo_files <- function(doi) {
  record <- zen4R::ZenodoManager$new()$getRecordByDOI(doi)
  file_info <- record$listFiles(pretty = TRUE)

  checksums <- tolower(sub("^md5:", "", file_info$checksum))
  stats::setNames(checksums, file_info$filename)
}
