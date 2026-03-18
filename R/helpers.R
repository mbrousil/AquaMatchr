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
#' @keywords internal
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

#' Get Arrow schemas for datasets that will be joined
#'
#' @description A helper function that generates `arrow::schema()` objects to
#' prevent errors when joining datasets using {arrow} and {duckdb} tools.
#'
#' @param dataset A character string indicating which schema to return. Options
#' are "wqp", "siteSR", or "sitelist".
#' @returns An `arrow::schema` object.
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
check_cols <- function(dataset, target_schema, file_label = "Input file") {

  # Extract column names from the formal schema
  required_cols <- names(target_schema)
  # Get names actually present in the user's file
  actual_cols <- names(dataset)
  # Check for mismatches
  missing <- setdiff(required_cols, actual_cols)

  if (length(missing) > 0) {
    stop(
      paste0(
        file_label, " is missing these expected columns:\n",
        paste("-", missing, collapse = "\n"),
        "\n\nPlease confirm that the correct file has been provided."
      ),
      call. = FALSE
    )
  }
  return(invisible(TRUE))
}

#' Validate time_window argument for DuckDB SQL syntax
#'
#' @param time_window A string like "5 days" or "72 hours".
#' @return invisible(TRUE) if valid, otherwise throws an error.
#' @keywords internal
check_time_window <- function(time_window) {
  # Ensure it is a single character string first
  if (!is.character(time_window) || length(time_window) != 1) {
    stop("The time_window argument must be a single character string (e.g., '5 days').", call. = FALSE)
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
    stop(
      "Invalid time_window format. Please provide a number followed by a space and a valid unit (e.g., '5 days', '72 hours').\n",
      "You provided: '", time_window, "'",
      call. = FALSE
    )
  }

  return(invisible(TRUE))
}
