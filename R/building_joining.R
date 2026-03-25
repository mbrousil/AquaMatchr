#' Build and save siteSR or lakeSR products from downloaded files
#'
#' @details
#' Reads and stacks siteSR or lakeSR files into a single object (an Arrow Table),
#' then optionally exports them to a single local file. The user can provide a
#' vector of filenames to the `sr_files` argument, which will then be used as
#' the input files. If this argument is not used, then the value of `which_sr`
#' will be used to infer the filenames based on the default outputs of
#' `download_siteSR()` or `download_lakeSR()`.
#'
#' It is often possible to use Arrow Tables with `dplyr` syntax, but users may want
#' to read the [Apache Arrow documentation on Tables](https://arrow.apache.org/docs/r/articles/data_objects.html#tables)
#' or the *R for Data Science* [chapter on Arrow](https://r4ds.hadley.nz/arrow.html#using-dplyr-with-arrow)
#' if these data structures are new to them. We use Arrow Tables because of their
#' efficiency and convenience when working with large datasets.
#'
#' If a file export is requested (i.e., `save` is TRUE), then the file is exported
#' to a user-specified location. If the user provides a path to a .feather file
#' in `save_location` then that will be used, otherwise a standardized filename
#' will be used and saved to the directory in `save_location`.
#'
#' @param which_sr String. Options are "siteSR" or "lakeSR", indicating which of
#' the two SR products should be built.
#' @param sr_location String. Path to location where SR files are saved.
#' @param algal_mask Logical. If TRUE, the algal mask version of the dataset (DSWE1a)
#' will be expected in `sr_location`. Otherwise DSWE1 is expected (i.e., FALSE).
#' @param sr_files Optional. A vector of filenames (five at most) with siteSR or
#' lakeSR files, like would be saved when running `download_lakeSR()` or `download_siteSR()`.
#' Should *not* include the directory provided in `sr_location`.
#' @param save Logical. Should the built SR dataset be saved locally? Defaults to FALSE.
#' @param save_location String. If save == TRUE, the path to the folder where the
#' output file should be saved. If a name ending in ".feather" is provided as part
#' of the path then this is the name that the file will be saved with. Otherwise,
#' a default name will be used by the function. It will always be a .feather file.
#' If a non-feather file is provided, the function will save the file to the directory
#' indicated by save_location, but under a different, standardized filename.
#'
#' @return An [Arrow Table](https://arrow.apache.org/docs/r/articles/data_objects.html#tables)
#' representing the SR dataset.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
build_sr <- function(which_sr, sr_location, algal_mask = NULL, sr_files = NULL,
                     save = FALSE, save_location = NULL){
  # Confirm correct use of SR tag
  if(!(which_sr == "lakeSR" | which_sr == "siteSR")){
    stop("Input for which_sr argument is not valid. Must be 'lakeSR' or 'siteSR'.")
  }

  # Confirm correct use of algal_mask
  if(!is.logical(algal_mask)){
    stop("Input for algal_mask argument is not a logical value. Must be TRUE or FALSE.")
  }

  # Make sure the (optional) save_location exists upfront if it's expected
  if(save){
    # No info provided = error
    if(is.null(save_location)){
      stop("Please provide a value for save_location.")
    } else if(!is.null(save_location)){
      save_info <- file.info(save_location)
      # NA for file.info$isdir = DNE
      if(is.na(save_info$isdir)){
        stop("The directory or file at save_location does not appear to exist.")
      }
    }
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

  # Read files and stack
  # We use the {arrow} package to concatenate the datasets into a single table
  # instead of something like rbind(), which likely will use more memory than is
  # available to the user.
  unified_sr_dataset <- arrow::open_dataset(
    # All files indicated to contain SR data
    sources = file_list,
    # The files should not be assumed to have Hive-style partitioning
    hive_style = FALSE,
    # They are saved as .feather files
    format = "feather",
    # Don't assume all files in the file_list have the same scheme. For
    # example, Aerosols cols may not be present in all
    unify_schemas = TRUE
  ) %>%
    # Convert to Arrow Table
    arrow::as_arrow_table()

  # Now export if requested by user
  if(save){
    # Standard name, in case filename not provided by user
    std_out_name <- paste0(input_string, "_full_concatenation.feather")

    # Save with standard filename if a directory is provided
    if(save_info$isdir){
      full_out_name <- file.path(save_location, std_out_name)

      arrow::write_feather(
        x = unified_sr_dataset,
        sink = full_out_name
      )

      message(
        paste0(
          "Saving SR file as ",
          full_out_name
        )
      )
      # A filename is provided, but it's not a .feather file
    } else if(!(save_info$isdir) & !grepl(pattern = "\\.feather$", x = save_location)){
      # Write to dir provided, but with a standard name
      emergency_out_name <- file.path(dirname(save_location), std_out_name)

      arrow::write_feather(
        x = unified_sr_dataset,
        sink = emergency_out_name
      )

      # Alert user
      message(
        paste0(
          "A non-feather file was indicated by save_location. Saving SR file as ",
          emergency_out_name
        )
      )
      # A .feather file is provided
    } else if(!(save_info$isdir) & grepl(pattern = "\\.feather$", x = save_location)){

      # Write externally as a single .feather file
      arrow::write_feather(
        x = unified_sr_dataset,
        sink = save_location
      )
    }

  }

  return(unified_sr_dataset)
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
#' Note that this function requires a .parquet file for output of the join. This
#' is because all join computation takes place out of memory and therefore cannot
#' be completed using other filetypes such as .csv or .feather.
#'
#' @param wqp_path Path to the file (.csv or .feather) storing the AquaMatch
#' parameter data to be joined. Should be data from a single parameter (e.g.,
#' chlorophyll *a*), as would be saved after using the `download_parameters()`
#' function.
#' @param siteSR_path Path to the file (.feather) storing the stacked version of
#' the siteSR dataset (either DSWE1 or DSWE1a). This is the equivalent of the
#' direct output of the `build_sr()` function when run with siteSR data.
#' @param site_list_path Path to the file (.csv) storing the site list for siteSR.
#' This is included in downloads done using `download_siteSR()`.
#' @param time_window A string indicating the amount of time on either side of the
#' in-situ measurements that should be used to match to siteSR overpass times, for
#' example: "2 days", "72 hours". Defaults to "5 days".
#' @param save_location String. The path where a parquet file containing the output
#' should be saved. If the string does not end in ".parquet" then an error will occur.
#' @return The path to the joined dataset. (Invisible)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
match_siteSR_to_WQP <- function(wqp_path, siteSR_path, site_list_path,
                                save_location,
                                time_window = "5 days"){

  # Ensure files exist
  if (!file.exists(wqp_path)) stop("File not found at wqp_path.")
  if (!file.exists(siteSR_path)) stop("File not found at siteSR_path.")
  if (!file.exists(site_list_path)) stop("File not found at site_list_path.")

  # Is WQP data csv or feather?
  if (grepl("\\.csv$", wqp_path)) {
    wqp_format <- "csv"
  } else if (grepl("\\.feather$", wqp_path)) {
    wqp_format <-  "feather"
  }

  # Check file cols to make sure things look right:

  # WQP:
  # Peek at file
  raw_wqp <- arrow::open_dataset(wqp_path, format = wqp_format)
  # Retrieve expected schema
  wqp_schema <- get_arrow_schema(dataset = "wqp")
  # Validate
  check_cols(dataset = raw_wqp, target_schema = wqp_schema, file_label = "WQP file")

  # siteSR
  raw_siteSR <- arrow::open_dataset(siteSR_path, format = "feather")
  siteSR_schema <- get_arrow_schema(dataset = "siteSR")
  check_cols(dataset = raw_siteSR, target_schema = siteSR_schema, file_label = "siteSR file")

  # site_list

  # Connect to DuckDB
  # Use on.exit() to ensure the connection closes cleanly
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Read datasets lazily via Arrow, injecting the correct schemas
  wqp_ds <- arrow::open_dataset(
    sources = wqp_path,
    format = wqp_format,
    col_types = get_arrow_schema("wqp")
  ) %>%
    # Check for NAs and correct (issue if csv is used, but won't accept null_values
    # if format is feather)
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::where(is.character),
        .fns = ~ dplyr::if_else(.x == "NA", NA_character_, .x)
      )
    )

  siteSR_ds <- arrow::open_dataset(
    sources = siteSR_path,
    format = "feather",
    col_types = get_arrow_schema("siteSR")
  )

  site_list_ds <- arrow::open_dataset(
    sources = site_list_path,
    format = "csv",
    col_types = get_arrow_schema("sitelist"),
    null_values = "NA"
  )

  # Register Arrow Datasets as DuckDB virtual tables
  wqp_db <- arrow::to_duckdb(wqp_ds, con, "wqp_tbl")
  siteSR_db <- arrow::to_duckdb(siteSR_ds, con, "siteSR_tbl")
  site_list_db <- arrow::to_duckdb(site_list_ds, con, "sitelist_tbl")

  # Create a new column of local times converted to UTC for landsat overpasses
  site_list_prep <- site_list_db %>%
    dplyr::mutate(
      # 612 mins = 10:12 AM local solar time. 1 degree lon = 4 mins
      # Multiply by 60 for secs & round to nearest int
      utc_seconds_offset = round((612 - (WGS84_Longitude * 4)) * 60)
    ) %>%
    dplyr::select(loc_id, siteSR_id, utc_seconds_offset)

  # Combine date and offset to get landsat_utc for joins
  siteSR_prep <- siteSR_db %>%
    dplyr::inner_join(
      # Join just the offset by siteSR_id
      site_list_prep %>% dplyr::select(siteSR_id, utc_seconds_offset),
      by = "siteSR_id"
    ) %>%
    dplyr::mutate(
      # Cast the date to a Midnight UTC timestamp, then add the minute offset
      landsat_utc = dplyr::sql("CAST(date AS TIMESTAMP) + INTERVAL 1 SECOND * CAST(utc_seconds_offset AS INTEGER)")
    )

  # Build the lazy query using dbplyr
  matchups_lazy <- wqp_db %>%
    dplyr::mutate(
      # Create UTC time window bounds based on the in-situ data
      join_min = dplyr::sql("CAST(harmonized_utc AS TIMESTAMP)") - dplyr::sql(paste0("INTERVAL '", time_window, "'")),
      join_max = dplyr::sql("CAST(harmonized_utc AS TIMESTAMP)") + dplyr::sql(paste0("INTERVAL '", time_window, "'"))
    ) %>%
    dplyr::left_join(
      site_list_prep %>% dplyr::select(loc_id, siteSR_id),
      by = c("MonitoringLocationIdentifier" = "loc_id")
    ) %>%
    # Join to prepared siteSR data
    dplyr::inner_join(siteSR_prep, by = "siteSR_id") %>%
    # Filter based on the calculated landsat_utc
    dplyr::filter(landsat_utc >= join_min, landsat_utc <= join_max) %>%
    dplyr::mutate(
      # Calculate precise time difference in days
      time_diff = dplyr::sql("date_diff('second', landsat_utc, CAST(harmonized_utc AS TIMESTAMP)) / 86400.0")
    )

  # Extract the translated SQL query
  sql_query <- dbplyr::sql_render(matchups_lazy)

  # Execute an out-of-memory write directly to Parquet via DuckDB:

  # A filename is provided, but it's not a .parquet file
  if(!grepl(pattern = "\\.parquet$", x = save_location)){
    stop("A non-parquet file was indicated by save_location. Please supply a .parquet name.")
    # A .parquet file is provided
  } else if(grepl(pattern = "\\.parquet$", x = save_location)){

    # We bypass dbplyr/arrow collection functions to guarantee it never hits R's RAM
    copy_query <- sprintf(
      "COPY (%s) TO '%s' (FORMAT PARQUET, CODEC 'ZSTD');",
      sql_query,
      save_location
    )
  }

  # Execute query and catch number of rows affected by it
  rows_affected <- DBI::dbExecute(con, copy_query)

  # Alert success to user
  cli::cli_alert_success(
    "Successfully wrote {format(rows_affected, big.mark = ',')} matchups to {.file {save_location}}."
  )

  # Return path to file, quietly
  return(invisible(save_location))
}
