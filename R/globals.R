# Global variable declarations to satisfy R CMD check.
# These are variables used in, e.g., dplyr, purrr, and DuckDB/SQL contexts where
# they are not explicitly defined as objects in the global environment.

#' @importFrom utils globalVariables
utils::globalVariables(
  c(
    # Piping and purrr symbols
    ".",

    # download_parameters and metadata variables
    "param", "identifier", "entity_name",

    # EDI and download utility variables
    "entityName", "entityId",

    # siteSR and lakeSR data columns
    "WGS84_Longitude", "WGS84_Latitude", "date",
    "MonitoringLocationIdentifier", "loc_id", "siteSR_id",

    # DuckDB / SQL Join temporary variables
    "utc_seconds_offset", "landsat_utc", "join_min", "join_max",
    "harmonized_utc", "time_diff"
  )
)
