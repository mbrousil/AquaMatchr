#' Match in situ data with overpasses
#'
#' @param sat_fp Vector of filepaths to the satellite data
#' @param wqp_data Data frame of in situ data
#' @param site_info Data frame of site metadata containing loc_id and siteSR_id cols
#' @param window Integer of number of days on either side of the in situ measurement
#'
#' @returns
#' @export
#'
#' @examples
make_matchups <- function(sat_fp, wqp_data, site_info, window) {

  # Create min and max times within WQP data corresponding to specified window
  wqp_data <- wqp_data %>%
    mutate(
      min_time = ActivityStartDate - days(window),
      max_time = ActivityStartDate + days(window)
    )

  # Add siteSR_id to WQP data to allow join with siteSR
  wqp_w_ids <- wqp_data %>%
    left_join(x = .,
              y = select(site_info, loc_id, siteSR_id),
              by = c("MonitoringLocationIdentifier" = "loc_id"))

  # Iterate over
  wqp_matchups <- map(
    .x = sat_fp,
    .f = ~{
      temp_file <- read_feather(file = .x) %>%
        filter(siteSR_id %in% unique(wqp_w_ids$siteSR_id)) %>%
        # This duplicate column will be sacrificed during the join below
        mutate(join_date = date)

      # Time range join with data.table
      matchup <- data.table(wqp_w_ids)[data.table(temp_file),
                                       on = .(siteSR_id,
                                              max_time >= join_date,
                                              min_time <= join_date),
                                       nomatch = NULL] %>%
        as_tibble() %>%
        # Calc time difference between reported in situ time and overpass time
        mutate(time_diff = ActivityStartDate - date)

      # Clean up
      rm(temp_file)
      gc()

      return(matchup)
    }
  ) %>%
    # Stack matchups across all missions
    bind_rows()

  # Return to user with addition of col indicating datetime of (in situ - overpass)
  wqp_matchups
}
