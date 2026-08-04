test_that("build_sr fails on bad SR input", {
  expect_error(
    build_sr(
      # DNE
      which_sr = "pondSR",
      sr_location = tempdir(),
      algal_mask = FALSE,
      save = TRUE,
      save_location = tempfile(fileext = ".feather")
    ),
    regexp = "Must be .*lakeSR.* or .*siteSR.*"
  )
})

test_that("build_sr fails on non-logical algal mask", {
  expect_error(
    build_sr(
      which_sr = "lakeSR",
      sr_location = tempdir(),
      # Not logical
      algal_mask = "False",
      save = TRUE,
      save_location = tempfile(fileext = ".feather")
    ),
    regexp = "Must be .*TRUE.* or .*FALSE.*"
  )
})

test_that("build_sr fails on missing input files", {
  expect_error(
    build_sr(
      which_sr = "lakeSR",
      # Empty
      sr_location = tempdir(),
      algal_mask = FALSE,
      save = TRUE,
      save_location = tempfile(fileext = ".feather")
    ),
    regexp = "SR files were not detected"
  )
})

test_that("build_sr routes saved files correctly based on save_location", {

  # Temporary directories for inputs and outputs
  tmp_base <- tempfile()
  dir.create(tmp_base)
  on.exit(unlink(tmp_base, recursive = TRUE))

  input_dir <- file.path(tmp_base, "input")
  dir.create(input_dir)

  out_dir_fallback <- file.path(tmp_base, "fallback_dir")
  dir.create(out_dir_fallback)

  # Create a tiny, valid .feather file for the function to read
  dummy_df <- data.frame(site_id = "lake_1", med_Blue = 500)
  dummy_feather <- "tiny_dummy.feather"
  arrow::write_feather(dummy_df, file.path(input_dir, dummy_feather))


  # Scenario A: User provides a specific .feather file path
  custom_out_file <- file.path(tmp_base, "my_custom_name.feather")

  # Expect the cli_alert_success message
  expect_message(
    build_sr(
      which_sr = "lakeSR",
      sr_location = input_dir,
      sr_files = dummy_feather,
      algal_mask = FALSE,
      save = TRUE,
      save_location = custom_out_file
    ),
    regexp = "Saving SR file as"
  )

  # Assert the custom file was created exactly as named
  expect_true(file.exists(custom_out_file))

  # Scenario B: User provides a directory path
  expect_message(
    build_sr(
      which_sr = "lakeSR",
      sr_location = input_dir,
      sr_files = dummy_feather,
      algal_mask = FALSE,
      save = TRUE,
      # Passing the directory, not a file
      save_location = out_dir_fallback
    ),
    regexp = "Saving SR file as"
  )

  # Assert the standardized fallback name was generated inside the directory
  expected_fallback_file <- file.path(out_dir_fallback, "lakeSR_DSWE1_full_concatenation.feather")
  expect_true(file.exists(expected_fallback_file))

  # Scenario C: User provides a specific file path, but it lacks .feather
  bad_ext_file <- file.path(tmp_base, "wrong_extension.csv")

  # Expect the cli_alert_info message
  expect_message(
    build_sr(
      which_sr = "lakeSR",
      sr_location = input_dir,
      sr_files = dummy_feather,
      algal_mask = FALSE,
      save = TRUE,
      save_location = bad_ext_file
    ),
    regexp = "A non-feather file was indicated"
  )

  # Assert the standardized fallback name was generated in that target directory
  expected_emergency_file <- file.path(tmp_base, "lakeSR_DSWE1_full_concatenation.feather")
  expect_true(file.exists(expected_emergency_file))
})

test_that("match_siteSR_to_WQP calculates offsets, filters correctly, and validates extensions", {
  # Set up temporary directory and file paths
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  wqp_path <- file.path(tmp_dir, "wqp.feather")
  siteSR_path <- file.path(tmp_dir, "siteSR.feather")
  sitelist_path <- file.path(tmp_dir, "sitelist.csv")

  # Create the dummy data needed to pass the DuckDB joins
  # In-situ measurement at noon UTC time
  wqp_df <- data.frame(
    MonitoringLocationIdentifier = "LOC_1",
    harmonized_utc = "2020-01-05 12:00:00"
  )
  arrow::write_feather(wqp_df, wqp_path)

  siteSR_df <- data.frame(
    siteSR_id = "SR_1",
    date = "2020-01-05"
  )
  arrow::write_feather(siteSR_df, siteSR_path)

  # Site located at exactly -90 Longitude
  # Function uses: utc_seconds_offset = round((612 - (WGS84_Longitude * 4)) * 60)
  # Math is thus: 612 - (-90 * 4) = 972 minutes offset.
  # 972 mins = +16 hours and 12 minutes past midnight...
  # Landsat overpass time is calculated as 2020-01-05 16:12:00 UTC, meaning
  # difference from in-situ Noon = 4 hours and 12 minutes.
  sitelist_df <- data.frame(
    loc_id = "LOC_1",
    siteSR_id = "SR_1",
    WGS84_Longitude = -90
  )
  readr::write_csv(sitelist_df, sitelist_path)

  out_match <- file.path(tmp_dir, "match.parquet")
  out_nomatch <- file.path(tmp_dir, "nomatch.parquet")

  # Mock the strict schema checkers so the simplified dummy data is allowed
  mock_get_schema <- function(...) NULL
  mock_check_cols <- function(...) TRUE

  testthat::with_mocked_bindings(
    {
      # Scenario A: Bad output extension should fail
      # (This tests the end-of-script extension check safely)
      expect_error(
        match_siteSR_to_WQP(
          wqp_path = wqp_path,
          siteSR_path = siteSR_path,
          site_list_path = sitelist_path,
          save_location = "bad_output_name.csv",
          time_window = "5 hours"
        ),
        regexp = "Please supply a .*\\.parquet.* name"
      )

      # Scenario B: A 5-hour window should capture the ~4.2 hour difference
      expect_message(
        match_siteSR_to_WQP(
          wqp_path = wqp_path,
          siteSR_path = siteSR_path,
          site_list_path = sitelist_path,
          save_location = out_match,
          time_window = "5 hours"
        ),
        regexp = "Successfully wrote 1 matchups"
      )

      # Scenario C: A 2-hour window should fail to capture it
      expect_message(
        match_siteSR_to_WQP(
          wqp_path = wqp_path,
          siteSR_path = siteSR_path,
          site_list_path = sitelist_path,
          save_location = out_nomatch,
          time_window = "2 hours"
        ),
        regexp = "Successfully wrote 0 matchups"
      )
    },
    get_arrow_schema = mock_get_schema,
    check_cols = mock_check_cols
  )

  # Verify the DuckDB math inside the saved parquet file
  res_match <- arrow::read_parquet(out_match)
  expect_equal(res_match$time_diff[1], -0.175)

  # Verify Scenario C is actually empty
  res_nomatch <- arrow::read_parquet(out_nomatch)
  expect_equal(nrow(res_nomatch), 0)
})

test_that("match_siteSR_to_WQP fails if input files do not exist", {
  # Create actual temp files for the ones NOT being tested in each block,
  # so the function only fails on the specific file it's meant to fail on:
  valid_temp_feather <- tempfile(fileext = ".feather")
  valid_temp_csv <- tempfile(fileext = ".csv")
  file.create(valid_temp_feather, valid_temp_csv)
  on.exit(unlink(c(valid_temp_feather, valid_temp_csv)))

  # 1. Missing WQP file
  expect_error(
    match_siteSR_to_WQP(
      wqp_path = "does_not_exist.feather",
      siteSR_path = valid_temp_feather,
      site_list_path = valid_temp_csv,
      save_location = "out.parquet"
    ),
    regexp = "File not found at .*wqp_path.*"
  )

  # 2. Missing siteSR file
  expect_error(
    match_siteSR_to_WQP(
      wqp_path = valid_temp_csv,
      siteSR_path = "does_not_exist.feather",
      site_list_path = valid_temp_csv,
      save_location = "out.parquet"
    ),
    regexp = "File not found at .*siteSR_path.*"
  )

  # 3. Missing site list file
  expect_error(
    match_siteSR_to_WQP(
      wqp_path = valid_temp_csv,
      siteSR_path = valid_temp_feather,
      site_list_path = "does_not_exist.csv",
      save_location = "out.parquet"
    ),
    regexp = "File not found at .*site_list_path.*"
  )
})

test_that("apply_handoffs enforces argument validation", {
  # Correction method validation
  expect_error(
    apply_handoffs(
      input_path = "in.parquet",
      handoff_path = "out.csv",
      # Invalid
      correction_method = "Roy_magic",
      sat_target = "LS7",
      algal_mask = FALSE,
      save_location = "out.parquet"
    ),
    regexp = "Must be .*Roy_deming.*, .*Roy_lm.*, or .*Gardner_poly.*"
  )
  # Satellite target validation
  expect_error(
    apply_handoffs(
      input_path = "in.parquet",
      handoff_path = "out.csv",
      correction_method = "Roy_deming",
      # Invalid
      sat_target = "LS6",
      algal_mask = FALSE,
      save_location = "out.parquet"
    ),
    regexp = "Must be .*LS7.* or .*LS8.*"
  )
  # Output filetype validation
  expect_error(
    apply_handoffs(
      input_path = "in.parquet",
      handoff_path = "out.csv",
      correction_method = "Roy_deming",
      sat_target = "LS7",
      algal_mask = FALSE,
      save_location = "out.csv"
    ),
    regexp = "Please supply a .*\\.parquet.* name"
  )
})

test_that("apply_handoffs computes Roy linear math and flags extreme values", {
  # Set up temp files
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  in_path <- file.path(tmp_dir, "input.parquet")
  handoff_path <- file.path(tmp_dir, "handoffs.csv")
  out_path <- file.path(tmp_dir, "output.parquet")

  # Create dummy handoff data (Roy Deming, LS5 to LS7)
  # Math check: intercept(10) + slope(2) * input
  handoff_df <- data.frame(
    correction = "Roy",
    method = "deming",
    dswe = "DSWE1",
    sat_corr = "LS5",
    sat_to = "LS7",
    band = "med_Blue",
    intercept = 10,
    slope = 2,
    B1 = NA,
    B2 = NA,
    min_in_val = 100,
    max_in_val = 500
  )
  readr::write_csv(handoff_df, handoff_path)

  # Create dummy SR data
  # Row 1: LT04 (gets mapped to LS5). Valid range. 10 + 2(200) = 410.
  # Row 2: LT05 (gets mapped to LS5). Extreme value (600 > 500). 10 + 2(600) = 1210.
  # Row 3: LE07 (Self-target). Should bypass math and return original 300.
  input_df <- data.frame(
    mission = c("LT04", "LT05", "LE07"),
    med_Blue = c(200, 600, 300)
  )
  arrow::write_parquet(input_df, in_path)

  # Run function
  expect_message(
    apply_handoffs(
      input_path = in_path,
      handoff_path = handoff_path,
      correction_method = "Roy_deming",
      sat_target = "LS7",
      algal_mask = FALSE,
      save_location = out_path
    ),
    regexp = "Successfully wrote SR file"
  )

  # Results
  res <- arrow::read_parquet(out_path)

  # Check math and self-targeting logic
  expect_equal(res$blue_corr_7, c(410, 1210, 300))

  # Check extreme value flags
  expect_true(is.na(res$flag_blue_7[1]))
  expect_equal(res$flag_blue_7[2], "extreme value")
  expect_true(is.na(res$flag_blue_7[3]))
})

test_that("apply_handoffs computes Gardner polynomial math and handles missing bands", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  in_path <- file.path(tmp_dir, "input.parquet")
  handoff_path <- file.path(tmp_dir, "handoffs.csv")
  out_path <- file.path(tmp_dir, "output.parquet")

  # Create dummy handoff Data (Gardner Poly, LS8 to LS7)
  # Math check: intercept(5) + B1(2) * input + B2(0.5) * input^2
  handoff_df <- data.frame(
    correction = "Gardner",
    method = "poly",
    dswe = "DSWE1",
    sat_corr = "LS8",
    sat_to = "LS7",
    band = "med_Green",
    intercept = 5,
    slope = NA,
    B1 = 2,
    B2 = 0.5,
    min_in_val = 10,
    max_in_val = 50
  )
  readr::write_csv(handoff_df, handoff_path)

  # Create dummy SR data
  # Note: Intentionally omitting med_Blue, med_Red, etc. to test the warning block
  input_df <- data.frame(
    mission = c("LC08"),
    # Math: 5 + 2(20) + 0.5(20^2) = 5 + 40 + 200 = 245
    med_Green = c(20)
  )
  arrow::write_parquet(input_df, in_path)

  # Run function
  # We use expect_warning/message to catch the missing band alerts
  expect_message(
    apply_handoffs(
      input_path = in_path,
      handoff_path = handoff_path,
      correction_method = "Gardner_poly",
      sat_target = "LS7",
      algal_mask = FALSE,
      save_location = out_path
    ),
    regexp = "Expected column .*med_Blue.* is missing"
  )

  # Assert Results
  res <- arrow::read_parquet(out_path)
  expect_equal(res$green_corr_7, 245)
  expect_true(is.na(res$flag_green_7[1]))
})
