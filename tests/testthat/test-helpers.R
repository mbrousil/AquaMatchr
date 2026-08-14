test_that("ask_user() rejects invalid input and retries", {
  # Built-in helper for sequential mock returns
  testthat::with_mocked_bindings(
    get_user_input = testthat::mock_output_sequence("not sure", "yes"),
    code = {
      expect_message(
        final_answer <- ask_user(
          algal_mask = FALSE,
          which_sr = "lakeSR",
          file_message = ""
        ),
        regexp = "Invalid input"
      )
      expect_equal(final_answer, "yes")
    }
  )
})

test_that("ask_user() aborts on invalid which_sr", {

  # Top-level parameter validation
  expect_error(
    ask_user(
      algal_mask = FALSE,
      which_sr = "oceanSR",
      file_message = ""
    ),
    regexp = "is not valid"
  )

})

test_that("ask_user() generic mode uses custom file message", {
  testthat::with_mocked_bindings(
    get_user_input = testthat::mock_output_sequence("yes"),
    code = {
      # The generic mode should add "my_custom_data" to the prompt
      expect_message(
        ask_user(
          algal_mask = FALSE,
          which_sr = "generic",
          file_message = "my_custom_data"
        ),
        regexp = "my_custom_data"
      )
    }
  )
})

test_that("check_edi_auth throws error when unauthenticated", {
  # Temporarily clear all possible EDI environment variables for this test block
  withr::with_envvar(
    new = c("EDI_API_KEY" = "", "EDI_TOKEN" = "", "AUTH_TOKEN" = ""),
    {
      expect_error(
        check_edi_auth(),
        regexp = "Authentication is required to query or download data from EDI."
      )
    }
  )
})

test_that("check_edi_auth succeeds when EDI_API_KEY is present", {
  # Temporarily set a mock API key
  withr::with_envvar(
    new = c("EDI_API_KEY" = "fake_test_key_12345"),
    {
      expect_true(check_edi_auth())
    }
  )
})

test_that("check_time_window() rejects invalid strings and units", {
  # Bad formats or unsupported units
  expect_error(check_time_window("five days"), regexp = "Invalid")
  expect_error(check_time_window("5 moments"), regexp = "Invalid")
  expect_error(check_time_window("days 5"), regexp = "Invalid")

  # Confirm expected correct behavior
  expect_no_error(check_time_window("5 days"))
})

test_that("check_cols() identifies exact missing columns dynamically with real schemas", {

  # Static snippet files
  sitelist_path <- testthat::test_path("testdata", "sitelist_2025-06-04_snippet.csv")
  sitesr_path   <- testthat::test_path("testdata", "siteSR_DSWE1_full_concatenation_snippet.parquet")
  wqp_path      <- testthat::test_path("testdata", "chla_harmonized_snippet.feather")

  # Read in files
  sitelist_data <- arrow::read_csv_arrow(sitelist_path)
  sitesr_data   <- arrow::read_parquet(sitesr_path)
  wqp_data      <- arrow::read_feather(wqp_path)

  # Retrieve target schemas
  sitelist_schema <- get_arrow_schema("sitelist")
  sitesr_schema   <- get_arrow_schema("siteSR")
  wqp_schema      <- get_arrow_schema("wqp")

  # Confirm success: unmodified files should pass
  expect_no_error(check_cols(sitelist_data, sitelist_schema, "Sitelist File"))
  expect_no_error(check_cols(sitesr_data, sitesr_schema, "SiteSR File"))
  expect_no_error(check_cols(wqp_data, wqp_schema, "WQP File"))

  # Confirm failure: modifications should fail
  # Sitelist check: Drop 'siteSR_id' and 'org_id'
  sitelist_bad <- sitelist_data[, !(names(sitelist_data) %in% c("siteSR_id", "org_id"))]

  expect_error(
    check_cols(sitelist_bad, sitelist_schema, "Sitelist File"),
    # Verify at least one of the exact column names is dynamically printed
    regexp = "siteSR_id"
  )

  # SiteSR check: Drop 'date' and 'med_Blue'
  sitesr_bad <- sitesr_data[, !(names(sitesr_data) %in% c("date", "med_Blue"))]

  expect_error(
    check_cols(sitesr_bad, sitesr_schema, "SiteSR File"),
    regexp = "med_Blue"
  )

  # WQP check: Drop 'harmonized_value'
  wqp_bad <- wqp_data[, !(names(wqp_data) %in% "harmonized_value")]

  expect_error(
    check_cols(wqp_bad, wqp_schema, "WQP File"),
    regexp = "harmonized_value"
  )

})
