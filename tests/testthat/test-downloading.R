test_that("IDs construct", {
  expect_equal(construct_id(identifier = 1756, version = 1), "edi.1756.1")
})
test_that("Quoted numbers work", {
  expect_equal(construct_id(identifier = "1756", version = "1"), "edi.1756.1")
})
test_that("Nonexistent versions fail", {
  expect_error(construct_id(identifier = 1756, version = 20))
})
test_that("Unexpected identifiers fail ID construction", {
  expect_error(construct_id(identifier = "words", version = "newest"))
})
test_that("Unexpected parameters fail", {
  # Requires "chla" instead of "chlorophyll"
  expect_error(
    download_parameters(parameters = c("chlorophyll"), version = "newest"),
    regexp = "The provided input for the parameters argument does not match"
  )
})
test_that("Multiple (mocked) parameters work", {
  # Mock internal auth helper so it passes
  testthat::local_mocked_bindings(
    check_edi_auth = function() TRUE
  )

  # Mock the EDIutils functions to intercept the web requests
  # Provide fake citation text, a fake metadata table, and a tiny raw CSV.
  testthat::local_mocked_bindings(
    list_data_package_revisions = function(...) "1",
    read_data_package_citation = function(...) "Mock Citation",
    read_data_entity_names = function(...) {
      data.frame(
        entityName = c("chla_harmonized_final", "tss_harmonized_final"),
        entityId = c("mock_1", "mock_2"),
        stringsAsFactors = FALSE
      )
    },
    read_data_entity = function(...) {
      # Return raw bytes of a tiny CSV so read_csv() has something to parse
      charToRaw("parameter,harmonized_value\nmock_param,1.5\n")
    },
    .package = "EDIutils"
  )

  # Run the function
  result <- download_parameters(parameters = c("chla", "tss"), version = "newest")

  # Verify that the function successfully iterated over both parameters
  # and returned a list containing both dfs
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(c("chla", "tss") %in% names(result)))
  expect_s3_class(result$chla, "data.frame")
  expect_s3_class(result$tss, "data.frame")
})

test_that("download_parameters('cdom') successfully downloads and parses 35MB dataset", {
  # Skip on CRAN
  testthat::skip_on_cran()

  # Check for authentication keys in the environment
  has_key <- Sys.getenv("EDI_API_KEY") != "" ||
    Sys.getenv("EDI_TOKEN") != "" ||
    Sys.getenv("AUTH_TOKEN") != ""

  # Skip gracefully if no credentials exist (prevents GitHub Actions failures if secret is missing)
  testthat::skip_if_not(
    condition = has_key,
    message = "EDI API credentials not found. Skipping live EDI integration test."
  )

  # Extend the default timeout to 5 minutes
  old_timeout <- getOption("timeout")
  options(timeout = max(300, old_timeout))
  on.exit(options(timeout = old_timeout))

  # Perform small live data pull
  result <- download_parameters(parameters = "cdom", version = "newest")

  # Verify the resulting object structure
  expect_type(result, "list")
  expect_true("cdom" %in% names(result))
  expect_s3_class(result$cdom, "data.frame")
  expect_gt(nrow(result$cdom), 0)
})

test_that("download_siteSR aborts when user declines to overwrite files", {
  # Create temporary directory
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create a dummy file that matches a standard output name to trigger the prompt
  dummy_file <- file.path(tmp_dir, "siteSR_Landsat4_DSWE1_2025-06-06.feather")
  file.create(dummy_file)

  # Mock ask_user to simulate the user typing "no"
  testthat::with_mocked_bindings(
    {
      expect_error(
        download_siteSR(save_location = tmp_dir, algal_mask = FALSE, ask = TRUE),
        regexp = "Cancelled by user."
      )
    },
    ask_user = function(...) "no"
  )
})


test_that("download_siteSR proceeds when user agrees to overwrite files", {
  # Create temporary directory
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  dummy_file <- file.path(tmp_dir, "siteSR_Landsat4_DSWE1_2025-06-06.feather")
  file.create(dummy_file)

  # Mock internal functions (no .package needed; defaults to AquaMatchr)
  testthat::local_mocked_bindings(
    ask_user = function(...) "yes",
    construct_id = function(...) "edi.mock.1"
  )

  # Mock external EDIutils functions
  testthat::local_mocked_bindings(
    read_data_entity_names = function(...) {
      data.frame(
        entityName = c(
          "siteSR data from Landsat 4, DSWE filter for confident water (DSWE = 1)",
          "siteSR sites list",
          "lakeSR handoff coefficients"
        ),
        entityId = c("mock_1", "mock_2", "mock_3"),
        stringsAsFactors = FALSE
      )
    },
    read_data_package_citation = function(...) "Mock Citation",
    .package = "EDIutils"
  )

  # Mock file parsers/writers to prevent crashes from fake raw bytes
  # Since we are faking the EDI downloads, actual read/write functions
  # will crash if they try to parse our fake data.
  testthat::local_mocked_bindings(
    read_data_entity = function(...) raw(0),
    .package = "EDIutils"
  )
  testthat::local_mocked_bindings(
    read_feather = function(...) data.frame(a = 1),
    write_feather = function(...) TRUE,
    .package = "arrow"
  )
  testthat::local_mocked_bindings(
    read_csv = function(...) data.frame(a = 1),
    write_csv = function(...) TRUE,
    .package = "readr"
  )

  # Expect the informational message confirming the download is proceeding
  expect_message(
    result <- download_siteSR(save_location = tmp_dir, algal_mask = FALSE, ask = TRUE),
    regexp = "Proceeding with download."
  )

  # Assert the function completed and returned paths
  expect_type(result, "character")
})


test_that("download_lakeSR aborts when user declines to overwrite files", {
  # Create temporary directory
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create a dummy file that matches a standard output name to trigger the prompt
  dummy_file <- file.path(tmp_dir, "lakeSR_Landsat4_DSWE1_2025-06-04.feather")
  file.create(dummy_file)

  # Mock ask_user to simulate the user typing "no"
  testthat::with_mocked_bindings(
    {
      expect_error(
        download_lakeSR(save_location = tmp_dir, algal_mask = FALSE, ask = TRUE),
        regexp = "Cancelled by user."
      )
    },
    ask_user = function(...) "no"
  )
})

test_that("download_lakeSR proceeds when user agrees to overwrite files", {
  # Create temporary directory
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create a dummy file that matches a standard output name
  dummy_file <- file.path(tmp_dir, "lakeSR_Landsat4_DSWE1_2025-06-04.feather")
  file.create(dummy_file)

  # Mock internal functions
  testthat::local_mocked_bindings(
    ask_user = function(...) "yes",
    construct_id = function(...) "edi.mock.2"
  )

  # Mock external EDIutils functions using lakeSR specific names
  testthat::local_mocked_bindings(
    read_data_entity_names = function(...) {
      data.frame(
        entityName = c(
          "lakeSR data from Landsat 4, DSWE filter for confident water (DSWE = 1)",
          "lakeSR sites list",
          "lakeSR handoff coefficients"
        ),
        entityId = c("mock_1", "mock_2", "mock_3"),
        stringsAsFactors = FALSE
      )
    },
    read_data_package_citation = function(...) "Mock lakeSR Citation",
    .package = "EDIutils"
  )

  # Mock file parsers/writers to prevent crashes from fake raw bytes
  testthat::local_mocked_bindings(
    read_data_entity = function(...) raw(0),
    .package = "EDIutils"
  )
  testthat::local_mocked_bindings(
    read_feather = function(...) data.frame(a = 1),
    write_feather = function(...) TRUE,
    .package = "arrow"
  )
  testthat::local_mocked_bindings(
    read_csv = function(...) data.frame(a = 1),
    write_csv = function(...) TRUE,
    .package = "readr"
  )

  # Expect the informational message confirming the download is proceeding
  expect_message(
    result <- download_lakeSR(save_location = tmp_dir, algal_mask = FALSE, ask = TRUE),
    regexp = "Proceeding with download."
  )

  # Assert the function completed and returned paths
  expect_type(result, "character")
})


