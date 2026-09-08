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

test_that("download_sceneMetadata rejects an invalid product", {
  expect_error(
    download_sceneMetadata(save_location = tempdir(), product = "riverSR")
  )
})

test_that("download_sceneMetadata downloads and writes both scene metadata files (mocked)", {
  # Mock internal auth helper so it passes
  testthat::local_mocked_bindings(
    check_edi_auth = function() TRUE
  )

  tmp <- withr::local_tempdir()

  # Mock the EDIutils functions to intercept the web requests. Each entity
  # gets distinct fake content so we can confirm the right bytes land in the
  # right output file.
  testthat::local_mocked_bindings(
    list_data_package_revisions = function(...) "1",
    read_data_package_citation = function(...) "Mock Citation",
    read_data_entity_names = function(...) {
      data.frame(
        entityName = c(
          "reduced column scene-level metadata for Landsat 4, 5, and 7",
          "reduced column scene-level metadata for Landsat 8 and 9"
        ),
        entityId = c("mock_457", "mock_89"),
        stringsAsFactors = FALSE
      )
    },
    read_data_entity = function(packageId, entityId, ...) {
      if (entityId == "mock_457") {
        charToRaw("sat_id,IMAGE_QUALITY\n1_2_LT05_003048_19841111,9\n")
      } else {
        charToRaw("sat_id,IMAGE_QUALITY_OLI\n1_LC08_003048_20130519,9\n")
      }
    },
    .package = "EDIutils"
  )

  # Run the function
  result <- download_sceneMetadata(save_location = tmp, product = "siteSR", ask = FALSE)

  # Verify both files were written with the expected names and content
  expect_type(result, "character")
  expect_length(result, 2)
  expect_true(all(file.exists(result)))
  expect_true(all(
    c("sceneMetadata_Landsat457.csv", "sceneMetadata_Landsat89.csv") %in% names(result)
  ))

  ls457 <- readr::read_csv(result[["sceneMetadata_Landsat457.csv"]], show_col_types = FALSE)
  ls89 <- readr::read_csv(result[["sceneMetadata_Landsat89.csv"]], show_col_types = FALSE)

  expect_true("IMAGE_QUALITY" %in% names(ls457))
  expect_true("IMAGE_QUALITY_OLI" %in% names(ls89))
  expect_equal(ls457$sat_id, "1_2_LT05_003048_19841111")
  expect_equal(ls89$sat_id, "1_LC08_003048_20130519")
})
