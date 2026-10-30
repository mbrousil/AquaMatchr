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
    check_edi_auth = function() TRUE,
    .package = "AquaMatchr"
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

# Build a mock EDI entity table for the SR download tests: `n_landsat` Landsat
# data entities (DSWE = 1 or DSWE = 1a) plus any auxiliary entities such as
# the sites list
mock_sr_entities <- function(prefix, extra_entities = character(),
                             n_landsat = 5, dswe = c("1", "1a")) {
  dswe <- match.arg(dswe)
  dswe_suffix <- if (dswe == "1") {
    "DSWE filter for confident water (DSWE = 1)"
  } else {
    "DSWE filter for confident water and algal mask (DSWE = 1a)"
  }
  landsat_names <- paste0(
    prefix, " data from Landsat ", c(4, 5, 7, 8, 9)[seq_len(n_landsat)],
    ", ", dswe_suffix
  )
  entity_names <- c(landsat_names, extra_entities)

  data.frame(
    entityName = entity_names,
    entityId = paste0("mock_", seq_along(entity_names)),
    stringsAsFactors = FALSE
  )
}

# Mock the pieces that the sidecar machinery and mocked downloads need:
# a fixed checksum from EDI, byte "downloads" that return empty raw vectors,
# and writer mocks that actually create files, because download_w_sidecar()
# aborts when the download callback does not produce a file
local_sr_download_mocks <- function(entity_table) {
  # local_mocked_bindings() registers its cleanup on the calling frame, so
  # from inside this helper the mocks must be tied to the test body frame
  # (.env = parent.frame()); otherwise they are torn down when the helper
  # returns.
  testthat::local_mocked_bindings(
    ask_user = function(...) "yes",
    construct_id = function(...) "edi.mock.1",
    .package = "AquaMatchr",
    .env = parent.frame()
  )

  testthat::local_mocked_bindings(
    read_data_entity_names = function(...) entity_table,
    read_data_package_citation = function(...) "Mock Citation",
    read_data_entity_checksum = function(...) {
      "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0"
    },
    read_data_entity = function(...) raw(0),
    .package = "EDIutils",
    .env = parent.frame()
  )

  # Mock file parsers/writers to prevent crashes from fake raw bytes
  testthat::local_mocked_bindings(
    read_feather = function(...) data.frame(a = 1),
    write_feather = function(x, sink, ...) file.create(sink),
    .package = "arrow",
    .env = parent.frame()
  )
  testthat::local_mocked_bindings(
    read_csv = function(...) data.frame(a = 1),
    write_csv = function(x, file, ...) file.create(file),
    .package = "readr",
    .env = parent.frame()
  )
}

test_that("download_siteSR aborts when user declines to overwrite files", {
  # Create temporary directory
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create a dummy file that matches a standard output name to trigger the prompt
  dummy_file <- file.path(tmp_dir, "siteSR_Landsat4_DSWE1.feather")
  file.create(dummy_file)

  # Mock ask_user to simulate the user typing "no". Mock construct_id to
  # skip check_edi_auth(), which would otherwise fail offline before the
  # function reaches the overwrite prompt.
  testthat::with_mocked_bindings(
    {
      expect_error(
        download_siteSR(download_folder = tmp_dir, algal_mask = FALSE,
                        update = "ask"),
        regexp = "Cancelled by user."
      )
    },
    ask_user = function(...) "no",
    construct_id = function(...) "edi.mock.1",
    .package = "AquaMatchr"
  )
})


test_that("download_siteSR proceeds when user agrees to overwrite files", {
  # Create temporary directory
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  dummy_file <- file.path(tmp_dir, "siteSR_Landsat4_DSWE1.feather")
  file.create(dummy_file)

  local_sr_download_mocks(
    entity_table = mock_sr_entities(
      prefix = "siteSR",
      extra_entities = c("siteSR sites list", "lakeSR handoff coefficients")
    )
  )

  # Expect the informational message confirming the download is proceeding
  expect_message(
    result <- download_siteSR(download_folder = tmp_dir, algal_mask = FALSE,
                             update = "ask"),
    regexp = "Proceeding with download."
  )

  # Assert the function completed and returned all seven paths
  expect_type(result, "character")
  expect_length(result, 7)
  expect_true(all(file.exists(result)))
})

test_that("download_siteSR downloads algal mask (DSWE1a) files when requested", {
  tmp_dir <- withr::local_tempdir()

  # Pre-create one DSWE1a file to trigger (and accept) the overwrite prompt
  file.create(file.path(tmp_dir, "siteSR_Landsat4_DSWE1a.feather"))

  local_sr_download_mocks(
    entity_table = mock_sr_entities(
      prefix = "siteSR",
      extra_entities = c("siteSR sites list", "lakeSR handoff coefficients"),
      dswe = "1a"
    )
  )

  # The prompt fires because a DSWE1a file already exists; ask_user is mocked
  # to "yes" in local_sr_download_mocks()
  expect_message(
    result <- download_siteSR(download_folder = tmp_dir, algal_mask = TRUE,
                             update = "ask"),
    regexp = "Proceeding with download."
  )

  # All seven paths returned, under the DSWE1a filenames
  expect_type(result, "character")
  expect_length(result, 7)
  expect_true(all(file.exists(result)))
  expect_true(all(c(
    "siteSR_Landsat4_DSWE1a.feather",
    "siteSR_Landsat9_DSWE1a.feather"
  ) %in% names(result)))

  # Sidecars were written for the DSWE1a data files
  expect_true(all(file.exists(paste0(
    result[c("siteSR_Landsat4_DSWE1a.feather", "siteSR_Landsat9_DSWE1a.feather")],
    ".edi.rds"
  ))))
})

test_that("download_siteSR aborts when EDI does not return five unique data entities", {
  tmp_dir <- withr::local_tempdir()

  # Mock construct_id to skip check_edi_auth(), and return only four Landsat
  # entities so the entity count validation fails
  testthat::local_mocked_bindings(
    construct_id = function(...) "edi.mock.1",
    .package = "AquaMatchr"
  )
  testthat::local_mocked_bindings(
    read_data_entity_names = function(...) {
      mock_sr_entities(prefix = "siteSR", n_landsat = 4)
    },
    .package = "EDIutils"
  )

  expect_error(
    download_siteSR(download_folder = tmp_dir, algal_mask = FALSE,
                   update = "never"),
    regexp = "Expected 5 unique siteSR data entities"
  )
})

test_that("download_lakeSR aborts when user declines to overwrite files", {
  # Create temporary directory
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create a dummy file that matches a standard output name to trigger the prompt
  dummy_file <- file.path(tmp_dir, "lakeSR_Landsat4_DSWE1.feather")
  file.create(dummy_file)

  # Mock ask_user to simulate the user typing "no". Mock construct_id to
  # skip check_edi_auth(), which would otherwise fail offline before the
  # function reaches the overwrite prompt.
  testthat::with_mocked_bindings(
    {
      expect_error(
        download_lakeSR(download_folder = tmp_dir, algal_mask = FALSE,
                        update = "ask"),
        regexp = "Cancelled by user."
      )
    },
    ask_user = function(...) "no",
    construct_id = function(...) "edi.mock.2",
    .package = "AquaMatchr"
  )
})

test_that("download_lakeSR proceeds when user agrees to overwrite files", {
  # Create temporary directory
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create a dummy file that matches a standard output name
  dummy_file <- file.path(tmp_dir, "lakeSR_Landsat4_DSWE1.feather")
  file.create(dummy_file)

  local_sr_download_mocks(
    entity_table = mock_sr_entities(
      prefix = "lakeSR",
      extra_entities = c("lakeSR sites list", "lakeSR handoff coefficients")
    )
  )

  # Expect the informational message confirming the download is proceeding
  expect_message(
    result <- download_lakeSR(download_folder = tmp_dir, algal_mask = FALSE,
                             update = "ask"),
    regexp = "Proceeding with download."
  )

  # Assert the function completed and returned all seven paths
  expect_type(result, "character")
  expect_length(result, 7)
  expect_true(all(file.exists(result)))
})

test_that("download_lakeSR aborts when EDI does not return five unique data entities", {
  tmp_dir <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    construct_id = function(...) "edi.mock.2",
    .package = "AquaMatchr"
  )
  testthat::local_mocked_bindings(
    read_data_entity_names = function(...) {
      mock_sr_entities(prefix = "lakeSR", n_landsat = 4)
    },
    .package = "EDIutils"
  )

  expect_error(
    download_lakeSR(download_folder = tmp_dir, algal_mask = FALSE,
                   update = "never"),
    regexp = "Expected 5 unique lakeSR data entities"
  )
})

test_that("download_lakeSR downloads algal mask (DSWE1a) files when requested", {
  tmp_dir <- withr::local_tempdir()

  # Pre-create one DSWE1a file to trigger (and accept) the overwrite prompt
  file.create(file.path(tmp_dir, "lakeSR_Landsat4_DSWE1a.feather"))

  local_sr_download_mocks(
    entity_table = mock_sr_entities(
      prefix = "lakeSR",
      extra_entities = c("lakeSR sites list", "lakeSR handoff coefficients"),
      dswe = "1a"
    )
  )

  expect_message(
    result <- download_lakeSR(download_folder = tmp_dir, algal_mask = TRUE,
                             update = "ask"),
    regexp = "Proceeding with download."
  )

  # All seven paths returned, under the DSWE1a filenames
  expect_type(result, "character")
  expect_length(result, 7)
  expect_true(all(file.exists(result)))
  expect_true(all(c(
    "lakeSR_Landsat4_DSWE1a.feather",
    "lakeSR_Landsat9_DSWE1a.feather"
  ) %in% names(result)))

  # Sidecars were written for the DSWE1a data files
  expect_true(all(file.exists(paste0(
    result[c("lakeSR_Landsat4_DSWE1a.feather", "lakeSR_Landsat9_DSWE1a.feather")],
    ".edi.rds"
  ))))
})

test_that("download_sceneMetadata rejects an invalid product", {
  expect_error(
    download_sceneMetadata(download_folder = tempdir(), product = "riverSR")
  )
})

test_that("download_sceneMetadata aborts when user declines to overwrite files", {
  tmp <- withr::local_tempdir()

  # Create a file matching a standard output name to trigger the prompt
  file.create(file.path(tmp, "sceneMetadata_Landsat457.csv"))

  # Mock ask_user to simulate the user typing "no". Mock construct_id to
  # skip check_edi_auth(), which would otherwise fail offline before the
  # function reaches the overwrite prompt.
  testthat::local_mocked_bindings(
    ask_user = function(...) "no",
    construct_id = function(...) "edi.mock.1",
    .package = "AquaMatchr"
  )

  expect_error(
    download_sceneMetadata(download_folder = tmp, product = "siteSR",
                           update = "ask"),
    regexp = "Cancelled by user."
  )
})

test_that("download_sceneMetadata downloads and writes both scene metadata files (mocked)", {
  # Mock internal auth helper so it passes
  testthat::local_mocked_bindings(
    check_edi_auth = function() TRUE,
    .package = "AquaMatchr"
  )

  tmp <- withr::local_tempdir()

  # Mock the EDIutils functions to intercept the web requests. Each entity
  # gets distinct fake content so we can confirm the right bytes land in the
  # right output file. The checksum endpoint is needed by the sidecar
  # machinery.
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
    read_data_entity_checksum = function(...) {
      "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0"
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
  result <- download_sceneMetadata(download_folder = tmp, product = "siteSR",
                                  update = "always")

  # Verify both files were written with the expected names and content
  expect_type(result, "character")
  expect_length(result, 2)
  expect_true(all(file.exists(result)))
  expect_true(all(
    c("sceneMetadata_Landsat457.csv", "sceneMetadata_Landsat89.csv") %in% names(result)
  ))

  # Verify sidecars were written next to each file
  expect_true(all(file.exists(paste0(result, ".edi.rds"))))

  ls457 <- readr::read_csv(result[["sceneMetadata_Landsat457.csv"]], show_col_types = FALSE)
  ls89 <- readr::read_csv(result[["sceneMetadata_Landsat89.csv"]], show_col_types = FALSE)

  expect_true("IMAGE_QUALITY" %in% names(ls457))
  expect_true("IMAGE_QUALITY_OLI" %in% names(ls89))
  expect_equal(ls457$sat_id, "1_2_LT05_003048_19841111")
  expect_equal(ls89$sat_id, "1_LC08_003048_20130519")
})
