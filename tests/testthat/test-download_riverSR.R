library(testthat)

# Expected target files across tests
target_files <- c(
  "riverSR_usa_v1.1.feather",
  "nhdplusv2_modified_v1.0.shp",
  "nhdplusv2_modified_v1.0.shx",
  "nhdplusv2_modified_v1.0.dbf",
  "nhdplusv2_modified_v1.0.prj"
)

test_that("download_riverSR creates directories and returns correct paths on fresh run", {

  # Setup nested temp folder to test directory creation
  tmp_parent <- tempfile()
  tmp_dir <- file.path(tmp_parent, "new_river_data")
  on.exit(unlink(tmp_parent, recursive = TRUE))

  # Mock a successful zen4R download
  mock_success <- function(path, ...) {
    file.create(file.path(path, target_files))
  }

  testthat::with_mocked_bindings(
    {
      out <- download_riverSR(save_location = tmp_dir)

      # Assert directory was created
      expect_true(dir.exists(tmp_dir))

      # Assert vector structure and named output
      expect_length(out, 5)
      expect_named(out, target_files)
      expect_true(all(file.exists(out)))
    },
    download_zenodo = mock_success,
    .package = "zen4R"
  )
})

test_that("download_riverSR skips download when all files already exist (force = FALSE)", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Pre-create all files
  file.create(file.path(tmp_dir, target_files))

  # Verify early return message
  expect_message(
    out <- download_riverSR(save_location = tmp_dir, force = FALSE),
    regexp = "All files already exist"
  )

  # Ensure named paths are still returned properly
  expect_named(out, target_files)
  expect_length(out, 5)
})

test_that("download_riverSR bypasses existing files when force = TRUE", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Pre-create all files
  file.create(file.path(tmp_dir, target_files))

  download_triggered <- FALSE
  mock_force <- function(path, ...) {
    download_triggered <<- TRUE
    file.create(file.path(path, target_files))
  }

  testthat::with_mocked_bindings(
    {
      expect_message(
        download_riverSR(save_location = tmp_dir, force = TRUE),
        regexp = "Force override enabled"
      )
      # Assert that zen4R was actually called
      expect_true(download_triggered)
    },
    download_zenodo = mock_force,
    .package = "zen4R"
  )
})

test_that("download_riverSR cleans up partial files on timeout without deleting pre-existing files", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Simulate 1 pre-existing valid file
  pre_existing <- file.path(tmp_dir, "riverSR_usa_v1.1.feather")
  file.create(pre_existing)

  # Mock a download that creates a partial shapefile, then throws a timeout warning
  mock_timeout <- function(path, ...) {
    file.create(file.path(path, "nhdplusv2_modified_v1.0.shp"))
    warning("downloaded length 4012340 != reported length 132318322")
  }

  testthat::with_mocked_bindings(
    {
      expect_error(
        download_riverSR(save_location = tmp_dir),
        regexp = "download failed, timed out, or resulted in corrupted files"
      )
    },
    download_zenodo = mock_timeout,
    .package = "zen4R"
  )

  # CRITICAL ASSERTS:
  # The pre-existing file should STILL exist
  expect_true(file.exists(pre_existing))
  # The newly created partial file should be DELETED
  expect_false(file.exists(file.path(tmp_dir, "nhdplusv2_modified_v1.0.shp")))
})

test_that("download_riverSR handles hard errors and cleans up partial files", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Mock a fatal network crash after starting a download
  mock_fatal <- function(path, ...) {
    file.create(file.path(path, "nhdplusv2_modified_v1.0.shp"))
    stop("Connection reset by peer")
  }

  testthat::with_mocked_bindings(
    {
      expect_error(
        download_riverSR(save_location = tmp_dir),
        regexp = "fatal error occurred during the download"
      )
    },
    download_zenodo = mock_fatal,
    .package = "zen4R"
  )

  # Assert partial file was removed on hard error
  expect_false(file.exists(file.path(tmp_dir, "nhdplusv2_modified_v1.0.shp")))
})
