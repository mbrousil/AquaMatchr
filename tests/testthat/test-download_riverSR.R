library(testthat)

# Expected target files across tests
target_files <- c(
  "riverSR_usa_v1.1.feather",
  "nhdplusv2_modified_v1.0.shp",
  "nhdplusv2_modified_v1.0.shx",
  "nhdplusv2_modified_v1.0.dbf",
  "nhdplusv2_modified_v1.0.prj"
)

riverSR_doi <- "10.5281/zenodo.4304567"

# Fake MD5 checksums "reported by Zenodo", one per target file
fake_zenodo_checksums <- c(
  "riverSR_usa_v1.1.feather"    = "0f343b0931126a20f133d67c2b018a3b",
  "nhdplusv2_modified_v1.0.shp" = "6067e1795b88e18b6f21f250b151e2ab",
  "nhdplusv2_modified_v1.0.shx" = "84d9ee44e457aa335c28c1f7f214c4a1",
  "nhdplusv2_modified_v1.0.dbf" = "a3c65c2974270fd093ee8a9bf8ae7d0b",
  "nhdplusv2_modified_v1.0.prj" = "b7a56873cd771f2c00a3b9ca5eb7d0b3"
)

# Run code with the zen4R download mock and the AquaMatchr Zenodo listing mock
# in place
with_riverSR_mocks <- function(code, download_mock,
                               listing_mock = function(doi) fake_zenodo_checksums) {
  testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      code,
      download_zenodo = download_mock,
      .package = "zen4R"
    ),
    pull_zenodo_files = listing_mock,
    .package = "AquaMatchr"
  )
}

# Create a valid .zenodo.rds sidecar for a dummy local file, matching what the
# function itself writes after a successful download
write_valid_sidecar <- function(file_path, file_name,
                                checksum = fake_zenodo_checksums[[file_name]]) {
  saveRDS(
    list(
      entity_key = file_name,
      doi = riverSR_doi,
      remote_checksum = checksum,
      local_checksum = digest::digest(file_path, algo = "md5", file = TRUE),
      checked_at = Sys.time()
    ),
    file = paste0(file_path, ".zenodo.rds")
  )
}

test_that("download_riverSR creates directories and returns correct paths on fresh run", {
  # Setup nested temp folder to test directory creation
  tmp_parent <- tempfile()
  tmp_dir <- file.path(tmp_parent, "new_river_data")
  on.exit(unlink(tmp_parent, recursive = TRUE))

  # Mock a successful zen4R download
  mock_success <- function(path, ..., files) {
    file.create(file.path(path, files))
  }

  with_riverSR_mocks(
    {
      out <- download_riverSR(download_folder = tmp_dir)

      # Assert directory was created
      expect_true(dir.exists(tmp_dir))

      # Assert vector structure and named output
      expect_length(out, 5)
      expect_named(out, target_files)
      expect_true(all(file.exists(out)))

      # Assert sidecars were written next to each download with the checksums
      # Zenodo reported plus a checksum of the local file
      for (f in target_files) {
        sidecar <- paste0(out[[f]], ".zenodo.rds")
        expect_true(file.exists(sidecar))

        metadata <- readRDS(sidecar)
        expect_identical(metadata$entity_key, f)
        expect_identical(metadata$remote_checksum, fake_zenodo_checksums[[f]])
        expect_identical(
          metadata$local_checksum,
          digest::digest(out[[f]], algo = "md5", file = TRUE)
        )
      }
    },
    download_mock = mock_success
  )
})

test_that("download_riverSR reuses all existing files with update = 'never'", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Pre-create all files
  file.create(file.path(tmp_dir, target_files))

  with_riverSR_mocks(
    {
      expect_message(
        out <- download_riverSR(download_folder = tmp_dir, update = "never"),
        regexp = "Using current local file"
      )

      # Ensure named paths are still returned properly
      expect_named(out, target_files)
      expect_length(out, 5)
    },
    download_mock = function(...) stop("download_zenodo should not be called"),
    listing_mock = function(doi) stop("pull_zenodo_files should not be called")
  )
})

test_that("download_riverSR downloads only missing files with update = 'never'", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Pre-create a partial set of files
  kept_files <- c("riverSR_usa_v1.1.feather", "nhdplusv2_modified_v1.0.shp")
  file.create(file.path(tmp_dir, kept_files))

  requested_files <- NULL
  mock_download <- function(path, ..., files) {
    requested_files <<- files
    file.create(file.path(path, files))
  }

  with_riverSR_mocks(
    {
      out <- download_riverSR(download_folder = tmp_dir, update = "never")

      # Only the missing files were requested from Zenodo
      expect_setequal(requested_files, setdiff(target_files, kept_files))
      expect_true(all(file.exists(out)))

      # Sidecars are written for newly downloaded files only
      expect_true(file.exists(
        file.path(tmp_dir, "nhdplusv2_modified_v1.0.shx.zenodo.rds")
      ))
      expect_false(file.exists(
        file.path(tmp_dir, "riverSR_usa_v1.1.feather.zenodo.rds")
      ))
    },
    download_mock = mock_download
  )
})

test_that("download_riverSR replaces existing files with update = 'always'", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Pre-create all files
  file.create(file.path(tmp_dir, target_files))

  requested_files <- NULL
  mock_download <- function(path, ..., files) {
    requested_files <<- files
    file.create(file.path(path, files))
  }

  with_riverSR_mocks(
    {
      expect_message(
        download_riverSR(download_folder = tmp_dir, update = "always"),
        regexp = "need to be downloaded"
      )

      # All files were re-requested from Zenodo
      expect_setequal(requested_files, target_files)
    },
    download_mock = mock_download
  )
})

test_that("download_riverSR prompts the user with update = 'ask'", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Pre-create all files so that the prompt triggers
  file.create(file.path(tmp_dir, target_files))

  # User declines: the download is cancelled and nothing is touched
  testthat::with_mocked_bindings(
    {
      expect_error(
        download_riverSR(download_folder = tmp_dir),
        regexp = "Cancelled by user."
      )
    },
    get_user_input = function(prompt) "no",
    .package = "AquaMatchr"
  )
  expect_true(all(file.exists(file.path(tmp_dir, target_files))))

  # User accepts: all files are re-downloaded
  requested_files <- NULL
  mock_download <- function(path, ..., files) {
    requested_files <<- files
    file.create(file.path(path, files))
  }

  with_riverSR_mocks(
    testthat::with_mocked_bindings(
      {
        expect_message(
          download_riverSR(download_folder = tmp_dir),
          regexp = "Proceeding with download."
        )
        expect_setequal(requested_files, target_files)
      },
      get_user_input = function(prompt) "yes",
      .package = "AquaMatchr"
    ),
    download_mock = mock_download
  )
})

test_that("update = 'if_changed' reuses files with current sidecars", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Pre-create all files with sidecars that match the Zenodo listing
  for (f in target_files) {
    file_path <- file.path(tmp_dir, f)
    file.create(file_path)
    write_valid_sidecar(file_path, f)
  }

  with_riverSR_mocks(
    {
      expect_message(
        out <- download_riverSR(download_folder = tmp_dir, update = "if_changed"),
        regexp = "Using current local file"
      )
      expect_true(all(file.exists(out)))
    },
    download_mock = function(...) stop("download_zenodo should not be called")
  )
})

test_that("update = 'if_changed' re-downloads changed, corrupted, or missing files", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Start with all files present and current
  for (f in target_files) {
    file_path <- file.path(tmp_dir, f)
    file.create(file_path)
    write_valid_sidecar(file_path, f)
  }

  # Case 1: tamper with a local file so it no longer matches its sidecar
  cat("tampered", file = file.path(tmp_dir, "riverSR_usa_v1.1.feather"),
      append = TRUE)

  # Case 2: remove a sidecar so the file's currency can't be confirmed
  unlink(file.path(tmp_dir, "nhdplusv2_modified_v1.0.shp.zenodo.rds"))

  # Case 3: Zenodo reports a different checksum for a third file
  altered_listing <- fake_zenodo_checksums
  altered_listing[["nhdplusv2_modified_v1.0.shx"]] <-
    "ffffffffffffffffffffffffffffffff"

  # Case 4: a file is missing entirely (its stale sidecar remains)
  unlink(file.path(tmp_dir, "nhdplusv2_modified_v1.0.dbf"))

  expected_redownloads <- c(
    "riverSR_usa_v1.1.feather",
    "nhdplusv2_modified_v1.0.shp",
    "nhdplusv2_modified_v1.0.shx",
    "nhdplusv2_modified_v1.0.dbf"
  )

  requested_files <- NULL
  mock_download <- function(path, ..., files) {
    requested_files <<- files
    file.create(file.path(path, files))
  }

  with_riverSR_mocks(
    {
      out <- download_riverSR(download_folder = tmp_dir, update = "if_changed")

      expect_setequal(requested_files, expected_redownloads)
      expect_true(all(file.exists(out)))

      # The stale sidecar of the deleted file was replaced
      metadata <- readRDS(
        file.path(tmp_dir, "nhdplusv2_modified_v1.0.dbf.zenodo.rds")
      )
      expect_identical(
        metadata$local_checksum,
        digest::digest(
          file.path(tmp_dir, "nhdplusv2_modified_v1.0.dbf"),
          algo = "md5", file = TRUE
        )
      )
    },
    download_mock = mock_download,
    listing_mock = function(doi) altered_listing
  )
})

test_that("download_riverSR rejects invalid update values", {
  expect_error(
    download_riverSR(download_folder = tempfile(), update = "sometimes"),
    regexp = "must be one of"
  )
})

test_that("download_riverSR cleans up partial files on timeout without deleting kept files", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Simulate 1 pre-existing valid file, which update = "never" keeps
  pre_existing <- file.path(tmp_dir, "riverSR_usa_v1.1.feather")
  file.create(pre_existing)

  # Mock a download that creates a partial shapefile, then throws a timeout warning
  mock_timeout <- function(path, ..., files) {
    file.create(file.path(path, "nhdplusv2_modified_v1.0.shp"))
    warning("downloaded length 4012340 != reported length 132318322")
  }

  with_riverSR_mocks(
    {
      expect_error(
        download_riverSR(download_folder = tmp_dir, update = "never"),
        regexp = "download failed, timed out, or resulted in corrupted files"
      )
    },
    download_mock = mock_timeout
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
  mock_fatal <- function(path, ..., files) {
    file.create(file.path(path, "nhdplusv2_modified_v1.0.shp"))
    stop("Connection reset by peer")
  }

  with_riverSR_mocks(
    {
      expect_error(
        download_riverSR(download_folder = tmp_dir),
        regexp = "fatal error occurred during the download"
      )
    },
    download_mock = mock_fatal
  )

  # Assert partial file was removed on hard error
  expect_false(file.exists(file.path(tmp_dir, "nhdplusv2_modified_v1.0.shp")))
})
