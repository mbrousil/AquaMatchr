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
  sitelist_path <- testthat::test_path("testdata", "sitelist_snippet.csv")
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

test_that("infer_arrow_format() returns the format for supported extensions", {
  expect_equal(infer_arrow_format("data.csv"), "csv")
  expect_equal(infer_arrow_format("data.feather"), "feather")
  expect_equal(infer_arrow_format("data.parquet"), "parquet")
})

test_that("infer_arrow_format() is case-insensitive", {
  expect_equal(infer_arrow_format("DATA.PARQUET"), "parquet")
  expect_equal(infer_arrow_format("x.Feather"), "feather")
  expect_equal(infer_arrow_format("x.CSV"), "csv")
})

test_that("infer_arrow_format() ignores dots in directory names", {
  expect_equal(infer_arrow_format("some.dir/file.parquet"), "parquet")
  expect_equal(infer_arrow_format("data.v2/sub.dir/file.feather"), "feather")
})

test_that("infer_arrow_format() aborts on unsupported extensions", {
  expect_error(
    infer_arrow_format("x.txt"),
    regexp = "does not have a supported extension"
  )
  expect_error(
    infer_arrow_format("x.rds"),
    regexp = "does not have a supported extension"
  )
})

test_that("infer_arrow_format() aborts when the path has no extension", {
  expect_error(
    infer_arrow_format("data"),
    regexp = "does not have a supported extension"
  )
})

test_that("infer_arrow_format() respects the `allowed` argument", {
  # Recognized extension, but not allowed for this argument
  expect_error(
    infer_arrow_format("x.csv", allowed = c("feather", "parquet")),
    regexp = "unsupported format for this argument"
  )

  # Allowed formats still pass under the same restriction
  expect_equal(
    infer_arrow_format("x.parquet", allowed = c("feather", "parquet")),
    "parquet"
  )
  expect_equal(
    infer_arrow_format("x.feather", allowed = c("feather", "parquet")),
    "feather"
  )
})

# Update argument and sidecar machinery ------------------------------------

# Fixed remote checksum returned by the mocked EDI checksum endpoint
fake_edi_checksum <- "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0"

# Mock the EDI checksum endpoint. Must be called from a test body: the mocks
# are tied to the calling test via .env = parent.frame(), otherwise they would
# be torn down when this helper returns.
local_edi_checksum_mock <- function(checksum = fake_edi_checksum) {
  testthat::local_mocked_bindings(
    read_data_entity_checksum = function(...) checksum,
    .package = "EDIutils",
    .env = parent.frame()
  )
}

# Write a sidecar exactly as download_w_sidecar() would after a successful
# download
write_edi_sidecar <- function(download_filepath, package_id = "edi.mock.1",
                              entity_id = "mock_1",
                              entity_key = "mock entity",
                              remote_checksum = fake_edi_checksum) {
  saveRDS(
    list(
      entity_key = entity_key,
      package_id = package_id,
      entity_id = entity_id,
      remote_checksum = remote_checksum,
      local_checksum = generate_local_checksum(download_filepath),
      checked_at = Sys.time()
    ),
    file = paste0(download_filepath, ".edi.rds")
  )
}

# Run download_w_sidecar() with the standard mock arguments
run_sidecar <- function(download_filepath, update, download_to_path,
                        entity_key = "mock entity") {
  download_w_sidecar(
    download_filepath = download_filepath,
    update = update,
    package_id = "edi.mock.1",
    entity_id = "mock_1",
    entity_key = entity_key,
    download_to_path = download_to_path
  )
}

test_that("check_update_arg() accepts and normalizes valid options", {
  expect_equal(check_update_arg("ask"), "ask")
  expect_equal(check_update_arg("always"), "always")
  expect_equal(check_update_arg("never"), "never")
  expect_equal(check_update_arg("if_changed"), "if_changed")

  # Case and surrounding whitespace are tolerated
  expect_equal(check_update_arg("  IF_CHANGED "), "if_changed")
})

test_that("check_update_arg() aborts on invalid input", {
  expect_error(check_update_arg("sometimes"), regexp = "must be one of")
  expect_error(check_update_arg("alway"), regexp = "must be one of")
  expect_error(check_update_arg(1), regexp = "must be one of")
  expect_error(check_update_arg(NA_character_), regexp = "must be one of")
  expect_error(check_update_arg(c("ask", "always")), regexp = "must be one of")
})

test_that("pull_entity_id() returns the single matching entity ID", {
  testthat::local_mocked_bindings(
    read_data_entity_names = function(...) {
      data.frame(
        entityName = c("lakeSR sites list", "lakeSR handoff coefficients"),
        entityId = c("mock_1", "mock_2"),
        stringsAsFactors = FALSE
      )
    },
    .package = "EDIutils"
  )

  expect_equal(
    pull_entity_id(package_id = "edi.mock.1", entity_name = "lakeSR sites list"),
    "mock_1"
  )
})

test_that("pull_entity_id() aborts on zero or multiple matching entities", {
  testthat::local_mocked_bindings(
    read_data_entity_names = function(...) {
      data.frame(
        entityName = c("duplicate", "duplicate", "unrelated"),
        entityId = c("mock_1", "mock_2", "mock_3"),
        stringsAsFactors = FALSE
      )
    },
    .package = "EDIutils"
  )

  # No matching entities
  expect_error(
    pull_entity_id(package_id = "edi.mock.1", entity_name = "missing entity"),
    regexp = "Expected exactly one EDI entity"
  )
  # Two matching entities
  expect_error(
    pull_entity_id(package_id = "edi.mock.1", entity_name = "duplicate"),
    regexp = "Expected exactly one EDI entity"
  )
})

test_that("download_w_sidecar() rejects the 'ask' update option", {
  expect_error(
    run_sidecar(tempfile(), update = "ask", download_to_path = function(path) NULL),
    regexp = "must be one of 'always', 'never', or 'if_changed'"
  )
})

test_that("download_w_sidecar() with update = 'always' replaces an existing file", {
  tmp_dir <- withr::local_tempdir()
  out_path <- file.path(tmp_dir, "data.csv")
  writeLines("old contents", out_path)

  local_edi_checksum_mock()

  result <- run_sidecar(
    out_path, update = "always",
    download_to_path = function(path) writeLines("new contents", path)
  )

  expect_true(result$downloaded)
  expect_equal(result$path, out_path)
  expect_equal(readLines(out_path), "new contents")

  # The sidecar records the remote checksum and the new file's checksum
  metadata <- readRDS(paste0(out_path, ".edi.rds"))
  expect_equal(metadata$remote_checksum, fake_edi_checksum)
  expect_equal(metadata$local_checksum, generate_local_checksum(out_path))
})

test_that("download_w_sidecar() with update = 'never' reuses an existing file", {
  tmp_dir <- withr::local_tempdir()
  out_path <- file.path(tmp_dir, "data.csv")
  writeLines("current contents", out_path)

  result <- run_sidecar(
    out_path, update = "never",
    download_to_path = function(path) stop("download callback should not run")
  )

  expect_false(result$downloaded)
  expect_equal(readLines(out_path), "current contents")
  # No sidecar was needed or written for the reused file
  expect_false(file.exists(paste0(out_path, ".edi.rds")))
})

test_that("download_w_sidecar() downloads a missing file even in 'never' mode", {
  tmp_dir <- withr::local_tempdir()
  out_path <- file.path(tmp_dir, "data.csv")

  local_edi_checksum_mock()

  result <- run_sidecar(
    out_path, update = "never",
    download_to_path = function(path) writeLines("downloaded", path)
  )

  expect_true(result$downloaded)
  expect_equal(readLines(out_path), "downloaded")
  expect_true(file.exists(paste0(out_path, ".edi.rds")))
})

test_that("download_w_sidecar() with update = 'if_changed' reuses a current file", {
  tmp_dir <- withr::local_tempdir()
  out_path <- file.path(tmp_dir, "data.csv")
  writeLines("current contents", out_path)
  write_edi_sidecar(out_path)

  local_edi_checksum_mock()

  result <- run_sidecar(
    out_path, update = "if_changed",
    download_to_path = function(path) stop("download callback should not run")
  )

  expect_false(result$downloaded)
  expect_equal(readLines(out_path), "current contents")
})

test_that("download_w_sidecar() with update = 'if_changed' re-downloads stale files", {
  tmp_dir <- withr::local_tempdir()

  local_edi_checksum_mock()

  # Scenario 1: the local file was modified after its sidecar was written
  tampered <- file.path(tmp_dir, "tampered.csv")
  writeLines("original", tampered)
  write_edi_sidecar(tampered)
  cat("extra bytes", file = tampered, append = TRUE)

  result <- run_sidecar(
    tampered, update = "if_changed",
    download_to_path = function(path) writeLines("fresh", path)
  )
  expect_true(result$downloaded)
  expect_equal(readLines(tampered), "fresh")

  # Scenario 2: the sidecar is missing entirely
  no_sidecar <- file.path(tmp_dir, "no_sidecar.csv")
  writeLines("original", no_sidecar)

  result <- run_sidecar(
    no_sidecar, update = "if_changed",
    download_to_path = function(path) writeLines("fresh", path)
  )
  expect_true(result$downloaded)

  # Scenario 3: the remote checksum differs from the recorded one
  remote_changed <- file.path(tmp_dir, "remote_changed.csv")
  writeLines("original", remote_changed)
  write_edi_sidecar(remote_changed,
                    remote_checksum = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")

  result <- run_sidecar(
    remote_changed, update = "if_changed",
    download_to_path = function(path) writeLines("fresh", path)
  )
  expect_true(result$downloaded)
})

test_that("download_w_sidecar() leaves no partial file when the download is interrupted", {
  tmp_dir <- withr::local_tempdir()
  out_path <- file.path(tmp_dir, "data.csv")

  local_edi_checksum_mock()

  expect_error(
    run_sidecar(
      out_path, update = "always",
      download_to_path = function(path) {
        writeLines("partial download", path)
        stop("connection reset by peer")
      }
    ),
    regexp = "connection reset by peer"
  )

  # Neither the final file, temporary files, nor a sidecar remain
  expect_false(file.exists(out_path))
  expect_length(list.files(tmp_dir), 0)
})

test_that("download_w_sidecar() aborts when the download callback creates no file", {
  tmp_dir <- withr::local_tempdir()
  out_path <- file.path(tmp_dir, "data.csv")

  local_edi_checksum_mock()

  expect_error(
    run_sidecar(out_path, update = "always", download_to_path = function(path) NULL),
    regexp = "did not create a file"
  )

  expect_false(file.exists(out_path))
  expect_length(list.files(tmp_dir), 0)
})

test_that("sidecar_is_current() respects the sidecar suffix and checksum algorithm", {
  tmp_dir <- withr::local_tempdir()
  out_path <- file.path(tmp_dir, "riverSR_usa_v1.1.feather")
  file.create(out_path)

  # A Zenodo-style sidecar with an MD5 checksum
  saveRDS(
    list(
      entity_key = "riverSR_usa_v1.1.feather",
      remote_checksum = "0F343B0931126A20F133D67C2B018A3B",
      local_checksum = generate_local_checksum(out_path, algo = "md5")
    ),
    file = paste0(out_path, ".zenodo.rds")
  )

  # Matches despite the case difference in the recorded checksum
  expect_true(sidecar_is_current(
    download_filepath = out_path,
    entity_key = "riverSR_usa_v1.1.feather",
    remote_checksum = "0f343b0931126a20f133d67c2b018a3b",
    sidecar_suffix = ".zenodo.rds",
    algo = "md5"
  ))

  # The default .edi.rds suffix does not find the .zenodo.rds sidecar
  expect_false(sidecar_is_current(
    download_filepath = out_path,
    entity_key = "riverSR_usa_v1.1.feather",
    remote_checksum = "0f343b0931126a20f133d67c2b018a3b",
    algo = "md5"
  ))

  # A modified local file is detected
  cat("tampered", file = out_path, append = TRUE)
  expect_false(sidecar_is_current(
    download_filepath = out_path,
    entity_key = "riverSR_usa_v1.1.feather",
    remote_checksum = "0f343b0931126a20f133d67c2b018a3b",
    sidecar_suffix = ".zenodo.rds",
    algo = "md5"
  ))
})

test_that("pull_zenodo_files() returns lowercase MD5 checksums keyed by file name", {
  testthat::local_mocked_bindings(
    ZenodoManager = list(new = function() {
      list(getRecordByDOI = function(doi) {
        list(listFiles = function(pretty = TRUE) {
          data.frame(
            filename = c("riverSR_usa_v1.1.feather", "nhdplusv2_modified_v1.0.shp"),
            filesize = c(138, 1),
            checksum = c(
              "md5:0F343B0931126A20F133D67C2B018A3B",
              "md5:6067E1795B88E18B6F21F250B151E2AB"
            ),
            download = c("https://example.com/a", "https://example.com/b"),
            stringsAsFactors = FALSE
          )
        })
      })
    }),
    .package = "zen4R"
  )

  expect_equal(
    pull_zenodo_files(doi = "10.5281/zenodo.4304567"),
    c("riverSR_usa_v1.1.feather" = "0f343b0931126a20f133d67c2b018a3b",
      "nhdplusv2_modified_v1.0.shp" = "6067e1795b88e18b6f21f250b151e2ab")
  )
})
