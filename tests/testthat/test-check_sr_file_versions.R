# Write a sidecar matching what the download functions write after a
# successful download. The data file itself does not need to exist. Named
# write_fake_sidecar so it can't mask a package function of the same name.
write_fake_sidecar <- function(folder, file_name, metadata,
                               suffix = ".edi.rds") {
  saveRDS(metadata, file = file.path(folder, paste0(file_name, suffix)))
}

fake_edi_metadata <- list(
  entity_key = "lakeSR sites list",
  package_id = "edi.2114.1",
  remote_checksum = "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0",
  local_checksum = "0f9e8d7c6b5a4f3e2d1c0b9a8f7e6d5c4b3a2f1e",
  checked_at = as.POSIXct("2026-11-01 10:00:00")
)

fake_zenodo_metadata <- list(
  entity_key = "riverSR_usa_v1.1.feather",
  doi = "10.5281/zenodo.4304567",
  remote_checksum = "0f343b0931126a20f133d67c2b018a3b",
  local_checksum = "6067e1795b88e18b6f21f250b151e2ab",
  checked_at = as.POSIXct("2026-11-02 12:00:00")
)

# EDI sidecar metadata identical to fake_edi_metadata except for its package ID
edi_metadata_for <- function(package_id) {
  modifyList(fake_edi_metadata, list(package_id = package_id))
}

test_that("check_sr_file_versions() aborts when the folder does not exist", {
  expect_error(
    check_sr_file_versions(tempfile()),
    regexp = "does not exist"
  )
})

test_that("check_sr_file_versions() returns a typed empty tibble when no sidecars are found", {
  tmp_dir <- withr::local_tempdir()

  # The folder contains only non-sidecar files; notes.rds must not be picked up
  writeLines("not a sidecar", file.path(tmp_dir, "notes.rds"))
  writeLines("data", file.path(tmp_dir, "data.csv"))

  expect_message(
    result <- check_sr_file_versions(tmp_dir),
    regexp = "No checksum sidecar files found"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(
    names(result),
    c("file", "source", "package_id", "checked_at",
      "remote_checksum", "local_checksum")
  )
  expect_type(result$file, "character")
  expect_type(result$source, "character")
  expect_type(result$package_id, "character")
  expect_s3_class(result$checked_at, "POSIXct")
  expect_equal(length(result$checked_at), 0)
  expect_type(result$remote_checksum, "character")
  expect_type(result$local_checksum, "character")
})

test_that("check_sr_file_versions() reports EDI and Zenodo sidecars", {
  tmp_dir <- withr::local_tempdir()

  write_fake_sidecar(
    tmp_dir, "lakeSR_poi_with_flags.csv",
    metadata = fake_edi_metadata
  )
  write_fake_sidecar(
    tmp_dir, "riverSR_usa_v1.1.feather",
    suffix = ".zenodo.rds",
    metadata = fake_zenodo_metadata
  )

  result <- check_sr_file_versions(tmp_dir)

  # Rows come back in list.files() (alphabetical) order; expected values
  # include the sidecar suffix stripped but the data extension preserved
  expected <- dplyr::tibble(
    file = c("lakeSR_poi_with_flags.csv", "riverSR_usa_v1.1.feather"),
    source = c("EDI", "Zenodo"),
    package_id = c("edi.2114.1", "10.5281/zenodo.4304567"),
    checked_at = as.POSIXct(c("2026-11-01 10:00:00", "2026-11-02 12:00:00")),
    remote_checksum = c(
      "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0",
      "0f343b0931126a20f133d67c2b018a3b"
    ),
    local_checksum = c(
      "0f9e8d7c6b5a4f3e2d1c0b9a8f7e6d5c4b3a2f1e",
      "6067e1795b88e18b6f21f250b151e2ab"
    )
  )

  expect_equal(result, expected)
  expect_s3_class(result$checked_at, "POSIXct")
})

test_that("check_sr_file_versions() keeps unreadable and non-list sidecars with NA metadata", {
  tmp_dir <- withr::local_tempdir()

  # Not a valid RDS file at all
  writeLines("junk", file.path(tmp_dir, "broken.edi.rds"))
  # Valid RDS, but not a metadata list
  saveRDS("just a string", file.path(tmp_dir, "not_a_list.zenodo.rds"))

  result <- check_sr_file_versions(tmp_dir)

  expect_equal(nrow(result), 2)
  expect_equal(result$file, c("broken", "not_a_list"))
  expect_equal(result$source, c("EDI", "Zenodo"))
  expect_true(all(is.na(result$package_id)))
  expect_true(all(is.na(result$checked_at)))
  expect_s3_class(result$checked_at, "POSIXct")
  expect_true(all(is.na(result$remote_checksum)))
  expect_true(all(is.na(result$local_checksum)))
})

test_that("check_sr_file_versions() reports NA only for the fields a sidecar lacks", {
  tmp_dir <- withr::local_tempdir()

  # A list sidecar missing the timestamp and local checksum
  write_fake_sidecar(
    tmp_dir, "partial.feather",
    metadata = list(
      package_id = "edi.2254.1",
      remote_checksum = "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0"
    )
  )

  result <- check_sr_file_versions(tmp_dir)

  expect_equal(nrow(result), 1)
  expect_equal(result$file, "partial.feather")
  expect_equal(result$source, "EDI")
  expect_equal(result$package_id, "edi.2254.1")
  expect_equal(
    result$remote_checksum,
    "a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0"
  )
  expect_true(is.na(result$checked_at))
  expect_s3_class(result$checked_at, "POSIXct")
  expect_true(is.na(result$local_checksum))
})

test_that("check_sr_file_versions() does not recurse into subfolders", {
  tmp_dir <- withr::local_tempdir()
  sub_dir <- file.path(tmp_dir, "nested")
  dir.create(sub_dir)

  write_fake_sidecar(sub_dir, "nested.feather", metadata = fake_edi_metadata)

  expect_message(
    result <- check_sr_file_versions(tmp_dir),
    regexp = "No checksum sidecar files found"
  )
  expect_equal(nrow(result), 0)
})

test_that("check_sr_file_versions() does not warn when files come from matching releases", {
  tmp_dir <- withr::local_tempdir()

  write_fake_sidecar(tmp_dir, "siteSR_Landsat4_DSWE1.feather", edi_metadata_for("edi.2254.2"))
  write_fake_sidecar(tmp_dir, "siteSR_Landsat5_DSWE1.feather", edi_metadata_for("edi.2254.2"))
  write_fake_sidecar(tmp_dir, "lakeSR_collated_handoffs.csv", edi_metadata_for("edi.2114.2"))
  write_fake_sidecar(tmp_dir, "sceneMetadata_Landsat89.csv", edi_metadata_for("edi.2254.1"))
  write_fake_sidecar(tmp_dir, "riverSR_usa_v1.1.feather", fake_zenodo_metadata,
                     suffix = ".zenodo.rds")

  expect_no_warning(result <- check_sr_file_versions(tmp_dir))
  expect_equal(nrow(result), 5)
})

test_that("check_sr_file_versions() warns when handoffs differ from the siteSR files", {
  tmp_dir <- withr::local_tempdir()

  write_fake_sidecar(tmp_dir, "siteSR_Landsat4_DSWE1.feather", edi_metadata_for("edi.2254.1"))
  write_fake_sidecar(tmp_dir, "lakeSR_collated_handoffs.csv", edi_metadata_for("edi.2114.2"))

  expect_warning(
    result <- check_sr_file_versions(tmp_dir),
    regexp = "handoff coefficients.*different release.*siteSR"
  )

  # The returned tibble is unaffected by the check
  expect_equal(nrow(result), 2)
  expect_equal(
    names(result),
    c("file", "source", "package_id", "checked_at",
      "remote_checksum", "local_checksum")
  )
})

test_that("check_sr_file_versions() warns when handoffs differ from the lakeSR files", {
  tmp_dir <- withr::local_tempdir()

  write_fake_sidecar(tmp_dir, "lakeSR_Landsat4_DSWE1.feather", edi_metadata_for("edi.2114.1"))
  write_fake_sidecar(tmp_dir, "lakeSR_collated_handoffs.csv", edi_metadata_for("edi.2114.2"))

  expect_warning(
    check_sr_file_versions(tmp_dir),
    regexp = "handoff coefficients.*different release.*lakeSR"
  )
})

test_that("check_sr_file_versions() warns when siteSR files come from more than one release", {
  tmp_dir <- withr::local_tempdir()

  write_fake_sidecar(tmp_dir, "siteSR_Landsat4_DSWE1.feather", edi_metadata_for("edi.2254.1"))
  write_fake_sidecar(tmp_dir, "siteSR_Landsat5_DSWE1.feather", edi_metadata_for("edi.2254.2"))
  write_fake_sidecar(tmp_dir, "lakeSR_collated_handoffs.csv", edi_metadata_for("edi.2114.2"))

  # Only the within-product warning fires; the handoff comparison is skipped
  # because there is no single siteSR release to compare against
  warnings <- testthat::capture_warnings(check_sr_file_versions(tmp_dir))
  expect_length(warnings, 1)
  expect_match(warnings, "siteSR files in this folder come from more than one release")
})

test_that("check_sr_file_versions() ignores sidecars without a usable EDI package ID", {
  tmp_dir <- withr::local_tempdir()

  write_fake_sidecar(tmp_dir, "siteSR_Landsat4_DSWE1.feather", edi_metadata_for("edi.2254.1"))
  # Unreadable handoff sidecar, and a sidecar with a malformed package ID
  writeLines("not an rds file", file.path(tmp_dir, "lakeSR_collated_handoffs.csv.edi.rds"))
  write_fake_sidecar(tmp_dir, "siteSR_Landsat5_DSWE1.feather", edi_metadata_for("not-an-id"))

  expect_no_warning(result <- check_sr_file_versions(tmp_dir))
  expect_equal(nrow(result), 3)
})

test_that("check_sr_file_versions() does not warn when only handoffs are present", {
  tmp_dir <- withr::local_tempdir()

  write_fake_sidecar(tmp_dir, "lakeSR_collated_handoffs.csv", edi_metadata_for("edi.2114.2"))

  expect_no_warning(check_sr_file_versions(tmp_dir))
})
