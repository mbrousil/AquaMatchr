#' Report the versions of downloaded SR files
#'
#' @description
#' Read the checksum sidecar files created by the download functions and
#' report which version of each downloaded SR file is present in a folder.
#'
#' @details
#' When the download functions write an SR file, they record a small metadata
#' file (a "sidecar") next to it: `<file>.edi.rds` for EDI products
#' (siteSR, lakeSR, scene metadata) and `<file>.zenodo.rds` for riverSR.
#' Sidecars record the remote version (EDI package ID or Zenodo DOI), checksums,
#' and the time the file was downloaded. This function reads all sidecars in
#' `download_folder` (subfolders are not searched) and returns one row per
#' sidecar.
#'
#' Sidecars that are unreadable or do not contain a metadata list are still
#' reported, with `NA` in the metadata fields, rather than dropped. Files
#' without a sidecar are not reported at all; run the download functions with
#' `update = "if_changed"` to restore sidecars for them.
#'
#' The function also warns when the EDI files in the folder do not fit
#' together:
#' * the siteSR files (or the lakeSR files) come from more than one release, or
#' * the lakeSR handoff coefficients come from a different release than the
#'   siteSR or lakeSR files. Handoffs are published only within the lakeSR
#'   package but are used with both products, so their revision is expected to
#'   match the revision of the siteSR and lakeSR files. This can happen when
#'   `download_siteSR()` and `download_lakeSR()` are run with different
#'   versions in the same folder, because they share one handoff file.
#'
#' These checks compare EDI revision numbers and assume that siteSR and lakeSR
#' revisions are released together. Scene metadata and riverSR files are not
#' checked. The returned tibble is not affected by the checks.
#'
#' @param download_folder String. Path to the folder containing the downloaded
#'   SR files and their sidecars.
#'
#' @return A tibble with one row per sidecar file found in `download_folder`:
#' * `file`: the name of the file the sidecar describes (sidecar suffix removed)
#' * `source`: "EDI" or "Zenodo"
#' * `package_id`: the EDI package ID, or the Zenodo DOI for riverSR sidecars
#' * `checked_at`: when the file was downloaded (POSIXct)
#' * `remote_checksum`: checksum reported by the remote source
#' * `local_checksum`: checksum of the locally saved file at download time
#'
#' Metadata fields are `NA` for sidecars that are unreadable or malformed. If
#' no sidecars are found, an informative message is emitted and a zero-row
#' tibble with the same columns is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' # After downloading siteSR and riverSR into the same folder
#' check_sr_file_versions(download_folder = "~/AquaMatchr_data")
#' }
check_sr_file_versions <- function(download_folder) {
  if (!dir.exists(download_folder)) {
    cli::cli_abort(
      c(
        "The folder {.file {download_folder}} does not exist.",
        "i" = "Please provide the path to a folder of downloaded SR data."
      ),
      call = NULL
    )
  }

  # The two sidecar suffixes written by the download functions
  sidecar_names <- list.files(
    download_folder,
    pattern = "\\.(edi|zenodo)\\.rds$"
  )

  if (length(sidecar_names) == 0) {
    cli::cli_alert_info(
      "No checksum sidecar files found in {.file {download_folder}}."
    )
    return(
      dplyr::tibble(
        file = character(),
        source = character(),
        package_id = character(),
        checked_at = .POSIXct(numeric(0)),
        remote_checksum = character(),
        local_checksum = character()
      )
    )
  }

  # Extract a single character field from sidecar metadata; NA when the
  # sidecar does not record a usable value
  sidecar_chr <- function(value) {
    if (is.character(value) && length(value) == 1L && !is.na(value)) {
      value
    } else {
      NA_character_
    }
  }

  # Same for the checked_at timestamp, preserving the POSIXct type of NA rows
  sidecar_time <- function(value) {
    if (length(value) == 1L && inherits(value, "POSIXt") && !is.na(value)) {
      value
    } else {
      as.POSIXct(NA)
    }
  }

  sidecar_row <- function(sidecar_name) {
    source <- if (grepl("\\.zenodo\\.rds$", sidecar_name)) "Zenodo" else "EDI"
    suffix <- if (source == "Zenodo") ".zenodo.rds" else ".edi.rds"

    # The file the sidecar describes, with the sidecar suffix removed
    file_root <- sub("\\.(edi|zenodo)\\.rds$", "", sidecar_name)

    metadata <- read_sidecar(file.path(download_folder, file_root), suffix)

    # Keep unreadable or non-list sidecars in the output with NA metadata
    if (!is.list(metadata)) {
      metadata <- list()
    }

    # EDI sidecars record package_id; Zenodo sidecars record doi
    version_id <- metadata$package_id
    if (is.null(version_id)) {
      version_id <- metadata$doi
    }

    dplyr::tibble(
      file = file_root,
      source = source,
      package_id = sidecar_chr(version_id),
      checked_at = sidecar_time(metadata$checked_at),
      remote_checksum = sidecar_chr(metadata$remote_checksum),
      local_checksum = sidecar_chr(metadata$local_checksum)
    )
  }

  versions <- purrr::map_dfr(sidecar_names, sidecar_row)

  warn_inconsistent_sr_versions(versions)

  versions
}

#' Warn when the EDI files in a folder come from mismatched releases
#'
#' @param versions The tibble built by [check_sr_file_versions()]
#'
#' @return Invisible NULL, called for its warning side effect. Warns when the
#'   siteSR or lakeSR files come from more than one release, or when the lakeSR
#'   handoff coefficients come from a different revision than the siteSR or
#'   lakeSR files. Rows without a usable EDI package ID are ignored.
#' @noRd
warn_inconsistent_sr_versions <- function(versions) {
  edi <- versions[
    versions$source == "EDI" &
      !is.na(versions$package_id) &
      grepl("^edi\\.[0-9]+\\.[0-9]+$", versions$package_id),
  ]

  is_handoff <- edi$file == "lakeSR_collated_handoffs.csv"

  product_ids <- list(
    siteSR = unique(edi$package_id[grepl("^siteSR_", edi$file)]),
    lakeSR = unique(edi$package_id[grepl("^lakeSR_", edi$file) & !is_handoff])
  )
  handoff_ids <- unique(edi$package_id[is_handoff])

  revision_of <- function(package_id) {
    sub("^edi\\.[0-9]+\\.", "", package_id)
  }

  for (product in names(product_ids)) {
    ids <- product_ids[[product]]

    if (length(ids) > 1L) {
      cli::cli_warn(
        c(
          "The {product} files in this folder come from more than one release: {.val {ids}}.",
          "i" = "Re-run the download function with a single {.arg version} so the files match."
        )
      )
    } else if (length(ids) == 1L && length(handoff_ids) == 1L &&
               revision_of(ids) != revision_of(handoff_ids)) {
      cli::cli_warn(
        c(
          "The lakeSR handoff coefficients ({.val {handoff_ids}}) come from a different release than the {product} files ({.val {ids}}).",
          "i" = "Re-run {.fn download_siteSR} or {.fn download_lakeSR} with a matching {.arg version} to refresh them."
        )
      )
    }
  }

  invisible(NULL)
}
