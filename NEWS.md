# AquaMatchr 0.0.0.9000

## Breaking changes

These are recent changes that may cause code written for earlier versions of the package to stop working. For each one, we've shown what to change in your code below.

### You now need to log in to EDI before downloading

The download functions (`download_parameters()`, `download_siteSR()`, `download_lakeSR()`, and `download_sceneMetadata()`) now require you to be logged in to the Environmental Data Initiative (EDI) before they will run. (This follows a change on EDI's side: as of July 2026, EDI requires all data access to be authenticated.) If you aren't logged in, the functions will stop with a message explaining what to do.

The easiest option is to run this once per R session:

```r
EDIutils::login()
```

If you'd rather not log in every session, you can store your EDI API key permanently by running `usethis::edit_r_environ()` and adding a line like `EDI_API_KEY='your-key-here'` to the file that opens, then restarting R.

### Some argument names have changed

If your code uses the old names, you'll see an "unused argument" error. The fixes are simple find-and-replace:

* In the download functions (`download_siteSR()`, `download_lakeSR()`, `download_sceneMetadata()`, `download_riverSR()`), `save_location` is now called `download_folder`:

  ```r
  # Old
  download_siteSR(save_location = "my/data/folder")
  # New
  download_siteSR(download_folder = "my/data/folder")
  ```

* In `match_siteSR_to_WQP()` and `apply_handoffs()`, `save_location` is now called `output_file`:

  ```r
  # Old
  match_siteSR_to_WQP(..., save_location = "output/matchups.parquet")
  # New
  match_siteSR_to_WQP(..., output_file = "output/matchups.parquet")
  ```

### The `ask` and `force` arguments have been replaced by `update`

The SR download functions used to ask a yes/no question about overwriting files (`ask = TRUE/FALSE`, or `force = TRUE/FALSE` in `download_riverSR()`). They now use a single `update` argument with four options: `"ask"` (the default — prompts you before replacing files), `"always"` (re-downloads every time), `"never"` (keeps existing files), and `"if_changed"` (re-downloads only files that have changed).

* `ask = TRUE` can simply be deleted, since asking is still the default behavior. (Or write `update = "ask"` if you prefer to be explicit.)
* `ask = FALSE` becomes `update = "always"`.
* In `download_riverSR()`, `force = TRUE` becomes `update = "always"` and `force = FALSE` becomes `update = "if_changed"`.

### `build_sr()` now works differently

`build_sr()` (which stacks the separate Landsat files into one dataset) has been reworked so it no longer loads the full dataset into your computer's memory. This makes it much more reliable on typical laptops, but it changes how you use it:

* You must now tell it where to save the result with `output_file`, and the result is always a `.parquet` file.
* It returns the path to that file rather than the dataset itself. To work with the result, open it with `arrow::open_dataset()`:

  ```r
  # Old
  stacked <- build_sr(which_sr = "siteSR", sr_location = "data", save = TRUE,
                      save_location = "data/stacked.feather")
  # New
  stacked_path <- build_sr(which_sr = "siteSR", sr_location = "data",
                           algal_mask = FALSE,
                           output_file = "data/stacked.parquet")
  stacked <- arrow::open_dataset(stacked_path)
  ```

* The `save` and `save_location` arguments no longer exist, and `algal_mask` must now be stated explicitly (`TRUE` or `FALSE`).

### Downloaded files no longer have dates in their names

Files used to be saved with dates in their names to match information on EDI, like `lakeSR_collated_handoffs_GEEv2025-02-12_QAv2025-06-04.csv`. They are now saved without dates, like `lakeSR_collated_handoffs.csv`. Two consequences:

* If your code points at a file by its old dated name (for example as `site_list_path` or `handoff_path`), update it to the new name.
* If you already have dated files downloaded, the download functions won't recognize them and will download fresh copies under the new names. You can delete the old dated files to save disk space.

## New features

* New function `download_sceneMetadata()` downloads scene-level metadata (e.g., whole-scene cloud cover) for siteSR or lakeSR. This is useful for quality-checking your data before matching, as demonstrated in the example workflow vignette.
* New `update = "if_changed"` option for all download functions: keeps your existing files if they haven't changed and re-downloads only the ones that have. Previously, if even one file was missing from a download, all of them were downloaded again. Small files ending in `.edi.rds` or `.zenodo.rds` will appear next to your downloads — these record file checksums so the package can tell whether anything changed, and you can safely ignore them.
* `match_siteSR_to_WQP()` now accepts `.parquet` files for `wqp_path` and `siteSR_path` in addition to the formats it accepted before, and `apply_handoffs()` accepts `.feather` or `.parquet` input.

## Minor improvements

* All SR download functions now return the paths to your local files, so you can capture them and pass them to the next step instead of typing paths by hand:

  ```r
  siteSR_paths <- download_siteSR(download_folder = "my/data/folder")
  match_siteSR_to_WQP(..., site_list_path = siteSR_paths["siteSR_site_list"])
  ```
