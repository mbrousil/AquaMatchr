# Test data

Small fixture files used by the tests in `tests/testthat/`. The table below reflects what is in the files and where they are used.

## Provenance

The four `*_snippet.*` files are subsets of the datasets they are named for. `lakeSR_collated_handoffs.csv` is a full copy of the handoffs file published on EDI.

| File | Used by | Contents |
|------------------------|------------------------|------------------------|
| `sitelist_snippet.csv` | `test-helpers.R` (`check_cols()` with real schemas); `test-building_joining.R` (`match_siteSR_to_WQP()` with CSV WQP input) | Site list: 10 rows, 35 columns (e.g. `siteSR_id`, `org_id`, `loc_id`, `WGS84_Latitude`/`WGS84_Longitude`, NHD waterbody/flowline fields, shoreline flags). All rows have `source = "WQP"`. |
| `siteSR_DSWE1_full_concatenation_snippet.parquet` | Same two tests as above | siteSR surface reflectance: 10 rows, 41 columns (`siteSR_id`, `dswe_filter`, `mission`, `sat_id`, `date`, `med_*`/`sd_*`/`mean_*` band summaries, pixel counts, flags). All rows are `dswe_filter = "DSWE1"` and `mission = "LT05"`, dated 1991-10-28 to 2010-02-02. |
| `chla_harmonized_snippet.feather` | Same two tests as above | Harmonized in-situ WQP data: 10 rows, 33 columns (e.g. `MonitoringLocationIdentifier`, `harmonized_utc`, `harmonized_value`, depth and quality flags). All rows are `parameter = "chlorophyll"`, with `harmonized_utc` from 2000-02-19 to 2023-09-19. |
| `chla_7day_siteSR_DSWE1_matchups_snippet.parquet` | `test-building_joining.R` (`apply_handoffs()` warns when `sat_target` is LS8) | Chlorophyll in-situ data joined to siteSR: 10 rows, 77 columns. Has the columns of the two snippets above plus `utc_seconds_offset`, `landsat_utc`, and `time_diff`, which are the columns `match_siteSR_to_WQP()` adds. `time_diff` ranges from about -5.9 to 6.9 days. Missions: 5 LT05, 3 LE07, 1 LC08, 1 LC09. Dates 2005-05-28 to 2022-07-17. |
| `lakeSR_collated_handoffs.csv` | `test-building_joining.R` (same LS8 test) | Handoff coefficients: 126 rows, 12 columns (`band`, `correction`, `dswe`, `sat_corr`, `sat_to`, `method`, `intercept`, `slope`, `B1`, `B2`, `min_in_val`, `max_in_val`). Covers Roy (`lm`, `deming`) and Gardner (`poly`) corrections for both `DSWE1` and `DSWE1a`, across 7 bands (`med_Blue`, `med_Green`, `med_Red`, `med_Nir`, `med_Swir1`, `med_Swir2`, `med_SurfaceTemp`). |
