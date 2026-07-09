# Attack Politics under Exposure: The Silberstein Affair and Negative Campaigning (AUTNES 2017)

Replication materials for the master's seminar paper "Attack Politics under
Exposure: The Silberstein Affair and Negative Campaigning in Press Releases
and on Facebook in the 2017 Austrian Election" (Yves Furrer, University of
Lucerne, 2026).

## Data

The AUTNES source data are distributed under a scientific use licence and
cannot be redistributed here. Register at AUSSDA (free) and download both
datasets, then place the CSV data files in `Data/raw/`:

- AUTNES Content Analysis of Party Press Releases: Cumulative File
  (SUF edition): https://doi.org/10.11587/25P2WR
  -> file needed: `10726_da_en_v1_0.csv`
- AUTNES Content Analysis of Campaign Facebook Pages 2017 (SUF edition):
  https://doi.org/10.11587/17OKDP
  -> file needed: `10728_da_en_v1_0.csv`

The 2019 Chapel Hill Expert Survey (`CHES2019V3.csv`) is downloaded
automatically from chesdata.eu on the first run of the script.

## How to run

1. Open `silberstein_analysis.R` in R (>= 4.4).
2. First run: set `RUN_DATA_PREP <- TRUE` (builds the dyad-day panel and
   saves it to `Data/`). Afterwards set it to `FALSE`.
3. Run the full script. All figures and tables are written to `Output/`.
4. Package versions are documented in `sessionInfo.txt`.

## Notes

Users of the AUTNES data are requested to notify the AUTNES team of
publications using the data (mail@autnes.at).
