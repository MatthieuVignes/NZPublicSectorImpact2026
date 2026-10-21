##################################################
# Stats NZ data preparation
#
# Converts Stats NZ Infoshare wide-format CSVs into long-format CSVs
#
#  Input files (put these in DATA_DIR):
#    nz_long_panel_unemployment.csv -> HLF097AA, Unemployment Rate SA
#    nz_long_panel_public_sector.csv -> QEM036AA, Filled Jobs Public Sector
#    nz_long_panel_wellbeing.csv -> GSS hardcoded means (already long)
#
#  Output: overwrites the same files with clean long-format CSVs
#          columns: period, value
#
#  Run this ONCE before 3-harmonise_calibration_panel.R
##################################################

library(dplyr)
library(readr)
library(stringr)
library(zoo)

# Config
DATA_DIR <- "Documents/Work-notMassey/Contracting-Stats/WORK_NC/Simulations_cuts_gvt_funding/data/statsnz" # adjust if required

# Function to parse Infoshare wide export
# Infoshare CSVs have:
# Row 1: table title (col 1), blank (col 2)
# Row 2: blank, dimension label (e.g. "Public Sector")
# Row 3+: optional extra header rows (sub-dimension labels)
# Then data rows: "YYYYQn", numeric_value
# Then footer rows starting with "Table information:" etc.
#
# This function finds the first data row (matches YYYYQn pattern),
# reads until a non-data row, and returns a two-column tibble.

parse_infoshare_csv <- function(path) {
  raw <- read_csv(path, col_names = FALSE, show_col_types = FALSE,
                  col_types = cols(.default = col_character()))

  # Quarter pattern: 1995Q1 … 2030Q4
  qtr_pat <- "^\\d{4}Q[1-4]$"

  data_rows <- which(str_detect(raw[[1]], qtr_pat))

  if (length(data_rows) == 0) {
    stop("No quarter rows found in ", path,
         ". Check the file is the Infoshare CSV export.")
  }

  tibble(
    period = raw[[1]][data_rows],
    value  = suppressWarnings(as.numeric(raw[[2]][data_rows]))
  ) |>
    filter(!is.na(value))
}

# 1. Unemployment rate
message("Processing unemployment rate...")
une_path <- file.path(DATA_DIR, "nz_long_panel_unemployment.csv")
if (file.exists(une_path)) {
  une <- parse_infoshare_csv(une_path)
  write_csv(une, une_path)
  message("  OK: ", nrow(une), " quarters, ",
          une$period[1], " – ", tail(une$period, 1))
} else {
  message("  MISSING: ", une_path)
}

# 2. Public sector filled jobs
message("Processing public sector filled jobs...")
ps_path <- file.path(DATA_DIR, "nz_long_panel_public_sector.csv")
if (file.exists(ps_path)) {
  ps <- parse_infoshare_csv(ps_path)
  write_csv(ps, ps_path)
  message("  OK: ", nrow(ps), " quarters, ",
          ps$period[1], " – ", tail(ps$period, 1))
} else {
  message("  MISSING: ", ps_path)
}

# 3. GSS wellbeing (already long, just validate)
message("Processing GSS wellbeing...")
wb_path <- file.path(DATA_DIR, "nz_long_panel_wellbeing.csv")
if (file.exists(wb_path)) {
  wb <- read_csv(wb_path, show_col_types = FALSE)
  # Rename to what 3-harmonise_calibration_panel.R expects
  if ("survey_year" %in% names(wb) && "life_satisfaction" %in% names(wb)) {
    wb_out <- wb |>
      transmute(
        year  = survey_year,
        value = life_satisfaction
      )
    write_csv(wb_out, wb_path)
    message("  OK: ", nrow(wb_out), " survey waves, ",
            wb_out$year[1], " – ", tail(wb_out$year, 1))
  } else {
    message("  Unexpected columns in wellbeing file: ",
            paste(names(wb), collapse = ", "))
  }
} else {
  message("  MISSING: ", wb_path)
}

message("\nDone. Long-format CSVs written to ", DATA_DIR)
message("Now run 3-harmonise_calibration_panel.R")
