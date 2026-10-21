##################################################
# Data harmonisation & panel construction
#
# Takes the raw CSVs produced by extract_oecd_panel.R,
# extract_statsnz_panel.R and manual extraction from DB to merge 
# them into two clean, analysis-ready panels:
#
#  1. nz_calibration_panel.csv -> long NZ-only quarterly series
#       (replaces the n=9 CALIB_DATA in the Stan model)
#  2. oecd_calibration_panel.csv -> country-quarter panel for
#       hierarchical/partial-pooling Stan model
##################################################

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(lubridate)
library(zoo)
library(janitor)

# 0. Configuration
# Paths to oecd and statsNZ data, respectively
NZ_DATA_DIR   <- "Documents/Work-notMassey/Contracting-Stats/WORK_NC/Simulations_cuts_gvt_funding/data/statsnz"   # adjust if needed
OECD_DATA_DIR <- "Documents/Work-notMassey/Contracting-Stats/WORK_NC/Simulations_cuts_gvt_funding/data/oecd"      # adjust if needed

# Date range to keep. We assume public sector employment is most reliably
# comparable across sources from the mid-1990s onward.
PANEL_START <- "1995-01-01"
PANEL_END   <- Sys.Date()

##################################################
#  Part A - NZ panel
##################################################

# A1. Functions for parsing Stats NZ date formats
# Stats NZ series formats: "2024Q1", "2024.03" or "Mar-2024" 
# depending on table. This converts robustly.
parse_nz_period <- function(x) {
  x <- as.character(x)
  result <- as.Date(rep(NA, length(x)))

  # YYYYQn - Infoshare format e.g. "1995Q1"
  m1 <- str_detect(x, "^\\d{4}Q[1-4]$")
  if (any(m1, na.rm = TRUE))
    result[m1] <- as.Date(as.yearqtr(x[m1], format = "%YQ%q"))

  # YYYY.MM e.g. "2024.03"
  m2 <- str_detect(x, "^\\d{4}\\.\\d{2}$")
  if (any(m2, na.rm = TRUE))
    result[m2] <- as.Date(as.yearmon(x[m2], format = "%Y.%m"))

  # Mon-YYYY e.g. "Mar-2024"
  m3 <- str_detect(x, "^[A-Za-z]{3}-\\d{4}$")
  if (any(m3, na.rm = TRUE))
    result[m3] <- as.Date(paste0("01-", x[m3]), format = "%d-%b-%Y")

  # YYYY-MM-DD
  m4 <- str_detect(x, "^\\d{4}-\\d{2}-\\d{2}$")
  if (any(m4, na.rm = TRUE))
    result[m4] <- as.Date(x[m4])

  result
}

# Convert any date to a quarter-start date for consistent joining
to_quarter <- function(d) {
  as.Date(as.yearqtr(d), frac = 0)
}

# A2. Load and standardise each NZ series
# If a file does not exist or has an unexpected structure, NULL returned, 
# with clear message

safe_read_csv <- function(path) {
  if (!file.exists(path)) {
    message("  [missing] ", path)
    return(NULL)
  }
  tryCatch({
    read_csv(path, show_col_types = FALSE) %>% clean_names()
  }, error = function(e) {
    message("  [read error] ", path, ": ", conditionMessage(e))
    NULL
  })
}

load_nz_unemployment <- function() {
  message("Loading NZ unemployment series...")
  df <- safe_read_csv(file.path(NZ_DATA_DIR, "nz_long_panel_unemployment.csv"))
  if (is.null(df)) return(NULL)

  # After prepare_statsnz.R: columns are "period" (YYYYQn) and "value"
  period_col <- intersect(names(df), c("period","time_period","date"))
  value_col  <- intersect(names(df), c("value","obs_value","data_value","unemp_rate"))

  if (length(period_col)==0 || length(value_col)==0) {
    message("  Unexpected column structure. Columns found: ",
           paste(names(df), collapse=", "))
    message("  Run prepare_statsnz.R first to convert Infoshare exports.")
    return(NULL)
  }

  df %>%
    transmute(
      date       = to_quarter(parse_nz_period(.data[[period_col[1]]])),
      unemp_rate = as.numeric(.data[[value_col[1]]])
    ) %>%
    filter(!is.na(date), !is.na(unemp_rate)) %>%
    distinct(date, .keep_all = TRUE) %>%
    arrange(date)
}

load_nz_public_sector <- function() {
  message("Loading NZ public sector employment series...")
  df <- safe_read_csv(file.path(NZ_DATA_DIR, "nz_long_panel_public_sector.csv"))
  if (is.null(df)) return(NULL)

  period_col <- intersect(names(df), c("period","time_period","date"))
  value_col  <- intersect(names(df), c("value","obs_value","data_value","ps_employment"))

  if (length(period_col)==0 || length(value_col)==0) {
    message("  Unexpected column structure. Columns found: ",
           paste(names(df), collapse=", "))
    message("  Run prepare_statsnz.R first to convert Infoshare exports.")
    return(NULL)
  }

  df %>%
    transmute(
      date          = to_quarter(parse_nz_period(.data[[period_col[1]]])),
      ps_employment = as.numeric(.data[[value_col[1]]])
    ) %>%
    filter(!is.na(date), !is.na(ps_employment)) %>%
    distinct(date, .keep_all = TRUE) %>%
    arrange(date) %>%
    # Converts level to quarter-on-quarter change -> "ps_chg_pct" variable 
    # needed by the Stan model
    mutate(ps_chg_pct = (ps_employment / lag(ps_employment) - 1) * 100)
}

load_nz_gdp <- function() {
  message("Loading NZ GDP growth series...")
  df <- safe_read_csv(file.path(NZ_DATA_DIR, "nz_long_panel_gdp_growth.csv"))
  if (is.null(df)) return(NULL)

  period_col <- intersect(names(df), c("period","time_period","date"))
  value_col  <- intersect(names(df), c("value","obs_value","data_value"))

  if (length(period_col)==0 || length(value_col)==0) {
    message("  Unexpected column structure. Columns found: ",
           paste(names(df), collapse=", "))
    return(NULL)
  }

  df %>%
    transmute(
      date      = to_quarter(parse_nz_period(.data[[period_col[1]]])),
      gdp_delta = as.numeric(.data[[value_col[1]]])
    ) %>%
    filter(!is.na(date), !is.na(gdp_delta)) %>%
    distinct(date, .keep_all = TRUE) %>%
    arrange(date)
}

load_nz_wellbeing <- function() {
  message("Loading NZ GSS wellbeing series (biennial - needs interpolation)...")
  df <- safe_read_csv(file.path(NZ_DATA_DIR, "nz_long_panel_wellbeing.csv"))
  if (is.null(df)) return(NULL)

  # GSS is biennial and column names vary (a lot) by release year.
  # This loader is intentionally permissive and flags for manual check.
  # After prepare_statsnz.R: columns are "year" and "value"
  year_col  <- intersect(names(df), c("year","survey_year"))
  value_col <- intersect(names(df), c("value","life_satisfaction","mean_score"))

  if (length(year_col)==0 || length(value_col)==0) {
    message("  Unexpected column structure. Columns found: ",
           paste(names(df), collapse=", "))
    message("  GSS release tables vary by year - manually identify the",
           " 'life satisfaction, mean, 0-10 scale' column and edit this",
           " function to reference it directly.")
    return(NULL)
  }

  df %>%
    transmute(
      date              = as.Date(paste0(.data[[year_col[1]]], "-01-01")),
      wellbeing_level   = as.numeric(.data[[value_col[1]]])
    ) %>%
    filter(!is.na(date), !is.na(wellbeing_level)) %>%
    distinct(date, .keep_all = TRUE) %>%
    arrange(date)
}

load_nz_benefit_uptake <- function() {
  message("Loading NZ benefit uptake series...")
  df <- safe_read_csv(file.path(NZ_DATA_DIR, "nz_long_panel_benefit_uptake.csv"))
  if (is.null(df)) return(NULL)

  period_col <- intersect(names(df), c("period","month","date"))
  value_col  <- intersect(names(df), c("jobseeker_recipients","value","count"))

  if (length(period_col)==0 || length(value_col)==0) {
    message("  Unexpected column structure. Columns found: ",
           paste(names(df), collapse=", "))
    message("  MSD fact sheet structure varies by release — manually identify",
           " the Jobseeker Support recipient count column.")
    return(NULL)
  }

  df %>%
    transmute(
      date        = to_quarter(parse_nz_period(.data[[period_col[1]]])),
      jobseeker_n = as.numeric(.data[[value_col[1]]])
    ) %>%
    filter(!is.na(date), !is.na(jobseeker_n)) %>%
    distinct(date, .keep_all = TRUE) %>%
    arrange(date)
}

# A3. Interpolate sparse series onto the common quarterly grid: GSS wellbeing 
# (biennial) needs interpolation onto quarters -> linear interpolation (flag
# in paper as assumption)
interpolate_to_quarterly <- function(df, value_col, date_grid) {
  if (is.null(df) || nrow(df) < 2) return(NULL)

  approx_vals <- approx(
    x    = as.numeric(df$date),
    y    = df[[value_col]],
    xout = as.numeric(date_grid),
    rule = 1  # no extrapolation beyond observed range
  )

  tibble(date = date_grid, value = approx_vals$y) %>%
    rename(!!value_col := value)
}

# A4. Build master NZ quarterly panel
build_nz_calibration_panel <- function(
    save_path = "data/nz_calibration_panel.csv") {

  message("\n=== Assembling NZ calibration panel ===\n")

  unemp     <- load_nz_unemployment()
  pubsector <- load_nz_public_sector()
  gdp       <- load_nz_gdp()
  wellbeing <- load_nz_wellbeing()
  benefits  <- load_nz_benefit_uptake()

  available <- list(unemployment = unemp, public_sector = pubsector,
                    gdp = gdp, wellbeing = wellbeing, benefits = benefits) %>%
    compact()

  if (length(available) == 0) {
    stop("No NZ series loaded successfully. Check that ",
        "extract_statsnz_panel.R has been run and produced CSVs in ",
        NZ_DATA_DIR, ", and that column names match what the loaders expect.")
  }

  message("Successfully loaded: ", paste(names(available), collapse=", "))
  if (length(available) < 5) {
    message("Missing: ", paste(setdiff(
      c("unemployment","public_sector","gdp","wellbeing","benefits"),
      names(available)), collapse=", "))
    message("Panel will be built with available series only.")
  }

  # Common quarterly date grid spanning the requested range
  date_grid <- seq(to_quarter(as.Date(PANEL_START)),
                   to_quarter(as.Date(PANEL_END)),
                   by = "quarter")

  panel <- tibble(date = date_grid)

  if (!is.null(unemp)) {
    panel <- panel %>% left_join(unemp, by = "date")
  }
  if (!is.null(pubsector)) {
    panel <- panel %>% left_join(
      pubsector %>% select(date, ps_chg_pct), by = "date")
  }
  if (!is.null(gdp)) {
    panel <- panel %>% left_join(gdp, by = "date")
  }
  if (!is.null(wellbeing)) {
    wb_interp <- interpolate_to_quarterly(wellbeing, "wellbeing_level", date_grid)
    if (!is.null(wb_interp)) {
      panel <- panel %>%
        left_join(wb_interp, by = "date") %>%
        # Convert level to difference to previous quarter
        mutate(wellbeing_delta = wellbeing_level - lag(wellbeing_level))
    }
  }
  if (!is.null(benefits)) {
    # Benefit uptake rate needs a denominator (newly unemployed count).
    # If unemployment data is available, approximate uptake rate as
    # change in Jobseeker recipients relative to change in unemployed.
    if (!is.null(unemp)) {
      panel <- panel %>%
        left_join(benefits, by = "date") %>%
        mutate(
          jobseeker_chg = jobseeker_n - lag(jobseeker_n),
          # crude proxy
          benefit_uptake = pmin(1, pmax(0,
            jobseeker_chg / (unemp_rate - lag(unemp_rate)) / 1000))
        )
      message("  NOTE: benefit_uptake is a crude derived proxy (change in",
             " Jobseeker recipients relative to change in unemployment).",
             " Ideally to be replaced with MSD's directly published uptake",
             " rate if available in their fact sheets.")
    } else {
      panel <- panel %>% left_join(benefits, by = "date")
    }
  }

  # Final cleanup: keep complete cases only for the core variables
  # the Stan model needs (ps_chg_pct, unemp_rate-derived delta, gdp_delta)
  panel <- panel %>%
    mutate(unemp_delta = unemp_rate - lag(unemp_rate)) %>%
    arrange(date)

  #n_complete <- panel %>% # Keep NA's to be imputed in Stan
    #filter(!is.na(ps_chg_pct), !is.na(unemp_delta)) %>%
    #nrow()

  message("\nPanel assembled: ", nrow(panel), " quarters total, ",
         n_complete, " with complete ps_chg_pct + unemp_delta")

  write_csv(panel, save_path)
  message("Saved: ", save_path)

  panel
}

##################################################
#  Part B - OECD cross-country panel
##################################################

load_oecd_csv <- function(filename) {
  path <- file.path(OECD_DATA_DIR, filename)
  if (!file.exists(path)) {
    message("  [missing] ", path)
    return(NULL)
  }
  tryCatch({
    df <- read_csv(path, show_col_types = FALSE)
    # OECD SDMX csv file with labels exports have paired columns:
    # CODE_COL, Label col, CODE_COL, Label col, ...
    # Clean duplicate name suffixes.
    # Strip label columns (odd-indexed after position 1) by keeping
    # only columns whose original header is ALL_CAPS or already clean
    raw_names <- names(df)
    # If it looks like an SDMX file (has REF_AREA or TIME_PERIOD), keep
    # only the code columns (uppercase with underscores) plus numeric cols.
    if (any(c("REF_AREA","TIME_PERIOD","OBS_VALUE") %in% raw_names)) {
      keep <- raw_names %in% c("REF_AREA","TIME_PERIOD","OBS_VALUE",
                                "FREQ","ADJUSTMENT","SEX","AGE",
                                "UNIT_MEASURE","TRANSFORMATION",
                                "STRUCTURE","ACTION")
      # OBS_VALUE in SDMX csv file with labels is the CODE col and is blank;
      # the actual numeric value is in the NEXT column "Observation value"
      obs_pos <- which(raw_names == "OBS_VALUE")
      if (length(obs_pos) > 0 && (obs_pos[1] + 1) <= ncol(df)) {
        # grab the label column next to OBS_VALUE as the real value
        df[["OBS_VALUE"]] <- df[[obs_pos[1] + 1]]
      }
      df <- df[, keep, drop = FALSE]
    }
    names(df) <- tolower(names(df))
    df
  }, error = function(e) {
    message("  [read error] ", path, ": ", conditionMessage(e))
    NULL
  })
}

# OECD SDMX csv file with labels exports typically include columns:
# REF_AREA, Reference area, TIME_PERIOD, OBS_VALUE, plus various
# dimension labels. clean_names() lowercases/snakes these to
# ref_area, reference_area, time_period, obs_value, etc.

standardise_oecd_series <- function(df, value_name) {
  if (is.null(df)) return(NULL)

  # Country: iso3 / ref_area (SDMX) / country
  country_col <- intersect(names(df), c("iso3","ref_area","country","reference_area"))
  # Period: time_period (SDMX quarterly "1995-Q1") / year (annual integer) / period
  period_col  <- intersect(names(df), c("time_period","year","period"))
  # Value: obs_value is col 23 in SDMX but often blank; observation_value is col 24
  # Also handle pre-cleaned files with direct value column names
  value_col   <- intersect(names(df), c("obs_value","observation_value","value",
                                         "gov_emp_pct","unemp_rate","gdp_growth",
                                         "gdp_growth_yoy","life_satisfaction"))

  if (length(country_col)==0 || length(period_col)==0 || length(value_col)==0) {
    message("  Unexpected OECD column structure. Columns found: ",
           paste(names(df), collapse=", "))
    return(NULL)
  }

  # For SDMX files obs_value may be all NA (label column used instead);
  # try each value_col candidate until we find one with actual numbers
  val <- NULL
  for (vc in value_col) {
    candidate <- suppressWarnings(as.numeric(df[[vc]]))
    if (sum(!is.na(candidate)) > 0) { val <- candidate; break }
  }
  if (is.null(val)) {
    message("  All value columns are NA. Columns tried: ",
            paste(value_col, collapse=", "))
    return(NULL)
  }

  tibble(
    country = as.character(df[[country_col[1]]]),
    period  = as.character(df[[period_col[1]]]),
    value   = val
  ) %>%
    filter(!is.na(value), !is.na(country), !is.na(period)) %>%
    rename(!!value_name := value)
}

read_sdmx_csv <- function(path, country_col, period_col, value_col,
                           filters = list()) {
  # Reads an OECD SDMX csv file with labels export by exact column name.
  # The format has paired CODE/Label columns; read_csv gives the Label
  # column a ...N suffix when names collide. We select here by position index
  # of the named CODE column, and grab value from its adjacent label col
  # only when the code col is all-NA.
  if (!file.exists(path)) { message("  [missing] ", path); return(NULL) }
  df <- tryCatch(read_csv(path, show_col_types = FALSE),
                 error = function(e) { message("  [error] ", path); NULL })
  if (is.null(df)) return(NULL)

  raw <- names(df)

  resolve_col <- function(col_name) {
    idx <- which(raw == col_name)
    if (length(idx) == 0) return(NULL)
    vals <- df[[idx[1]]]
    # If all NA, try the next column (label companion)
    if (all(is.na(vals)) && (idx[1] + 1) <= ncol(df))
      vals <- df[[idx[1] + 1]]
    vals
  }

  country <- resolve_col(country_col)
  period  <- resolve_col(period_col)
  value   <- suppressWarnings(as.numeric(resolve_col(value_col)))

  if (is.null(country) || is.null(period) || is.null(value))  {
    message("  Could not resolve columns in ", basename(path))
    message("  Available: ", paste(raw, collapse=", "))
    return(NULL)
  }

  result <- tibble(country = as.character(country),
                   period  = as.character(period),
                   value   = value)

  for (nm in names(filters)) {
    col_vals <- as.character(resolve_col(nm))
    result <- result[col_vals == filters[[nm]], ]
  }

  result %>% filter(!is.na(value), !is.na(country), !is.na(period))
}

build_oecd_calibration_panel <- function(
    save_path = "data/oecd_calibration_panel.csv") {

  message("\n=== Assembling OECD cross-country panel ===\n")

  gov_path  <- file.path(OECD_DATA_DIR, "oecd_gov_employment.csv")
  une_path  <- file.path(OECD_DATA_DIR, "oecd_unemployment.csv")
  gdp_path  <- file.path(OECD_DATA_DIR, "oecd_gdp.csv")

  # Gov employment: clean CSV with iso3, year, gov_emp_pct
  gov_emp <- if (file.exists(gov_path)) {
    read_csv(gov_path, show_col_types = FALSE) %>%
      transmute(country = as.character(iso3),
                period  = as.character(year),
                ps_employment_pct = as.numeric(gov_emp_pct)) %>%
      filter(!is.na(ps_employment_pct))
  } else { message("  [missing] ", gov_path); NULL }

  # Unemployment: SDMX csv file with labels, filter Sex=_T Age=Y_GE15 Adj=Y
  unemp <- read_sdmx_csv(une_path,
    country_col = "REF_AREA", period_col = "TIME_PERIOD", value_col = "OBS_VALUE",
    filters = list(SEX = "_T", AGE = "Y_GE15", ADJUSTMENT = "Y")) %>%
    { if (!is.null(.)) rename(., unemp_rate = value) else NULL }

  # GDP: SDMX csv file with labels
  gdp <- read_sdmx_csv(gdp_path,
    country_col = "REF_AREA", period_col = "TIME_PERIOD", value_col = "OBS_VALUE") %>%
    { if (!is.null(.)) rename(., gdp_growth = value) else NULL }

  wellbeing <- NULL  # no OECD wellbeing file available

  available <- list(gov_emp = gov_emp, unemp = unemp,
                    gdp = gdp, wellbeing = wellbeing) %>% compact()

  if (length(available) == 0) {
    stop("No OECD series loaded successfully. Check that ",
        "extract_oecd_panel.R has been run and produced CSVs in ",
        OECD_DATA_DIR, ".")
  }

  message("Successfully loaded: ", paste(names(available), collapse=", "))

  # Gov employment is annual; unemp + gdp are quarterly.
  # We build a quarterly spine, left-join annual gov_emp by year,
  # then join quarterly series on country + period.

  quarterly_series <- available[names(available) != "gov_emp"]
  annual_series    <- available[names(available) == "gov_emp"]

  if (length(quarterly_series) > 0) {
    panel <- reduce(quarterly_series,
                    function(x, y) full_join(x, y, by = c("country","period")))
  } else {
    # All annual - just use gov_emp as spine
    panel <- available[["gov_emp"]] %>% rename(period = period)
  }

  # Attach annual gov_emp by matching year portion of quarterly period
  if (!is.null(available[["gov_emp"]])) {
    gov <- available[["gov_emp"]] %>%
      rename(year_key = period)
    panel <- panel %>%
      mutate(year_key = str_extract(period, "^\\d{4}")) %>%
      left_join(gov, by = c("country", "year_key")) %>%
      select(-year_key)
  }

  panel <- panel %>%
    arrange(country, period) %>%
    group_by(country) %>%
    mutate(
      ps_chg_pct  = if ("ps_employment_pct" %in% names(.))
                      ps_employment_pct - lag(ps_employment_pct) else NA_real_,
      unemp_delta = if ("unemp_rate" %in% names(.))
                      unemp_rate - lag(unemp_rate) else NA_real_
    ) %>%
    ungroup()

  n_complete <- panel %>%
    filter(!is.na(ps_chg_pct), !is.na(unemp_delta)) %>%
    nrow()

  message("\nOECD panel assembled: ", nrow(panel), " country-period rows, ",
         n_complete, " with complete ps_chg_pct + unemp_delta across ",
         n_distinct(panel$country), " countries.")

  write_csv(panel, save_path)
  message("Saved: ", save_path)

  panel
}

##################################################
#  Part C - Identify consolidation episodes
#  (narrative-style identification, à la Devries et al. 2011)
##################################################
# Flags quarters where public sector employment fell by more than
# a threshold, as candidate "consolidation episodes" for closer
# case-study analysis or for a treatment-effect / event-study design.

flag_consolidation_episodes <- function(panel, threshold_pct = -1.0,
                                        min_consecutive = 2) {
  panel %>%
    arrange(date) %>%
    mutate(
      is_contraction = !is.na(ps_chg_pct) & ps_chg_pct <= threshold_pct,
      episode_run    = with(rle(is_contraction), rep(seq_along(lengths), lengths))
    ) %>%
    group_by(episode_run) %>%
    mutate(run_length = n()) %>%
    ungroup() %>%
    mutate(
      consolidation_episode = is_contraction & run_length >= min_consecutive
    ) %>%
    select(-episode_run, -run_length)
}

##################################################
#  Part D - Diagnostic plots
##################################################

plot_panel_diagnostics <- function(panel, title_prefix = "NZ") {
  library(ggplot2)
  library(patchwork)

  p1 <- ggplot(panel, aes(x = date, y = ps_chg_pct)) +
    geom_hline(yintercept = 0, colour = "grey70") +
    geom_line(colour = "#378ADD") +
    labs(title = paste(title_prefix, "- Public sector employment change (%)"),
         x = NULL, y = "QoQ % change") +
    theme_minimal()

  p2 <- ggplot(panel, aes(x = date, y = unemp_rate)) +
    geom_line(colour = "#E24B4A") +
    labs(title = paste(title_prefix, "- Unemployment rate"),
         x = NULL, y = "%") +
    theme_minimal()

  if ("consolidation_episode" %in% names(panel)) {
    p1 <- p1 +
      geom_point(data = filter(panel, consolidation_episode),
                aes(x = date, y = ps_chg_pct), colour = "#A32D2D", size = 2)
  }

  p1 / p2
}

##################################################
#  Run all
##################################################
# Step 1: run 2-prepare_statsnz.R first to convert Infoshare exports
# Step 2: source this file to build both panels

nz_panel   <- build_nz_calibration_panel(
  save_path = "data/nz_calibration_panel.csv")
nz_panel   <- flag_consolidation_episodes(nz_panel)

oecd_panel <- build_oecd_calibration_panel(
  save_path = "data/oecd_calibration_panel.csv")

plot_panel_diagnostics(nz_panel, "NZ")

message("Script loaded.")
message("Run build_nz_calibration_panel() and build_oecd_calibration_panel()")
message("after extract_statsnz_panel.R and extract_oecd_panel.R have produced")
message("their raw CSV outputs in the data/ directory.")
message("")
message("IMPORTANT: column-name mapping in the load_* functions above makes")
message("(makes) my guess assumptions about Stats NZ / OECD export column names.")
message("Inspecting the raw CSVs with names(read_csv('your_file.csv')) and adjust")
message("the intersect() calls in each loader if the actual column names differ.")
