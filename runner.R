#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

fallback_source_order <- c(
  "imports.R",
  "auxiliary.R",
  "data.R",
  "pre_processing_data.R",
  "post_test_analysis.R",
  "plots.R",
  "pre_test_power.R",
  "MultiCell.R"
)

load_geolift_runtime <- function() {
  if (requireNamespace("GeoLift", quietly = TRUE)) {
    return(invisible(TRUE))
  }

  # Fallback mode sources GeoLift's R files directly instead of loading the
  # installed package namespace. For the single GeoLift() happy path we only
  # need the core runtime dependencies used by post_test_analysis.R and its
  # helpers, not the full package ecosystem used by every vignette or helper.
  required_packages <- c(
    "augsynth",
    "dplyr",
    "ggplot2",
    "gridExtra",
    "lifecycle",
    "progress",
    "rlang",
    "scales",
    "stringr",
    "tibble",
    "tidyr"
  )

  for (package_name in required_packages) {
    suppressPackageStartupMessages(
      library(package_name, character.only = TRUE)
    )
  }

  script_dir <- getwd()
  r_dir <- file.path(script_dir, "R")

  for (file_name in fallback_source_order) {
    source(file.path(r_dir, file_name), local = .GlobalEnv)
  }

  if (!exists("GeoLift", mode = "function")) {
    stop("Failed to load GeoLift runtime from installed package or local R sources.")
  }

  invisible(TRUE)
}

load_geolift_runtime()

default_input <- list(
  config = list(
    seed = 11,
    num_locations = 10,
    num_periods = 40,
    treatment_locations = c("geo_1", "geo_2"),
    treatment_start_time = 31,
    treatment_end_time = 36,
    lift_pct = 0.12,
    model = "none",
    alpha = 0.1,
    stat_test = "Total",
    confidence_intervals = FALSE
  )
)

deep_merge <- function(base, override) {
  merged <- base
  for (name in names(override)) {
    value <- override[[name]]
    if (is.list(value) && !is.null(merged[[name]]) && is.list(merged[[name]])) {
      merged[[name]] <- deep_merge(merged[[name]], value)
    } else {
      merged[[name]] <- value
    }
  }
  merged
}

load_input_json <- function(path) {
  if (identical(path, "-")) {
    return(fromJSON(file("stdin"), simplifyVector = FALSE))
  }
  fromJSON(path, simplifyVector = FALSE)
}

normalize_config <- function(config) {
  config$seed <- as.integer(config$seed)
  config$num_locations <- as.integer(config$num_locations)
  config$num_periods <- as.integer(config$num_periods)
  config$treatment_start_time <- as.integer(config$treatment_start_time)
  config$treatment_end_time <- as.integer(config$treatment_end_time)
  config$lift_pct <- as.numeric(config$lift_pct)
  config$alpha <- as.numeric(config$alpha)
  config$model <- as.character(config$model)
  config$stat_test <- as.character(config$stat_test)
  config$confidence_intervals <- isTRUE(config$confidence_intervals)
  config$treatment_locations <- as.character(unlist(config$treatment_locations))
  config
}

generate_demo_data <- function(config) {
  set.seed(config$seed)
  locations <- paste0("geo_", seq_len(config$num_locations))
  time <- seq_len(config$num_periods)
  panel <- expand.grid(location = locations, time = time, KEEP.OUT.ATTRS = FALSE)
  geo_index <- match(panel$location, locations)
  base_level <- 180 + geo_index * 9
  trend <- panel$time * 1.8
  seasonality <- 12 * sin(panel$time / 3)
  noise <- rnorm(nrow(panel), mean = 0, sd = 6)
  panel$Y <- round(pmax(base_level + trend + seasonality + noise, 1), 2)

  treated_rows <- panel$location %in% config$treatment_locations &
    panel$time >= config$treatment_start_time &
    panel$time <= config$treatment_end_time
  panel$Y[treated_rows] <- round(panel$Y[treated_rows] * (1 + config$lift_pct), 2)
  panel
}

top_weights <- function(weight_frame, limit = 5) {
  ordered <- weight_frame[order(abs(weight_frame$weight), decreasing = TRUE), ]
  ordered <- ordered[abs(ordered$weight) >= 1e-4, ]
  if (nrow(ordered) == 0) {
    return(list())
  }
  ordered <- head(ordered, limit)
  lapply(seq_len(nrow(ordered)), function(i) {
    list(
      location = as.character(ordered$location[i]),
      weight = unname(as.numeric(ordered$weight[i]))
    )
  })
}

run_model <- function(params) {
  merged <- deep_merge(default_input, params)
  config <- normalize_config(merged$config)
  demo_data <- generate_demo_data(config)

  geolift_result <- GeoLift(
    Y_id = "Y",
    data = demo_data,
    locations = config$treatment_locations,
    treatment_start_time = config$treatment_start_time,
    treatment_end_time = config$treatment_end_time,
    alpha = config$alpha,
    model = config$model,
    ConfidenceIntervals = config$confidence_intervals,
    stat_test = config$stat_test
  )

  summary_obj <- summary(geolift_result)
  observed_treatment <- subset(
    demo_data,
    location %in% config$treatment_locations &
      time >= config$treatment_start_time &
      time <= config$treatment_end_time
  )

  list(
    model = "geolift",
    data_source = "synthetic_demo",
    config = config,
    treatment = list(
      locations = config$treatment_locations,
      treatment_start_time = config$treatment_start_time,
      treatment_end_time = config$treatment_end_time,
      observed_treatment_outcome = unname(sum(observed_treatment$Y))
    ),
    results = list(
      average_att = unname(as.numeric(summary_obj$ATT_est)),
      percent_lift = unname(as.numeric(summary_obj$PercLift)),
      incremental_outcome = unname(as.numeric(summary_obj$incremental)),
      p_value = unname(as.numeric(summary_obj$pvalue)),
      scaled_l2_imbalance = unname(as.numeric(summary_obj$L2ImbalanceScaled)),
      significant_at_90pct = isTRUE(summary_obj$pvalue <= 0.1)
    ),
    donor_weights = top_weights(summary_obj$weights, limit = 5)
  )
}

args <- commandArgs(trailingOnly = TRUE)
input_path <- if (length(args) > 0) args[[1]] else "-"
result <- run_model(load_input_json(input_path))
cat(toJSON(result, auto_unbox = TRUE, pretty = TRUE))
cat("\n")
