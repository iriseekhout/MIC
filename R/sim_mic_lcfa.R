#' LCFA-Based MIC for a Single-Item Measure
#'
#' `sim_mic_lcfa()` estimates the minimal important change (MIC) for a
#' single-item measure (SIM) using longitudinal confirmatory factor analysis
#' with an auxiliary variable.
#'
#' The function implements a Model C-type approach (see reference) for SIMs. The SIM is measured
#' at Time 1 and Time 2, an auxiliary variable is measured at Time 1 and Time 2,
#' and a transition rating is included as an anchor.
#'
#' If the pooled SIM values across Time 1 and Time 2 have 12 or fewer observed
#' levels, the original SIM values are used as ordered categories. If the pooled
#' SIM values have more than 12 observed levels, the SIM is discretized using
#' `var_discretize()` with common equal-width bins across Time 1 and Time 2.
#' In both cases, SIM response levels are equalized across time before fitting
#' the LCFA model.
#'
#' The SIM loading and SIM thresholds are constrained equal over time so that
#' the Time 1 and Time 2 latent factors are on the same measurement scale.
#'
#' The latent MIC is estimated as `MIC.theta = tau_trt / f2`, where `tau_trt` is
#' the transition-rating threshold and `f2` is the transition-rating loading on
#' the Time 2 factor.
#'
#' The SIM-scale MIC is first calculated on the transformed SIM scale as
#' `sqrt(var(SIM_T1_transformed) * Rel_SIM_T1) * MIC.theta`, where
#' `Rel_SIM_T1` is the model R-squared of the Time 1 SIM. If `var_discretize()`
#' is used, the transformed-scale MIC is then back-transformed to the original
#' SIM metric using the equal-width bin width.
#'
#' @details
#' Continuous SIMs with many distinct values are not used directly as ordered
#' indicators. When discretization is needed, `sim_mic_lcfa()` applies
#' `var_discretize()` to the pooled Time 1 and Time 2 SIM values. This ensures
#' that the same equal-width binning rule is applied at both time points.
#'
#' The LCFA model is fitted to the discretized/equalized ordered SIM variables.
#' Therefore, the reliability of the SIM, `Rel_SIM_T1`, is the reliability of
#' the transformed SIM used in the CFA model. For this reason, the SIM MIC is
#' first computed on the transformed SIM scale:
#'
#' `MIC_SIM_transformed = sqrt(var(SIM_T1_transformed) * Rel_SIM_T1) * MIC.theta`.
#'
#' If discretization was used, this transformed-scale MIC is then converted back
#' to the original SIM metric using:
#'
#' `MIC_SIM_original = MIC_SIM_transformed * backtransform_factor`,
#'
#' where `backtransform_factor` is the equal-width bin width, calculated as the
#' pooled original SIM range divided by the number of requested discretized
#' levels. If no discretization is used, `backtransform_factor = 1`.
#'
#'
#' @param mydata Data frame.
#' @param sim Character vector of length 2. Names of the SIM variable at Time 1
#'   and Time 2.
#' @param aux Character vector of length 2. Names of the auxiliary variable at
#'   Time 1 and Time 2.
#' @param trt Character. Name of the transition rating / anchor variable.
#' @param trt_cut Numeric. Cutpoint used to binarize `trt` if it has more than
#'   two observed values. Values greater than or equal to `trt_cut` are coded 1.
#'   If `trt` already has exactly two observed values, the larger value is coded
#'   as 1 and `trt_cut` is ignored.
#' @param sim_levels Integer. Number of ordered levels used when discretizing
#'   the SIM with `var_discretize()`. Must be between 2 and 12. Default is 10.
#' @param sim_discretize Character. One of `"auto"`, `"yes"`, or `"no"`.
#'   `"auto"` applies `var_discretize()` only when the pooled SIM has more than
#'   12 observed levels. `"yes"` always applies `var_discretize()`. `"no"` never
#'   discretizes and errors if the pooled SIM has more than 12 observed levels.
#' @param min_resp Integer. Minimum response count per category used by
#'   `equalize_levels()` when equalizing SIM response levels across time.
#' @param aux_ordered Logical. If `TRUE`, treats the auxiliary variables as
#'   ordered indicators in lavaan. Auxiliary variable loadings and thresholds
#'   are freely estimated over time.
#' @param B Integer. Number of nonparametric bootstrap samples. Bootstrap
#'   confidence intervals are computed only when `B >= 100`.
#' @param report_every Integer. Print bootstrap progress every `report_every`
#'   attempted bootstrap fits.
#' @param seed Optional integer seed for reproducibility.
#' @param add_lmodel Optional lavaan syntax appended to the generated model.
#' @param print_model Logical. If `TRUE`, prints the generated lavaan model.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#' @param ... Additional arguments passed to `lavaan::cfa()`.
#'
#' @return A `sim_mic_lcfa` object with elements including:
#' \itemize{
#'   \item `MIC.theta`: MIC on the latent-change scale;
#'   \item `MIC.sim.transformed`: MIC on the transformed SIM scale;
#'   \item `MIC.sim`: MIC on the original SIM metric;
#'   \item `rel_SIM1`: model R-squared / reliability of SIM at Time 1;
#'   \item `rel_trt`: model R-squared / reliability of the transition rating;
#'   \item `psb`: estimated present-state bias;
#'   \item `ci`: bootstrap confidence intervals, if requested;
#'   \item `sim_prepared`: information about discretization, equalization, and
#'   back-transformation.
#' }
#'
#' @references
#' Terluin B, Pua YH, Fromy P, Trigg A, van der Zwaard B, Bjorner JB.
#' Estimating the minimal important change of single-item measures using the
#' adjusted predictive modeling method or the longitudinal confirmatory factor
#' analysis method. Quality of Life Research. 2026.
#' doi:10.1007/s11136-025-04134-3
#'
#' Terluin B, Trigg A, Fromy P, Schuller W, Terwee CB, Bjorner JB.
#' Estimating anchor-based minimal important change using longitudinal
#' confirmatory factor analysis. Qual Life Res. 2024;33:963-973.
#' doi:10.1007/s11136-023-03577-w
#'
#' @examples
#' \dontrun{
#' sim <- simdat(N = 500, seed = 123, add_change = TRUE)
#' dat <- sim$datw
#'
#' # Create a toy single-item measure from several items
#' dat$sim_t1 <- rowSums(dat[, paste0("item", 1:8), drop = FALSE])
#' dat$sim_t2 <- rowSums(dat[, paste0("item", 1:8, ".1"), drop = FALSE])
#'
#' out <- sim_mic_lcfa(
#'   mydata = dat,
#'   sim = c("sim_t1", "sim_t2"),
#'   aux = c("item9", "item9.1"),
#'   trt = "trat",
#'   sim_discretize = "auto",
#'   sim_levels = 10,
#'   aux_ordered = TRUE,
#'   B = 0,
#'   print_model = TRUE
#' )
#'
#' out
#'
#' # Inspect how the SIM was prepared
#' out$sim_prepared$used_discretization
#' out$sim_prepared$backtransform_factor
#' out$sim_prepared$original_levels
#' }
#'
#' @seealso [var_discretize()], [equalize_levels()], [mic_lcfa()], [simdat()]
#'
#' @export
sim_mic_lcfa <- function(
    mydata,
    sim,
    aux,
    trt,
    trt_cut = 1,
    sim_levels = 10L,
    sim_discretize = c("auto", "yes", "no"),
    min_resp = 1L,
    aux_ordered = FALSE,
    B = 0L,
    report_every = 50L,
    seed = NULL,
    add_lmodel = NULL,
    print_model = FALSE,
    verbose = FALSE,
    ...
) {

  sim_discretize <- match.arg(sim_discretize)

  # -------------------------------------------------------------------------
  # Argument checks
  # -------------------------------------------------------------------------

  if (!is.data.frame(mydata)) {
    stop("`mydata` must be a data frame.", call. = FALSE)
  }

  if (!is.character(sim) || length(sim) != 2L) {
    stop(
      "`sim` must be a character vector of length 2: Time 1 and Time 2 SIM variables.",
      call. = FALSE
    )
  }

  if (!is.character(aux) || length(aux) != 2L) {
    stop(
      "`aux` must be a character vector of length 2: Time 1 and Time 2 auxiliary variables.",
      call. = FALSE
    )
  }

  if (!is.character(trt) || length(trt) != 1L) {
    stop("`trt` must be a single character string.", call. = FALSE)
  }

  needed <- c(sim, aux, trt)
  missing_vars <- setdiff(needed, names(mydata))

  if (length(missing_vars) > 0L) {
    stop(
      "Variables not found in `mydata`: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.numeric(mydata[[sim[1L]]]) || !is.numeric(mydata[[sim[2L]]])) {
    stop("Both variables in `sim` must be numeric.", call. = FALSE)
  }

  if (!is.logical(aux_ordered) || length(aux_ordered) != 1L ||
      is.na(aux_ordered)) {
    stop("`aux_ordered` must be either TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(sim_levels) || length(sim_levels) != 1L ||
      is.na(sim_levels) || !is.finite(sim_levels) ||
      sim_levels < 2L || sim_levels > 12L ||
      sim_levels != floor(sim_levels)) {
    stop("`sim_levels` must be an integer between 2 and 12.", call. = FALSE)
  }

  sim_levels <- as.integer(sim_levels)

  if (!is.numeric(min_resp) || length(min_resp) != 1L ||
      is.na(min_resp) || !is.finite(min_resp) ||
      min_resp < 1L || min_resp != floor(min_resp)) {
    stop("`min_resp` must be a positive integer.", call. = FALSE)
  }

  min_resp <- as.integer(min_resp)

  if (!is.numeric(B) || length(B) != 1L || is.na(B) ||
      !is.finite(B) || B < 0 || B != floor(B)) {
    stop("`B` must be a single non-negative integer.", call. = FALSE)
  }

  B <- as.integer(B)

  if (!is.numeric(report_every) || length(report_every) != 1L ||
      is.na(report_every) || !is.finite(report_every) ||
      report_every < 1 || report_every != floor(report_every)) {
    stop("`report_every` must be a single positive integer.", call. = FALSE)
  }

  report_every <- as.integer(report_every)

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
        !is.finite(seed) || seed != floor(seed)) {
      stop("`seed` must be NULL or a single integer.", call. = FALSE)
    }
  }

  # -------------------------------------------------------------------------
  # Helper: prepare SIM pair
  # -------------------------------------------------------------------------

  prepare_sim_pair <- function(data) {

    sim_t1 <- data[[sim[1L]]]
    sim_t2 <- data[[sim[2L]]]

    pooled <- c(sim_t1, sim_t2)
    pooled_nonmiss <- pooled[!is.na(pooled)]
    pooled_levels <- sort(unique(pooled_nonmiss))

    if (length(pooled_levels) < 2L) {
      stop(
        "The pooled SIM variables have fewer than two observed values.",
        call. = FALSE
      )
    }

    pooled_range <- diff(range(pooled_nonmiss, na.rm = TRUE))

    if (!is.finite(pooled_range) || pooled_range <= 0) {
      stop("The pooled SIM range must be positive.", call. = FALSE)
    }

    use_discretize <- switch(
      sim_discretize,
      yes = TRUE,
      no = FALSE,
      auto = length(pooled_levels) > 12L
    )

    if (!use_discretize && length(pooled_levels) > 12L) {
      stop(
        "The pooled SIM variables have more than 12 observed levels. ",
        "Use `sim_discretize = 'yes'` or `sim_discretize = 'auto'`.",
        call. = FALSE
      )
    }

    n_t1 <- length(sim_t1)

    if (use_discretize) {

      # Common equal-width bins across Time 1 and Time 2.
      disc <- var_discretize(
        pooled,
        n_levels = sim_levels,
        require_all_levels = FALSE,
        warn_unused = TRUE
      )

      sim1_raw_cat <- disc$score[seq_len(n_t1)]
      sim2_raw_cat <- disc$score[n_t1 + seq_along(sim_t2)]

      # One transformed unit corresponds to this many original SIM units.
      backtransform_factor <- pooled_range / sim_levels

      discretization <- disc

    } else {

      # Original SIM values are already suitable ordered categories.
      sim1_raw_cat <- sim_t1
      sim2_raw_cat <- sim_t2

      backtransform_factor <- 1
      discretization <- NULL
    }

    dat_sim <- data.frame(
      SIM1 = sim1_raw_cat,
      SIM2 = sim2_raw_cat
    )

    # Equalize response levels across time.
    if (isTRUE(verbose)) {
      eq <- equalize_levels(
        data = dat_sim,
        pair_by = "position",
        min_resp = min_resp,
        verbose = TRUE,
        print_tables = FALSE
      )
    } else {
      eq <- suppressMessages(
        equalize_levels(
          data = dat_sim,
          pair_by = "position",
          min_resp = min_resp,
          verbose = FALSE,
          print_tables = FALSE
        )
      )
    }

    dat_eq <- eq$data

    sim1_levels <- sort(unique(dat_eq$SIM1[!is.na(dat_eq$SIM1)]))
    sim2_levels <- sort(unique(dat_eq$SIM2[!is.na(dat_eq$SIM2)]))

    if (!identical(sim1_levels, sim2_levels)) {
      stop(
        "SIM1 and SIM2 do not have the same observed response levels after equalization.",
        call. = FALSE
      )
    }

    if (length(sim1_levels) < 2L) {
      stop(
        "Equalized SIM has fewer than two observed response levels.",
        call. = FALSE
      )
    }

    # Transformed scale used for MIC conversion.
    # This preserves the transformed/original score spacing before lavaan recoding.
    SIM1_transformed <- dat_eq$SIM1
    SIM2_transformed <- dat_eq$SIM2

    # Recode only for lavaan. This gives clean consecutive ordinal categories.
    final_levels <- sim1_levels

    recode_to_consecutive <- function(x) {
      as.integer(match(x, final_levels))
    }

    SIM1_lavaan <- recode_to_consecutive(SIM1_transformed)
    SIM2_lavaan <- recode_to_consecutive(SIM2_transformed)

    list(
      data = data.frame(
        SIM1 = SIM1_lavaan,
        SIM2 = SIM2_lavaan,
        SIM1_transformed = SIM1_transformed,
        SIM2_transformed = SIM2_transformed
      ),
      original_levels = final_levels,
      lavaan_levels = seq_along(final_levels),
      n_thresholds = length(final_levels) - 1L,
      used_discretization = use_discretize,
      pooled_range = pooled_range,
      backtransform_factor = backtransform_factor,
      discretization = discretization,
      equalization = eq
    )
  }

  # -------------------------------------------------------------------------
  # Helper: equal SIM threshold constraints
  # -------------------------------------------------------------------------

  make_equal_sim_thresholds <- function(n_thresholds) {

    if (!is.numeric(n_thresholds) || length(n_thresholds) != 1L ||
        is.na(n_thresholds) || n_thresholds < 1L ||
        n_thresholds != floor(n_thresholds)) {
      stop("`n_thresholds` must be a positive integer.", call. = FALSE)
    }

    n_thresholds <- as.integer(n_thresholds)

    threshold_terms <- paste0(
      "b",
      seq_len(n_thresholds),
      "*t",
      seq_len(n_thresholds)
    )

    paste0(
      "SIM1 | ", paste(threshold_terms, collapse = " + "), "\n",
      "SIM2 | ", paste(threshold_terms, collapse = " + ")
    )
  }

  # -------------------------------------------------------------------------
  # Helper: build lavaan model
  # -------------------------------------------------------------------------

  build_model <- function(n_thresholds) {

    sim_thresholds <- make_equal_sim_thresholds(
      n_thresholds = n_thresholds
    )

    model <- paste0(
      "# Factors\n",
      "F1 =~ a_sim*SIM1 + aux1 + f1*trt\n",
      "F2 =~ a_sim*SIM2 + aux2 + f2*trt\n\n",

      "# Equal SIM thresholds over time\n",
      sim_thresholds, "\n\n",

      "# Correlated residuals over time\n",
      "SIM1 ~~ SIM2\n",
      "aux1 ~~ aux2\n\n",

      "# Variances/covariances\n",
      "F1 ~~ 1*F1\n",
      "F2 ~~ NA*F2 + var_F2*F2\n",
      "F1 ~~ cov_F1F2*F2\n\n",

      "# Free residual variance of SIM2 under theta parameterization\n",
      "SIM2 ~~ NA*SIM2\n\n",

      "# Means\n",
      "F1 ~ 0*1\n",
      "F2 ~ mn_ch*1\n\n",

      "# Transition-rating threshold\n",
      "trt | tau_trt*t1\n\n",

      "# Derived values\n",
      "mn_change := mn_ch\n",
      "sd_change := sqrt(1 + var_F2 - 2*cov_F1F2)\n",
      "MIC.theta := tau_trt / f2\n",
      "psb := f1 / f2 + 1\n"
    )

    if (!is.null(add_lmodel)) {
      model <- paste(model, add_lmodel, sep = "\n")
    }

    model
  }

  # -------------------------------------------------------------------------
  # Helper: one model fit
  # -------------------------------------------------------------------------

  fit_once <- function(dat, keep_fit = TRUE, print_model_once = FALSE) {

    dat <- as.data.frame(dat)

    dat <- dat[
      stats::complete.cases(dat[, needed, drop = FALSE]),
      needed,
      drop = FALSE
    ]

    if (nrow(dat) < 20L) {
      stop(
        "Too few complete observations after removing missing values.",
        call. = FALSE
      )
    }

    # Transition rating
    trt_raw <- dat[[trt]]

    if (is.logical(trt_raw)) {
      trt_raw <- as.integer(trt_raw)
    }

    if (!is.numeric(trt_raw) && !is.integer(trt_raw)) {
      stop("`trt` must be numeric, integer, or logical.", call. = FALSE)
    }

    trt_vals <- sort(unique(trt_raw[!is.na(trt_raw)]))

    if (length(trt_vals) < 2L) {
      stop("`trt` must have at least two observed values.", call. = FALSE)
    }

    if (length(trt_vals) == 2L) {
      trt_bin <- ifelse(
        is.na(trt_raw),
        NA_integer_,
        as.integer(trt_raw == trt_vals[2L])
      )
    } else {
      trt_bin <- ifelse(
        is.na(trt_raw),
        NA_integer_,
        as.integer(trt_raw >= trt_cut)
      )
    }

    if (length(unique(trt_bin[!is.na(trt_bin)])) < 2L) {
      stop(
        "Binarized `trt` has fewer than two observed categories.",
        call. = FALSE
      )
    }

    # Prepare SIM
    prep <- prepare_sim_pair(dat)

    cfa_data <- data.frame(
      SIM1 = prep$data$SIM1,
      SIM2 = prep$data$SIM2,
      aux1 = dat[[aux[1L]]],
      aux2 = dat[[aux[2L]]],
      trt = trt_bin
    )

    model <- build_model(
      n_thresholds = prep$n_thresholds
    )

    if (isTRUE(print_model_once)) {
      cat("\n================ SIM MIC LCFA MODEL ================\n\n")
      cat(model)
      cat("\n====================================================\n")
    }

    lavaan_ordered <- c("SIM1", "SIM2", "trt")

    if (isTRUE(aux_ordered)) {
      lavaan_ordered <- c(lavaan_ordered, "aux1", "aux2")
    }

    fit <- lavaan::cfa(
      model = model,
      data = cfa_data,
      std.lv = TRUE,
      ordered = lavaan_ordered,
      parameterization = "theta",
      ...
    )

    converged <- tryCatch(
      lavaan::lavInspect(fit, "converged"),
      error = function(e) FALSE
    )

    if (!isTRUE(converged)) {
      stop("lavaan model did not converge.", call. = FALSE)
    }

    pe <- lavaan::parameterEstimates(
      fit,
      standardized = TRUE,
      rsquare = TRUE
    )

    MIC.theta <- pe$est[pe$label == "MIC.theta"]
    psb <- pe$est[pe$label == "psb"]
    mn_change <- pe$est[pe$label == "mn_change"]
    sd_change <- pe$est[pe$label == "sd_change"]

    rel_SIM1 <- pe$est[
      pe$lhs == "SIM1" &
        pe$op == "r2"
    ]

    rel_trt <- pe$est[
      pe$lhs == "trt" &
        pe$op == "r2"
    ]

    if (length(MIC.theta) != 1L || !is.finite(MIC.theta)) {
      stop("Could not extract finite `MIC.theta`.", call. = FALSE)
    }

    if (length(rel_SIM1) != 1L || !is.finite(rel_SIM1)) {
      stop("Could not extract finite R-squared for SIM1.", call. = FALSE)
    }

    if (length(rel_trt) != 1L || !is.finite(rel_trt)) {
      rel_trt <- NA_real_
    }

    if (length(psb) != 1L || !is.finite(psb)) {
      psb <- NA_real_
    }

    if (length(mn_change) != 1L || !is.finite(mn_change)) {
      mn_change <- NA_real_
    }

    if (length(sd_change) != 1L || !is.finite(sd_change)) {
      sd_change <- NA_real_
    }

    # MIC on transformed SIM scale.
    MIC.sim.transformed <- sqrt(
      stats::var(prep$data$SIM1_transformed, na.rm = TRUE) * rel_SIM1
    ) * MIC.theta

    # MIC on original SIM metric.
    MIC.sim <- MIC.sim.transformed * prep$backtransform_factor

    fit_measures <- tryCatch(
      lavaan::fitMeasures(
        fit,
        c("cfi.scaled", "tli.scaled", "rmsea.scaled", "srmr")
      ),
      error = function(e) NULL
    )

    list(
      MIC.theta = as.numeric(MIC.theta),
      MIC.sim.transformed = as.numeric(MIC.sim.transformed),
      MIC.sim = as.numeric(MIC.sim),
      rel_SIM1 = as.numeric(rel_SIM1),
      rel_trt = as.numeric(rel_trt),
      psb = as.numeric(psb),
      mn_change = as.numeric(mn_change),
      sd_change = as.numeric(sd_change),
      fit = if (isTRUE(keep_fit)) fit else NULL,
      parameter_estimates = pe,
      cfa_data = cfa_data,
      sim_prepared = prep,
      lavaan_model = model,
      fit_measures = fit_measures,
      n_complete = nrow(cfa_data)
    )
  }

  # -------------------------------------------------------------------------
  # Reproducibility
  # -------------------------------------------------------------------------

  if (!is.null(seed)) {

    old_seed_exists <- exists(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )

    if (old_seed_exists) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
    }

    set.seed(as.integer(seed))

    on.exit({
      if (old_seed_exists) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
  }

  # -------------------------------------------------------------------------
  # Main fit
  # -------------------------------------------------------------------------

  main <- fit_once(
    dat = mydata,
    keep_fit = TRUE,
    print_model_once = print_model
  )

  # -------------------------------------------------------------------------
  # Bootstrap
  # -------------------------------------------------------------------------

  boot_values <- NULL
  boot_success <- NULL
  ci <- NULL

  if (B > 0L && B < 100L) {
    message(
      "Bootstrap CI not computed because `B < 100`. ",
      "Set `B >= 100` to request bootstrap confidence intervals."
    )
  }

  if (B >= 100L) {

    n <- nrow(mydata)

    boot_values <- data.frame(
      MIC.theta = rep(NA_real_, B),
      MIC.sim.transformed = rep(NA_real_, B),
      MIC.sim = rep(NA_real_, B),
      rel_SIM1 = rep(NA_real_, B),
      rel_trt = rep(NA_real_, B),
      psb = rep(NA_real_, B),
      mn_change = rep(NA_real_, B),
      sd_change = rep(NA_real_, B)
    )

    boot_success <- rep(FALSE, B)

    if (isTRUE(verbose)) {
      message("Starting bootstrap with B = ", B, ".")
    }

    for (b in seq_len(B)) {

      idx <- sample.int(n, size = n, replace = TRUE)
      dat_b <- mydata[idx, , drop = FALSE]

      boot_out <- tryCatch(
        fit_once(
          dat = dat_b,
          keep_fit = FALSE,
          print_model_once = FALSE
        ),
        error = function(e) NULL
      )

      if (!is.null(boot_out)) {
        boot_values$MIC.theta[b] <- boot_out$MIC.theta
        boot_values$MIC.sim.transformed[b] <- boot_out$MIC.sim.transformed
        boot_values$MIC.sim[b] <- boot_out$MIC.sim
        boot_values$rel_SIM1[b] <- boot_out$rel_SIM1
        boot_values$rel_trt[b] <- boot_out$rel_trt
        boot_values$psb[b] <- boot_out$psb
        boot_values$mn_change[b] <- boot_out$mn_change
        boot_values$sd_change[b] <- boot_out$sd_change
        boot_success[b] <- TRUE
      }

      if (b %% report_every == 0L || b == B) {
        message(
          "Bootstrap fit ",
          b,
          " / ",
          B,
          " completed; successful fits = ",
          sum(boot_success),
          "."
        )
      }
    }

    if (sum(boot_success) > 0L) {

      cl <- function(x) {
        stats::quantile(
          x,
          probs = c(0.025, 0.975),
          na.rm = TRUE,
          names = FALSE
        )
      }

      make_ci <- function(estimate, boot) {
        qq <- cl(boot)

        c(
          estimate = estimate,
          lower = qq[1L],
          upper = qq[2L]
        )
      }

      ci <- rbind(
        MIC.theta = make_ci(main$MIC.theta, boot_values$MIC.theta),
        MIC.sim.transformed = make_ci(
          main$MIC.sim.transformed,
          boot_values$MIC.sim.transformed
        ),
        MIC.sim = make_ci(main$MIC.sim, boot_values$MIC.sim),
        rel_SIM1 = make_ci(main$rel_SIM1, boot_values$rel_SIM1),
        rel_trt = make_ci(main$rel_trt, boot_values$rel_trt),
        psb = make_ci(main$psb, boot_values$psb),
        mn_change = make_ci(main$mn_change, boot_values$mn_change),
        sd_change = make_ci(main$sd_change, boot_values$sd_change)
      )

    } else {
      warning(
        "No successful bootstrap fits were obtained.",
        call. = FALSE
      )
    }
  }

  # -------------------------------------------------------------------------
  # Output
  # -------------------------------------------------------------------------

  out <- list(
    MIC.theta = main$MIC.theta,
    MIC.sim.transformed = main$MIC.sim.transformed,
    MIC.sim = main$MIC.sim,
    rel_SIM1 = main$rel_SIM1,
    rel_trt = main$rel_trt,
    psb = main$psb,
    mn_change = main$mn_change,
    sd_change = main$sd_change,
    ci = ci,
    fit_measures = main$fit_measures,
    fit = main$fit,
    parameter_estimates = main$parameter_estimates,
    lavaan_model = main$lavaan_model,
    cfa_data = main$cfa_data,
    sim_prepared = main$sim_prepared,
    boot_values = boot_values,
    boot_success = boot_success,
    n_successful_boot = if (!is.null(boot_success)) sum(boot_success) else 0L,
    n_complete = main$n_complete,
    call = match.call()
  )

  class(out) <- "sim_mic_lcfa"

  out
}


#' @export
print.sim_mic_lcfa <- function(x, digits = 3, ...) {

  fmt <- function(z) {
    if (is.null(z) || length(z) == 0L || is.na(z)) {
      return("NA")
    }

    formatC(z, format = "f", digits = digits)
  }

  cat("LCFA-based MIC for a single-item measure\n")
  cat("----------------------------------------\n")
  cat("MIC theta:", fmt(x$MIC.theta), "\n")
  cat("MIC SIM, transformed scale:", fmt(x$MIC.sim.transformed), "\n")
  cat("MIC SIM, original scale:", fmt(x$MIC.sim), "\n")
  cat("Reliability SIM T1:", fmt(x$rel_SIM1), "\n")
  cat("Reliability transition rating:", fmt(x$rel_trt), "\n")
  cat("Present-state bias:", fmt(x$psb), "\n")
  cat("Mean latent change:", fmt(x$mn_change), "\n")
  cat("SD latent change:", fmt(x$sd_change), "\n")

  if (!is.null(x$sim_prepared)) {
    cat("Discretization used:", x$sim_prepared$used_discretization, "\n")
    cat("Back-transform factor:", fmt(x$sim_prepared$backtransform_factor), "\n")
  }

  if (!is.null(x$fit_measures)) {
    cat("\nFit measures\n")
    cat("------------\n")
    print(round(x$fit_measures, digits))
  }

  if (!is.null(x$ci)) {

    cat("\nBootstrap 95% CIs\n")
    cat("-----------------\n")

    for (i in seq_len(nrow(x$ci))) {
      cat(
        rownames(x$ci)[i],
        ": ",
        fmt(x$ci[i, "lower"]),
        " to ",
        fmt(x$ci[i, "upper"]),
        "\n",
        sep = ""
      )
    }

    cat("\nSuccessful bootstrap fits:", x$n_successful_boot, "\n")
  }

  invisible(x)
}
