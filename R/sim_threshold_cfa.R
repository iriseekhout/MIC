#' Estimate a CFA-based interpretation threshold for a continuous single-item measure
#'
#' `sim_threshold_cfa()` estimates an interpretation threshold for a continuous
#' single-item measure using a confirmatory factor analysis approach.
#'
#' The technique is based on:
#'
#' Terluin, B., Pua, Y.H., Fromy, P. et al. Estimating the minimal important
#' change of single-item measures using the adjusted predictive modeling method
#' or the longitudinal confirmatory factor analysis method. Qual Life Res 35,
#' 39 (2026). DOI: 10.1007/s11136-025-04134-3
#'
#' The `sim_threshold_cfa()` function:
#' \enumerate{
#'   \item discretizes a continuous SIM into equal-width ordered categories;
#'   \item fits a one-factor CFA model with an anchor transition item;
#'   \item computes `theta_star`, the latent factor value where the anchor
#'         threshold occurs;
#'   \item maps `theta_star` back to the original SIM scale using CFA-implied
#'         category probabilities and bin midpoints.
#' }
#'
#' @param mydata Data frame.
#' @param var_formula Formula of the form `tr ~ sim + aux1 + aux2 + ...`.
#'   The left-hand side is the transition or anchor variable. The first
#'   right-hand side variable is the continuous SIM. Remaining right-hand side
#'   variables are auxiliary CFA indicators.
#' @param sim_levels Number of equal-width levels for discretizing the SIM.
#'   Must be between 2 and 12.
#' @param ordered Additional auxiliary variables to treat as ordered in lavaan.
#' @param add_lmodel Optional lavaan syntax appended to the generated model.
#' @param tr_cut Cutpoint for binarizing the transition variable when it has
#'   more than two unique non-missing values. Values >= `tr_cut` are coded 1.
#'   If the transition variable already has exactly two unique values, `tr_cut`
#'   is ignored.
#' @param B Number of bootstrap samples. Bootstrap CI is computed only if
#'   `B >= 100`.
#' @param report_every During bootstrapping, print progress every
#'   `report_every` attempted fits.
#' @param require_all_sim_levels Passed to `var_discretize()`.
#' @param factor_name Name of the latent factor.
#' @param sim_suffix Suffix appended to the SIM variable name after
#'   discretization.
#' @param std.lv Passed to `lavaan::cfa()`.
#' @param parameterization Passed to `lavaan::cfa()`.
#' @param verbose If `TRUE`, print generated lavaan model.
#' @param ... Additional arguments passed to `lavaan::cfa()`.
#'
#' @return A `sim_threshold_cfa` object. The printed output is compact.
#'   Additional details can be retrieved with `sim_threshold_details()`.
#'
#' @export
sim_threshold_cfa <- function(
    mydata,
    var_formula,
    sim_levels = 10L,
    ordered = NULL,
    add_lmodel = NULL,
    tr_cut = 1,
    B = 0L,
    report_every = 50L,
    require_all_sim_levels = FALSE,
    factor_name = "F1",
    sim_suffix = "_ord",
    std.lv = TRUE,
    parameterization = "theta",
    verbose = FALSE,
    ...
) {

  if (!inherits(var_formula, "formula")) {
    stop("`var_formula` must be a formula, e.g. tr ~ sim + aux1 + aux2.", call. = FALSE)
  }

  if (length(var_formula) != 3L) {
    stop("`var_formula` must have both left-hand and right-hand sides.", call. = FALSE)
  }

  tr_var <- all.vars(var_formula[[2L]])

  if (length(tr_var) != 1L) {
    stop("The left-hand side of `var_formula` must contain exactly one variable.", call. = FALSE)
  }

  rhs_vars <- all.vars(var_formula[[3L]])

  if (length(rhs_vars) < 1L) {
    stop("The right-hand side of `var_formula` must contain at least the SIM variable.", call. = FALSE)
  }

  sim_var <- rhs_vars[1L]
  aux_vars <- rhs_vars[-1L]

  needed_vars <- c(tr_var, sim_var, aux_vars)
  missing_vars <- setdiff(needed_vars, names(mydata))

  if (length(missing_vars) > 0L) {
    stop(
      "Variables not found in `mydata`: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.numeric(mydata[[sim_var]])) {
    stop("SIM variable `", sim_var, "` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(mydata[[tr_var]])) {
    stop("Transition variable `", tr_var, "` must be numeric.", call. = FALSE)
  }

  if (!is.null(ordered)) {
    missing_ordered <- setdiff(ordered, aux_vars)

    if (length(missing_ordered) > 0L) {
      stop(
        "Variables in `ordered` must be auxiliary variables in `var_formula`. ",
        "Not found among auxiliary variables: ",
        paste(missing_ordered, collapse = ", "),
        call. = FALSE
      )
    }
  }

  report_every <- as.integer(report_every)

  if (is.na(report_every) || report_every < 1L) {
    stop("`report_every` must be a positive integer.", call. = FALSE)
  }

  sim_obs_name <- paste0(sim_var, sim_suffix)

  if (sim_obs_name %in% c(tr_var, aux_vars)) {
    stop(
      "The generated discretized SIM name `", sim_obs_name,
      "` conflicts with an existing CFA variable name. ",
      "Use a different `sim_suffix`.",
      call. = FALSE
    )
  }

  fit_once <- function(dat, keep_fit = TRUE, verbose_fit = FALSE) {

    dat <- as.data.frame(dat)

    # ---- transition / anchor variable ----
    tr_x <- dat[[tr_var]]
    tr_vals <- sort(unique(tr_x[!is.na(tr_x)]))

    if (length(tr_vals) < 2L) {
      stop("Transition variable must have at least two observed values.", call. = FALSE)
    }

    if (length(tr_vals) == 2L) {

      # Already binary in the sense of having two observed values.
      # Keep the original variable name in the CFA data.
      tr_obs_name <- tr_var

      tr_bin <- ifelse(
        is.na(tr_x),
        NA_integer_,
        as.integer(tr_x == tr_vals[2L])
      )

    } else {

      # Only apply tr_cut when the transition variable has more than two
      # unique observed values.
      tr_obs_name <- paste0(tr_var, "_bin")

      tr_bin <- ifelse(
        is.na(tr_x),
        NA_integer_,
        as.integer(tr_x >= tr_cut)
      )
    }

    if (length(unique(tr_bin[!is.na(tr_bin)])) < 2L) {
      stop(
        "Binarized transition variable has fewer than two observed categories.",
        call. = FALSE
      )
    }

    if (tr_obs_name %in% c(sim_obs_name, aux_vars)) {
      stop(
        "The anchor variable name `", tr_obs_name,
        "` conflicts with another CFA variable name.",
        call. = FALSE
      )
    }

    # ---- discretize SIM ----
    sim_disc <- var_discretize(
      dat[[sim_var]],
      n_levels = sim_levels,
      require_all_levels = require_all_sim_levels,
      warn_unused = !require_all_sim_levels
    )

    # ---- CFA-only data frame ----
    cfa_data <- data.frame(
      sim_value = sim_disc$score,
      anchor_value = tr_bin,
      check.names = FALSE
    )

    names(cfa_data) <- c(sim_obs_name, tr_obs_name)

    if (length(aux_vars) > 0L) {
      cfa_data <- cbind(
        cfa_data,
        dat[aux_vars]
      )
    }

    # ---- lavaan model ----
    indicators <- c(
      sim_obs_name,
      aux_vars,
      paste0("lambda_anchor*", tr_obs_name)
    )

    lmodel <- paste0(
      factor_name, " =~ ", paste(indicators, collapse = " + "), "\n",
      tr_obs_name, " | tau_anchor*t1"
    )

    if (!is.null(add_lmodel)) {
      lmodel <- paste(lmodel, add_lmodel, sep = "\n")
    }

    if (isTRUE(verbose_fit)) {
      message("Generated lavaan model:\n", lmodel)
    }

    lavaan_ordered <- unique(c(sim_obs_name, tr_obs_name, ordered))

    fit <- lavaan::cfa(
      model = lmodel,
      data = cfa_data,
      std.lv = std.lv,
      parameterization = parameterization,
      ordered = lavaan_ordered,
      ...
    )

    theta_star <- cfa_theta_star(
      fit,
      threshold_label = "tau_anchor",
      loading_label = "lambda_anchor"
    )

    probs <- cfa_cprob(
      fit = fit,
      item = sim_obs_name,
      theta = theta_star,
      factor_name = factor_name,
      item_levels = sim_disc$used_levels
    )

    mids <- sim_disc$midpoints
    common_levels <- intersect(names(probs), names(mids))

    if (length(common_levels) == 0L) {
      stop("Could not align CFA probabilities with SIM bin midpoints.", call. = FALSE)
    }

    threshold <- sum(probs[common_levels] * mids[common_levels])

    list(
      threshold = as.numeric(threshold),
      theta_star = as.numeric(theta_star),
      probs = probs,
      midpoints = mids,
      sim_discretized = sim_disc,
      lavaan_model = lmodel,
      cfa_data = cfa_data,
      fit = if (isTRUE(keep_fit)) fit else NULL,
      sim_var = sim_var,
      sim_obs_name = sim_obs_name,
      tr_var = tr_var,
      tr_obs_name = tr_obs_name,
      aux_vars = aux_vars,
      ordered = lavaan_ordered
    )
  }

  main <- fit_once(
    dat = mydata,
    keep_fit = TRUE,
    verbose_fit = verbose
  )

  boot_thresholds <- NULL
  boot_success <- NULL
  ci <- NULL

  if (!is.null(B) && B >= 100L) {

    B <- as.integer(B)
    n <- nrow(mydata)

    boot_thresholds <- rep(NA_real_, B)
    boot_success <- rep(FALSE, B)

    message("Starting bootstrap with B = ", B, ".")

    for (b in seq_len(B)) {

      idx <- sample.int(n, size = n, replace = TRUE)
      dat_b <- mydata[idx, , drop = FALSE]

      boot_out <- tryCatch(
        fit_once(
          dat = dat_b,
          keep_fit = FALSE,
          verbose_fit = FALSE
        ),
        error = function(e) NULL
      )

      if (!is.null(boot_out)) {
        boot_thresholds[b] <- boot_out$threshold
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

    ci <- stats::quantile(
      boot_thresholds,
      probs = c(0.025, 0.975),
      na.rm = TRUE,
      names = TRUE
    )
  }

  out <- list(
    threshold = main$threshold,
    ci = ci,
    sim_original = mydata[[sim_var]],
    sim_discrete = main$cfa_data[[main$sim_obs_name]],
    sim_midpoints = main$midpoints,
    lavaan_model = main$lavaan_model
  )

  attr(out, "details") <- list(
    call = match.call(),
    fit = main$fit,
    cfa_data = main$cfa_data,
    theta_star = main$theta_star,
    probs = main$probs,
    sim_discretized = main$sim_discretized,
    sim_var = main$sim_var,
    sim_obs_name = main$sim_obs_name,
    tr_var = main$tr_var,
    tr_obs_name = main$tr_obs_name,
    aux_vars = main$aux_vars,
    ordered = main$ordered,
    boot_thresholds = boot_thresholds,
    boot_success = boot_success
  )

  class(out) <- "sim_threshold_cfa"

  out
}












#' @export
print.sim_threshold_cfa <- function(x, ...) {

  cat("CFA-based SIM threshold\n")
  cat("------------------------\n")
  cat("Threshold:", round(x$threshold, 4), "\n")

  if (!is.null(x$ci)) {
    cat(
      "95% CI:",
      round(x$ci[1L], 4),
      "to",
      round(x$ci[2L], 4),
      "\n"
    )
  }

  details <- attr(x, "details")

  if (!is.null(details)) {
    cat("SIM variable:", details$sim_var, "\n")
    cat("Discretized SIM variable:", details$sim_obs_name, "\n")
    cat("Anchor variable:", details$tr_var, "\n")
    cat("Anchor CFA variable:", details$tr_obs_name, "\n")
  }

  cat("\nUse `sim_threshold_details(x)` to retrieve the lavaan fit, CFA data, probabilities, and bootstrap results.\n")

  invisible(x)
}










#' Extract details from a SIM threshold object
#'
#' `sim_threshold_details()` extracts additional details from an object returned
#' by `sim_threshold_cfa()`, including the fitted lavaan model, CFA data,
#' latent anchor location, model-implied category probabilities, and bootstrap
#' results.
#'
#' @param x A `sim_threshold_cfa` object.
#'
#' @return A list of additional model details.
#'
sim_threshold_details <- function(x) {

  if (!inherits(x, "sim_threshold_cfa")) {
    stop("`x` must be a sim_threshold_cfa object.", call. = FALSE)
  }

  attr(x, "details")
}
