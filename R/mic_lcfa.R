#' Estimate present-state bias and anchor-based MIC using longitudinal CFA
#'
#' `mic_lcfa()` estimates:
#'
#' \enumerate{
#'   \item present-state bias in transition ratings, and
#'   \item anchor-based minimal important change (MIC)
#' }
#'
#' using an unconstrained longitudinal confirmatory factor analysis model.
#'
#' The input data should contain paired Time 1 and Time 2 PROM items plus one
#' transition rating variable. Item pairs may be identified by position or by
#' suffixes. By default, paired item levels are first equalized using
#' `equalize_levels()`, and the lavaan syntax is generated using `lcfa_model()`.
#'
#' This implementation follows the updated recommendation that the transition
#' rating loadings should be freely estimated; that is, the constraint
#' `f1 == -f2` is not applied.
#'
#' @references
#' Terluin, B., Fromy, P., Trigg, A., Terwee, C.B., & Bjorner, J.B.
#' Effect of present state bias on minimal important change estimates:
#' a simulation study. Quality of Life Research. 2024.
#' doi:10.1007/s11136-024-03763-4
#'
#' Terluin B, Griffiths P, Trigg A, Terwee CB, Bjorner JB.
#' Present state bias in transition ratings was accurately estimated in
#' simulated and real data. J Clin Epidemiol. 2022;143:128-136.
#' doi:10.1016/j.jclinepi.2021.12.024
#'
#' Terluin B, Trigg A, Fromy P, Schuller W, Terwee CB, Bjorner JB.
#' Estimating anchor-based minimal important change using longitudinal
#' confirmatory factor analysis. Qual Life Res. 2024;33:963-973.
#' doi:10.1007/s11136-023-03577-w
#'
#' @param mydat Data frame containing paired Time 1 and Time 2 PROM items and
#'   one transition rating variable.
#' @param trt Character. Name of the transition rating variable. If `NULL`,
#'   the final column of `mydat` is used.
#' @param model Optional lavaan model syntax. If `NULL`, the model is generated
#'   automatically using `lcfa_model()`.
#' @param trt_cut Cutpoint used to dichotomize the transition rating variable
#'   when it has more than two observed values. Values >= `trt_cut` are coded 1.
#' @param auto_equalize Logical. If `TRUE`, item levels are equalized using
#'   `equalize_levels()`.
#' @param pair_by Pairing method passed to `equalize_levels()` and `lcfa_model()`.
#'   `"position"` assumes Time 1 items followed by Time 2 items. `"suffix"`
#'   detects pairs using `t1_suffix` and `t2_suffix`.
#' @param t1_suffix Regex suffix identifying Time 1 items when
#'   `pair_by = "suffix"`.
#' @param t2_suffix Regex suffix identifying Time 2 items when
#'   `pair_by = "suffix"`.
#' @param min_resp Minimum number of responses required in each response
#'   category before collapsing.
#' @param shift_to_one Passed to `equalize_levels()`.
#' @param shift_back Passed to `equalize_levels()`.
#' @param B Integer. Number of bootstrap samples. Bootstrap CI is computed only if
#'   `B > 100`.
#' @param report_every Progress message interval during bootstrapping.
#' @param N_ets Number of simulated theta values used for expected test score
#'   calculation.
#' @param return_prepared Logical. If `TRUE`, returns prepared data, model, fit,
#'   and bootstrap details.
#' @param print_model Logical. If `TRUE`, prints the generated lavaan model.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @return A `mic_lcfa` object.
#'
#' @export
mic_lcfa <- function(
    mydat,
    trt = NULL,
    model = NULL,
    trt_cut = 1,
    auto_equalize = TRUE,
    pair_by = c("position", "suffix"),
    t1_suffix = NULL,
    t2_suffix = NULL,
    min_resp = 10L,
    shift_to_one = TRUE,
    shift_back = FALSE,
    B = 0L,
    report_every = 100L,
    N_ets = 5000L,
    return_prepared = FALSE,
    print_model = TRUE,
    verbose = TRUE
) {

  pair_by <- match.arg(pair_by)

  # ---------------------------------------------------------------------------
  # Basic checks
  # ---------------------------------------------------------------------------

  if (!is.data.frame(mydat)) {
    stop("`mydat` must be a data frame.", call. = FALSE)
  }

  if (ncol(mydat) < 3L) {
    stop(
      "`mydat` must contain paired items and one transition rating variable.",
      call. = FALSE
    )
  }

  if (is.null(trt)) {
    trt <- names(mydat)[ncol(mydat)]
  }

  if (!is.character(trt) || length(trt) != 1L || is.na(trt)) {
    stop("`trt` must be a single character string.", call. = FALSE)
  }

  if (!trt %in% names(mydat)) {
    stop("Transition rating variable `", trt, "` not found in `mydat`.", call. = FALSE)
  }

  if (!is.numeric(mydat[[trt]])) {
    stop("Transition rating variable `", trt, "` must be numeric.", call. = FALSE)
  }

  report_every <- as.integer(report_every)

  if (is.na(report_every) || report_every < 1L) {
    stop("`report_every` must be a positive integer.", call. = FALSE)
  }

  B <- as.integer(B)

  if (is.na(B) || B < 0L) {
    stop("`B` must be a non-negative integer.", call. = FALSE)
  }

  N_ets <- as.integer(N_ets)

  if (is.na(N_ets) || N_ets < 1L) {
    stop("`N_ets` must be a positive integer.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Split item data and transition rating
  # ---------------------------------------------------------------------------

  item_data_raw <- mydat[, setdiff(names(mydat), trt), drop = FALSE]
  trt_raw <- mydat[[trt]]

  non_numeric_items <- names(item_data_raw)[
    !vapply(item_data_raw, is.numeric, logical(1))
  ]

  if (length(non_numeric_items) > 0L) {
    stop(
      "All PROM item variables must be numeric. Non-numeric variables: ",
      paste(non_numeric_items, collapse = ", "),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Equalize item levels
  # ---------------------------------------------------------------------------

  eq <- NULL

  if (isTRUE(auto_equalize)) {

    eq <- equalize_levels(
      data = item_data_raw,
      pair_by = pair_by,
      t1_suffix = t1_suffix,
      t2_suffix = t2_suffix,
      min_resp = min_resp,
      shift_to_one = shift_to_one,
      shift_back = shift_back,
      verbose = verbose,
      print_tables = verbose
    )

    item_data <- eq$data
    pair_map <- eq$pair_map

  } else {

    item_data <- item_data_raw
    pair_map <- NULL
  }

  # ---------------------------------------------------------------------------
  # Dichotomize transition rating
  # ---------------------------------------------------------------------------

  trt_vals <- sort(unique(trt_raw[!is.na(trt_raw)]))

  if (length(trt_vals) < 2L) {
    stop("Transition rating variable must have at least two observed values.", call. = FALSE)
  }

  if (length(trt_vals) == 2L) {

    if (all(trt_vals %in% c(0, 1))) {
      trt_bin <- as.integer(trt_raw)
    } else {
      trt_bin <- ifelse(
        is.na(trt_raw),
        NA_integer_,
        as.integer(trt_raw == trt_vals[2L])
      )
    }

  } else {

    trt_bin <- ifelse(
      is.na(trt_raw),
      NA_integer_,
      as.integer(trt_raw >= trt_cut)
    )
  }

  if (length(unique(trt_bin[!is.na(trt_bin)])) != 2L) {
    stop(
      "Dichotomized transition rating variable must have exactly two observed categories.",
      call. = FALSE
    )
  }

  mydat_lcfa <- cbind(item_data, trt_bin)
  names(mydat_lcfa)[ncol(mydat_lcfa)] <- trt

  # ---------------------------------------------------------------------------
  # Generate or validate LCFA model
  # ---------------------------------------------------------------------------

  if (is.null(model)) {

    # Important: call lcfa_model(print_model = FALSE) to avoid possible
    # recursive printing / node stack overflow.
    mod_obj <- lcfa_model(
      data = mydat_lcfa,
      trt = trt,
      pair_by = pair_by,
      t1_suffix = t1_suffix,
      t2_suffix = t2_suffix,
      pair_map = pair_map,
      print_model = FALSE
    )

    model_lcfa <- mod_obj$model

    if (isTRUE(print_model)) {
      cat("\n================ LCFA LAVAAN MODEL ================\n\n")
      cat(model_lcfa)
      cat("\n====================================================\n")
    }

  } else {

    if (grepl("f1\\s*==\\s*-\\s*f2", model)) {
      stop(
        "The supplied model contains `f1 == -f2`. ",
        "This transition-loading constraint is no longer recommended. ",
        "Please remove it from the model.",
        call. = FALSE
      )
    }

    # Build metadata for item names and item pairs while using the supplied model.
    mod_obj <- lcfa_model(
      data = mydat_lcfa,
      trt = trt,
      pair_by = pair_by,
      t1_suffix = t1_suffix,
      t2_suffix = t2_suffix,
      pair_map = pair_map,
      print_model = FALSE
    )

    model_lcfa <- model

    if (isTRUE(print_model)) {
      cat("\n================ LCFA LAVAAN MODEL ================\n\n")
      cat(model_lcfa)
      cat("\n====================================================\n")
    }
  }

  t1_items <- mod_obj$t1_items

  if (length(t1_items) < 1L) {
    stop("No Time 1 items were identified.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Fit unconstrained LCFA model
  # ---------------------------------------------------------------------------

  fit_lcfa <- lavaan::cfa(
    model_lcfa,
    data = mydat_lcfa,
    std.lv = TRUE,
    ordered = TRUE,
    parameterization = "theta"
  )

  pe <- lavaan::parameterEstimates(fit_lcfa, rsquare = TRUE)

  psb_est <- pe$est[pe$label == "psb"]
  MIC.theta <- pe$est[pe$label == "b_param"]

  if (length(psb_est) != 1L || !is.finite(psb_est)) {
    stop("Could not uniquely identify finite parameter labelled `psb`.", call. = FALSE)
  }

  if (length(MIC.theta) != 1L || !is.finite(MIC.theta)) {
    stop("Could not uniquely identify finite parameter labelled `b_param`.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Convert MIC theta to expected test score difference using mirt
  # ---------------------------------------------------------------------------

  mod_irt <- mirt::mirt(
    mydat_lcfa[, t1_items, drop = FALSE],
    verbose = FALSE,
    itemtype = "graded"
  )

  theta1 <- as.matrix(stats::rnorm(N_ets, 0, 1))
  theta2 <- theta1 + MIC.theta

  mean_ets1 <- mean(mirt::expected.test(mod_irt, theta1))
  mean_ets2 <- mean(mirt::expected.test(mod_irt, theta2))

  MIC.ets <- mean_ets2 - mean_ets1

  # ---------------------------------------------------------------------------
  # Bootstrap
  # ---------------------------------------------------------------------------

  boot <- NULL
  MIC_CI <- NULL
  nboot <- 0L

  if (B > 100L) {

    boot <- rep(NA_real_, B)

    if (isTRUE(verbose)) {
      message("Starting bootstrap with B = ", B, ".")
    }

    for (i in seq_len(B)) {

      j <- sample.int(nrow(mydat_lcfa), nrow(mydat_lcfa), replace = TRUE)
      mydat_boot <- mydat_lcfa[j, , drop = FALSE]

      boot_val <- tryCatch({

        fit_boot <- lavaan::cfa(
          model_lcfa,
          data = mydat_boot,
          std.lv = TRUE,
          ordered = TRUE,
          parameterization = "theta"
        )

        pe_boot <- lavaan::parameterEstimates(fit_boot, rsquare = TRUE)
        MIC.theta_boot <- pe_boot$est[pe_boot$label == "b_param"]

        if (length(MIC.theta_boot) != 1L || !is.finite(MIC.theta_boot)) {
          stop("Invalid bootstrap MIC.theta.", call. = FALSE)
        }

        mod_irt_boot <- mirt::mirt(
          mydat_boot[, t1_items, drop = FALSE],
          itemtype = "graded",
          verbose = FALSE
        )

        theta1_boot <- as.matrix(stats::rnorm(N_ets, 0, 1))
        theta2_boot <- theta1_boot + MIC.theta_boot

        mean_ets1_boot <- mean(mirt::expected.test(mod_irt_boot, theta1_boot))
        mean_ets2_boot <- mean(mirt::expected.test(mod_irt_boot, theta2_boot))

        mean_ets2_boot - mean_ets1_boot

      }, error = function(e) NA_real_)

      if (is.finite(boot_val)) {
        nboot <- nboot + 1L
        boot[nboot] <- boot_val
      }

      if (nboot > 0L && nboot %% report_every == 0L) {
        message("Successfully estimated ", nboot, " bootstrap MICs.")
      }
    }

    if (nboot > 0L) {
      boot <- boot[seq_len(nboot)]

      MIC_CI <- stats::quantile(
        boot,
        probs = c(0.025, 0.975),
        na.rm = TRUE,
        names = TRUE
      )

    } else {

      boot <- numeric(0L)
      MIC_CI <- NULL

      warning("No successful bootstrap estimates were obtained.", call. = FALSE)
    }

  } else if (B > 0L && B < 100L) {

    message(
      "Bootstrap CI not computed because `B < 100`. ",
      "Set `B >= 100` to request bootstrapping."
    )
  }

  # ---------------------------------------------------------------------------
  # Output
  # ---------------------------------------------------------------------------

  out <- list(
    psb = as.numeric(psb_est),
    MIC.theta = as.numeric(MIC.theta),
    MIC.ets = as.numeric(MIC.ets),
    MIC_CI = MIC_CI,
    nboot = nboot
  )

  if (isTRUE(return_prepared)) {
    out$prepared_data <- mydat_lcfa
    out$equalize_levels <- eq
    out$lcfa_model <- mod_obj
    out$model_lcfa <- model_lcfa
    out$fit_lcfa <- fit_lcfa
    out$boot <- boot
  }

  structure(out, class = "mic_lcfa")
}


#' @export
print.mic_lcfa <- function(x, ...) {
  cat("LCFA-based MIC estimation\n")
  cat("--------------------------------------\n")
  cat("Present-state bias:", round(x$psb, 4), "\n")
  cat("MIC theta:", round(x$MIC.theta, 4), "\n")
  cat("MIC expected test score:", round(x$MIC.ets, 4), "\n")

  if (!is.null(x$MIC_CI)) {
    cat(
      "Bootstrap 95% CI:",
      round(x$MIC_CI[1L], 4),
      "to",
      round(x$MIC_CI[2L], 4),
      "\n"
    )
  }

  invisible(x)
}



