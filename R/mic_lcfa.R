#' Estimate Present-State Bias and Anchor-Based MIC Using Longitudinal CFA
#'
#' `mic_lcfa()` estimates present-state bias and anchor-based minimal important
#' change (MIC) using a longitudinal confirmatory factor analysis (LCFA) model.
#'
#' The input data should contain paired Time 1 and Time 2 PROM items plus one
#' transition rating variable. Item pairs may be identified by column position
#' or by suffixes. By default, paired item levels are first equalized using
#' `equalize_levels()`, and lavaan syntax is generated using `lcfa_model()`.
#'
#' The LCFA model estimates the MIC on the latent-change scale. This latent MIC
#' is then mapped onto the observed PROM summed-score metric using an expected
#' score function.
#'
#' @details
#' The LCFA model uses both Time 1 and Time 2 item responses, together with the
#' transition rating, to estimate the latent MIC and present-state bias. The
#' latent MIC is then mapped onto the observed PROM summed-score metric.
#'
#' \enumerate{
#'   \item at the baseline latent trait distribution, `theta ~ N(0, 1)`;
#'   \item at the shifted latent trait distribution,
#'   `theta ~ N(MIC.theta, 1)`.
#' }
#'
#' Two expected-score mappings are available through `score_method`:
#'
#' \itemize{
#'   \item `"irt"` uses an IRT-based expected test score method via
#'   `mirt::expected.test()`. This follows the published implementation of
#'   Terluin et al. (2024).
#'   \item `"cfa"` uses a closed-form CFA probit expected-score method based on
#'   lavaan-estimated item loadings and thresholds. For each item, the method
#'   computes marginal category probabilities under `theta ~ N(mu, 1)`, first
#'   with `mu = 0` and then with `mu = MIC.theta`. These probabilities are used
#'   to obtain expected item scores, which are summed across items to obtain the
#'   expected PROM score.
#'   \item `"both"` computes expected scores using both irt and cfa methods
#' }
#'
#' Under the CFA probit method, the closed-form marginal cumulative probability
#' for item threshold `tau_k` is based on the latent response model
#' `Y* = lambda * theta + error`. If `theta ~ N(mu, 1)` and
#' `error ~ N(0, 1)`, then `Y* ~ N(lambda * mu, lambda^2 + 1)`. Therefore,
#' marginal category probabilities can be computed without simulating theta
#' values or fitting an additional IRT model.
#'
#'
#' @references
#' Terluin B, Trigg A, Fromy P, Schuller W, Terwee CB, Bjorner JB.
#' Estimating anchor-based minimal important change using longitudinal
#' confirmatory factor analysis. Qual Life Res. 2024;33:963-973.
#' doi:10.1007/s11136-023-03577-w
#'
#' Terluin B, Fromy P, Trigg A, Terwee CB, Bjorner JB.
#' Effect of present state bias on minimal important change estimates:
#' a simulation study. Quality of Life Research. 2024.
#' doi:10.1007/s11136-024-03763-4
#'
#' Terluin B, Griffiths P, Trigg A, Terwee CB, Bjorner JB.
#' Present state bias in transition ratings was accurately estimated in
#' simulated and real data. J Clin Epidemiol. 2022;143:128-136.
#' doi:10.1016/j.jclinepi.2021.12.024
#'
#' @param mydat Data frame containing paired Time 1 and Time 2 PROM items and
#'   one transition rating variable.
#' @param trt Character. Name of the transition rating variable. If `NULL`, the
#'   final column of `mydat` is used.
#' @param model Optional lavaan model syntax. If `NULL`, the model is generated
#'   automatically using `lcfa_model()`.
#' @param trt_cut Numeric. Cutpoint used to dichotomize the transition rating
#'   variable when it has more than two observed values. Values greater than or
#'   equal to `trt_cut` are coded as 1, and lower values are coded as 0. If the
#'   transition rating is already binary, the larger value is coded as 1 and
#'   `trt_cut` is ignored.
#' @param auto_equalize Logical. If `TRUE`, item levels are equalized using
#'   `equalize_levels()`.
#' @param pair_by Character. Pairing method passed to `equalize_levels()` and
#'   `lcfa_model()`. `"position"` assumes Time 1 items followed by Time 2
#'   items. `"suffix"` detects pairs using `t1_suffix` and `t2_suffix`.
#' @param t1_suffix Regex suffix identifying Time 1 items when
#'   `pair_by = "suffix"`.
#' @param t2_suffix Regex suffix identifying Time 2 items when
#'   `pair_by = "suffix"`.
#' @param min_resp Integer. Minimum number of responses required in each
#'   response category before collapsing.
#' @param B Integer. Number of bootstrap samples. Bootstrap confidence
#'   intervals are computed only if `B >= 100`.
#' @param report_every Integer. Progress message interval during bootstrapping.
#' @param N_ets Integer. Number of simulated theta values used for the IRT
#'   expected test score calculation. This argument is not used by the
#'   closed-form CFA method.
#' @param score_method Character. Method used to map the latent MIC to the
#'   observed PROM score metric. Options are `"irt"`, `"cfa"`, and `"both"`.
#'   `"irt"` uses `mirt::expected.test()`, `"cfa"` uses the closed-form
#'   CFA/probit expected-score method, and `"both"` returns both mappings.
#' @param return_prepared Logical. If `TRUE`, returns prepared data, generated
#'   model, fitted LCFA object, and bootstrap details.
#' @param print_model Logical. If `TRUE`, prints the generated lavaan model.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @return A `mic_lcfa` object with elements including:
#' \itemize{
#'   \item `psb`: estimated present-state bias;
#'   \item `MIC.theta`: MIC on the latent-change scale;
#'   \item `MIC.ets`: MIC on the observed PROM summed-score metric;
#'   \item `MIC.ets.irt`: IRT-based expected-score MIC, if requested;
#'   \item `MIC.ets.cfa`: CFA-based expected-score MIC, if requested;
#'   \item `MIC_CI`: bootstrap confidence interval, if requested;
#'   \item `nboot`: number of successful bootstrap estimates.
#' }
#'
#' If `return_prepared = TRUE`, the returned object also contains the prepared
#' data, generated lavaan model, fitted LCFA object, and bootstrap values.
#'
#' @examples
#' \dontrun{
#' # Minimal working example
#' sim <- simdat(N = 500, seed = 123, add_change = TRUE)
#' dat <- sim$datw
#'
#' mydat <- dat[, c(
#'   sim$item_names$t1_items,
#'   sim$item_names$t2_items,
#'   "trat"
#' )]
#'
#' # Compare IRT and CFA mappings
#' out_both <- mic_lcfa(
#'   mydat = mydat,
#'   trt = "trat",
#'   trt_cut = 1,
#'   auto_equalize = TRUE,
#'   pair_by = "suffix",
#'   t1_suffix = "",
#'   t2_suffix = "\\.1",
#'   min_resp = 5,
#'   score_method = "both",
#'   B = 0,
#'   print_model = FALSE,
#'   verbose = FALSE)
#'
#' out_both$MIC.ets
#'
#' # Bootstrap confidence interval using the faster CFA mapping
#' out_boot <- mic_lcfa(
#'   mydat = mydat,
#'   trt = "trat",
#'   trt_cut = 1,
#'   auto_equalize = TRUE,
#'   pair_by = "suffix",
#'   t1_suffix = "",
#'   t2_suffix = "\\.1",
#'   min_resp = 5,
#'   score_method = "cfa",
#'   B = 200,
#'   report_every = 50,
#'   print_model = FALSE,
#'   verbose = TRUE)
#'
#' out_boot
#' }
#'
#' @seealso [lcfa_model()], [equalize_levels()], [simdat()]
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
    min_resp = 5L,
    B = 0L,
    report_every = 100L,
    N_ets = 5000L,
    score_method = c("irt", "cfa", "both"),
    return_prepared = FALSE,
    print_model = TRUE,
    verbose = TRUE
) {

  pair_by <- match.arg(pair_by)
  score_method <- match.arg(score_method)

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
    stop("Transition rating variable `", trt, "` not found.", call. = FALSE)
  }

  if (!is.numeric(mydat[[trt]])) {
    stop("Transition rating variable `", trt, "` must be numeric.", call. = FALSE)
  }

  B <- as.integer(B)
  report_every <- as.integer(report_every)
  N_ets <- as.integer(N_ets)

  if (is.na(B) || B < 0L) {
    stop("`B` must be a non-negative integer.", call. = FALSE)
  }

  if (is.na(report_every) || report_every < 1L) {
    stop("`report_every` must be a positive integer.", call. = FALSE)
  }

  if (is.na(N_ets) || N_ets < 1L) {
    stop("`N_ets` must be a positive integer.", call. = FALSE)
  }

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

  eq <- NULL

  if (isTRUE(auto_equalize)) {

    eq <- equalize_levels(
      data = item_data_raw,
      pair_by = pair_by,
      t1_suffix = t1_suffix,
      t2_suffix = t2_suffix,
      min_resp = min_resp,
      verbose = verbose,
      print_tables = verbose
    )

    item_data <- eq$data
    pair_map <- eq$pair_map

  } else {

    item_data <- item_data_raw
    pair_map <- NULL
  }

  trt_vals <- sort(unique(trt_raw[!is.na(trt_raw)]))

  if (length(trt_vals) < 2L) {
    stop(
      "Transition rating variable must have at least two observed values.",
      call. = FALSE
    )
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

  if (is.null(model)) {

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

  } else {

    if (grepl("f1\\s*==\\s*-\\s*f2", model)) {
      stop(
        "The supplied model contains `f1 == -f2`. ",
        "Please remove this constraint.",
        call. = FALSE
      )
    }

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
  }

  if (isTRUE(print_model)) {
    cat("\n================ LCFA LAVAAN MODEL ================\n\n")
    cat(model_lcfa)
    cat("\n====================================================\n")
  }

  t1_items <- mod_obj$t1_items

  fit_lcfa <- lavaan::cfa(
    model_lcfa,
    data = mydat_lcfa,
    std.lv = TRUE,
    ordered = TRUE,
    parameterization = "theta"
  )

  converged <- tryCatch(
    lavaan::lavInspect(fit_lcfa, "converged"),
    error = function(e) FALSE
  )

  if (!isTRUE(converged)) {
    stop("LCFA model did not converge.", call. = FALSE)
  }

  pe <- lavaan::parameterEstimates(fit_lcfa, rsquare = TRUE)

  psb_est <- get_labeled_est(pe, "psb")
  MIC.theta <- get_labeled_est(pe, c("b_param", "MIC.theta", "mic"))

  MIC.ets.irt <- NULL
  MIC.ets.cfa <- NULL

  if (score_method %in% c("irt", "both")) {
    MIC.ets.irt <- mic_ets_irt(
      data = mydat_lcfa[, t1_items, drop = FALSE],
      MIC.theta = MIC.theta,
      N_ets = N_ets
    )
  }

  if (score_method %in% c("cfa", "both")) {
    MIC.ets.cfa <- mic_ets_cfa_closed(
      fit = fit_lcfa,
      data = mydat_lcfa,
      t1_items = t1_items,
      MIC.theta = MIC.theta,
      factor_name = "F1"
    )
  }

  if (score_method == "irt") {
    MIC.ets <- MIC.ets.irt
  } else if (score_method == "cfa") {
    MIC.ets <- MIC.ets.cfa
  } else {
    MIC.ets <- c(
      irt = MIC.ets.irt,
      cfa = MIC.ets.cfa
    )
  }

  boot <- NULL
  boot_irt <- NULL
  boot_cfa <- NULL
  MIC_CI <- NULL
  MIC_CI_irt <- NULL
  MIC_CI_cfa <- NULL
  nboot <- 0L

  if (B >= 100L) {

    if (score_method %in% c("irt", "both")) {
      boot_irt <- rep(NA_real_, B)
    }

    if (score_method %in% c("cfa", "both")) {
      boot_cfa <- rep(NA_real_, B)
    }

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

        converged_boot <- tryCatch(
          lavaan::lavInspect(fit_boot, "converged"),
          error = function(e) FALSE
        )

        if (!isTRUE(converged_boot)) {
          stop("Bootstrap LCFA model did not converge.", call. = FALSE)
        }

        pe_boot <- lavaan::parameterEstimates(fit_boot, rsquare = TRUE)

        MIC.theta_boot <- get_labeled_est(
          pe_boot,
          c("b_param", "MIC.theta", "mic")
        )

        out_boot <- list(
          irt = NA_real_,
          cfa = NA_real_
        )

        if (score_method %in% c("irt", "both")) {
          out_boot$irt <- mic_ets_irt(
            data = mydat_boot[, t1_items, drop = FALSE],
            MIC.theta = MIC.theta_boot,
            N_ets = N_ets
          )
        }

        if (score_method %in% c("cfa", "both")) {
          out_boot$cfa <- mic_ets_cfa_closed(
            fit = fit_boot,
            data = mydat_boot,
            t1_items = t1_items,
            MIC.theta = MIC.theta_boot,
            factor_name = "F1"
          )
        }

        out_boot

      }, error = function(e) NULL)

      if (!is.null(boot_val)) {

        ok <- TRUE

        if (score_method %in% c("irt", "both")) {
          ok <- ok && is.finite(boot_val$irt)
        }

        if (score_method %in% c("cfa", "both")) {
          ok <- ok && is.finite(boot_val$cfa)
        }

        if (isTRUE(ok)) {

          nboot <- nboot + 1L

          if (score_method %in% c("irt", "both")) {
            boot_irt[nboot] <- boot_val$irt
          }

          if (score_method %in% c("cfa", "both")) {
            boot_cfa[nboot] <- boot_val$cfa
          }
        }
      }

      if (nboot > 0L && nboot %% report_every == 0L) {
        message("Successfully estimated ", nboot, " bootstrap MICs.")
      }
    }

    if (nboot > 0L) {

      if (score_method %in% c("irt", "both")) {
        boot_irt <- boot_irt[seq_len(nboot)]

        MIC_CI_irt <- stats::quantile(
          boot_irt,
          probs = c(0.025, 0.975),
          na.rm = TRUE,
          names = TRUE
        )
      }

      if (score_method %in% c("cfa", "both")) {
        boot_cfa <- boot_cfa[seq_len(nboot)]

        MIC_CI_cfa <- stats::quantile(
          boot_cfa,
          probs = c(0.025, 0.975),
          na.rm = TRUE,
          names = TRUE
        )
      }

      if (score_method == "irt") {
        boot <- boot_irt
        MIC_CI <- MIC_CI_irt
      } else if (score_method == "cfa") {
        boot <- boot_cfa
        MIC_CI <- MIC_CI_cfa
      } else {
        boot <- data.frame(
          irt = boot_irt,
          cfa = boot_cfa
        )

        MIC_CI <- rbind(
          irt = MIC_CI_irt,
          cfa = MIC_CI_cfa
        )
      }

    } else {

      boot <- numeric(0L)
      warning("No successful bootstrap estimates were obtained.", call. = FALSE)
    }

  } else if (B > 0L && B < 100L) {

    message(
      "Bootstrap CI not computed because `B < 100`. ",
      "Set `B >= 100` to request bootstrapping."
    )
  }

  out <- list(
    psb = as.numeric(psb_est),
    MIC.theta = as.numeric(MIC.theta),
    MIC.ets = MIC.ets,
    MIC.ets.irt = MIC.ets.irt,
    MIC.ets.cfa = MIC.ets.cfa,
    score_method = score_method,
    MIC_CI = MIC_CI,
    MIC_CI_irt = MIC_CI_irt,
    MIC_CI_cfa = MIC_CI_cfa,
    nboot = nboot
  )

  if (isTRUE(return_prepared)) {
    out$prepared_data <- mydat_lcfa
    out$equalize_levels <- eq
    out$lcfa_model <- mod_obj
    out$model_lcfa <- model_lcfa
    out$fit_lcfa <- fit_lcfa
    out$boot <- boot
    out$boot_irt <- boot_irt
    out$boot_cfa <- boot_cfa
  }

  structure(out, class = "mic_lcfa")
}

#' @export
print.mic_lcfa <- function(x, digits = 4, ...) {

  cat("LCFA-based MIC estimation\n")
  cat("--------------------------------------\n")
  cat("Present-state bias:", round(x$psb, digits), "\n")
  cat("MIC theta:", round(x$MIC.theta, digits), "\n")
  cat("Score method:", x$score_method, "\n")

  if (identical(x$score_method, "both")) {

    cat("MIC expected test score, IRT:", round(x$MIC.ets.irt, digits), "\n")
    cat("MIC expected test score, CFA:", round(x$MIC.ets.cfa, digits), "\n")

  } else {

    cat("MIC expected test score:", round(x$MIC.ets, digits), "\n")
  }

  if (!is.null(x$MIC_CI)) {

    cat("\nBootstrap 95% CI\n")
    cat("----------------\n")

    if (is.matrix(x$MIC_CI)) {

      for (i in seq_len(nrow(x$MIC_CI))) {
        cat(
          rownames(x$MIC_CI)[i],
          ": ",
          round(x$MIC_CI[i, 1L], digits),
          " to ",
          round(x$MIC_CI[i, 2L], digits),
          "\n",
          sep = ""
        )
      }

    } else {

      cat(
        round(x$MIC_CI[1L], digits),
        " to ",
        round(x$MIC_CI[2L], digits),
        "\n"
      )
    }
  }

  invisible(x)
}


#' Get a labelled parameter estimate
#'
#' @noRd
get_labeled_est <- function(pe, labels) {

  for (lab in labels) {
    val <- pe$est[pe$label == lab]

    if (length(val) == 1L && is.finite(val)) {
      return(as.numeric(val))
    }
  }

  stop(
    "Could not uniquely identify finite parameter labelled: ",
    paste(labels, collapse = " or "),
    call. = FALSE
  )
}


#' Derive item score levels from prepared item data
#'
#' @noRd
derive_item_levels_list <- function(data, items) {

  out <- vector("list", length(items))
  names(out) <- items

  for (item in items) {

    vals <- sort(unique(data[[item]][!is.na(data[[item]])]))

    if (length(vals) < 2L) {
      stop(
        "Item `", item, "` has fewer than two observed score values.",
        call. = FALSE
      )
    }

    out[[item]] <- vals
  }

  out
}


#' Extract first finite parameter estimate
#'
#' @noRd
first_finite_or_default <- function(x, default) {

  x <- x[is.finite(x)]

  if (length(x) >= 1L) {
    as.numeric(x[1L])
  } else {
    default
  }
}


#' Extract CFA item parameters for marginal expected score calculation
#'
#' @noRd
extract_cfa_item_parameters <- function(
    fit,
    items,
    factor_name = "F1",
    item_levels_list
) {

  pe <- lavaan::parameterEstimates(fit)

  factor_var <- pe$est[
    pe$lhs == factor_name &
      pe$op == "~~" &
      pe$rhs == factor_name
  ]

  factor_var <- first_finite_or_default(factor_var, default = 1)

  item_parameters <- vector("list", length(items))
  names(item_parameters) <- items

  for (item in items) {

    lambda <- pe$est[
      pe$lhs == factor_name &
        pe$op == "=~" &
        pe$rhs == item
    ]

    if (length(lambda) != 1L || !is.finite(lambda)) {
      stop(
        "Could not extract a unique finite loading for item `",
        item,
        "`.",
        call. = FALSE
      )
    }

    tau_tab <- pe[
      pe$lhs == item &
        pe$op == "|",
      ,
      drop = FALSE
    ]

    if (nrow(tau_tab) == 0L) {
      stop(
        "No thresholds found for item `",
        item,
        "`.",
        call. = FALSE
      )
    }

    tau_order <- suppressWarnings(
      as.integer(sub("^t", "", tau_tab$rhs))
    )

    if (all(is.na(tau_order))) {
      tau <- sort(tau_tab$est)
    } else {
      tau <- tau_tab$est[order(tau_order)]
    }

    resid_var <- pe$est[
      pe$lhs == item &
        pe$op == "~~" &
        pe$rhs == item
    ]

    resid_var <- first_finite_or_default(resid_var, default = 1)

    scores <- item_levels_list[[item]]

    if (is.null(scores)) {
      stop(
        "No score values supplied for item `",
        item,
        "`.",
        call. = FALSE
      )
    }

    if (length(scores) != length(tau) + 1L) {
      stop(
        "Number of score values for item `",
        item,
        "` does not equal number of thresholds + 1.",
        call. = FALSE
      )
    }

    item_parameters[[item]] <- list(
      item = item,
      lambda = as.numeric(lambda),
      tau = as.numeric(tau),
      scores = as.numeric(scores),
      factor_var = as.numeric(factor_var),
      resid_var = as.numeric(resid_var)
    )
  }

  item_parameters
}


#' IRT-based expected test score MIC
#'
#' @noRd
mic_ets_irt <- function(
    data,
    MIC.theta,
    N_ets = 5000L
) {

  mod_irt <- mirt::mirt(
    data,
    verbose = FALSE,
    itemtype = "graded"
  )

  theta1 <- as.matrix(stats::rnorm(N_ets, mean = 0, sd = 1))
  theta2 <- theta1 + MIC.theta

  ets1 <- mean(mirt::expected.test(mod_irt, theta1))
  ets2 <- mean(mirt::expected.test(mod_irt, theta2))

  as.numeric(ets2 - ets1)
}


#' Closed-form CFA expected item score
#'
#' @noRd
cfa_expected_item_score_marginal <- function(
    mu,
    item_pars
) {

  lambda <- item_pars$lambda
  tau <- item_pars$tau
  scores <- item_pars$scores
  factor_var <- item_pars$factor_var
  resid_var <- item_pars$resid_var

  denom <- sqrt((lambda^2 * factor_var) + resid_var)

  cum_probs <- stats::pnorm(
    (tau - lambda * mu) / denom
  )

  probs <- c(
    cum_probs[1L],
    diff(cum_probs),
    1 - cum_probs[length(cum_probs)]
  )

  probs[probs < 0 & probs > -1e-12] <- 0
  probs[probs > 1 & probs < 1 + 1e-12] <- 1
  probs <- probs / sum(probs)

  as.numeric(sum(scores * probs))
}


#' Closed-form CFA expected test score
#'
#' @noRd
cfa_expected_test_score_marginal <- function(
    mu,
    item_parameters
) {

  sum(vapply(
    item_parameters,
    function(item_pars) {
      cfa_expected_item_score_marginal(
        mu = mu,
        item_pars = item_pars
      )
    },
    numeric(1)
  ))
}


#' Closed-form CFA-based expected test score MIC
#'
#' @noRd
mic_ets_cfa_closed <- function(
    fit,
    data,
    t1_items,
    MIC.theta,
    factor_name = "F1"
) {

  item_levels_list <- derive_item_levels_list(
    data = data,
    items = t1_items
  )

  item_parameters <- extract_cfa_item_parameters(
    fit = fit,
    items = t1_items,
    factor_name = factor_name,
    item_levels_list = item_levels_list
  )

  ets_baseline <- cfa_expected_test_score_marginal(
    mu = 0,
    item_parameters = item_parameters
  )

  ets_shifted <- cfa_expected_test_score_marginal(
    mu = MIC.theta,
    item_parameters = item_parameters
  )

  as.numeric(ets_shifted - ets_baseline)
}

