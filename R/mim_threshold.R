#' Estimate a CFA-based threshold for a multi-item questionnaire
#'
#' `mim_threshold()` estimates an anchor-based interpretation threshold for
#' a multi-item measure (MIM) using a one-factor CFA model with an anchor/transition
#' rating item. This technique is based on
#'
#' [1] Terluin, B., Koopman, J.E., Hoogendam, L. et al. Estimating meaningful thresholds
#' for multi-item questionnaires using item response theory. Qual Life Res 32, 1819–1830 (2023)
#'
#' [2] Terluin, B., Trigg, A., Fromy, P. et al. Estimating anchor-based minimal important
#'  change using longitudinal confirmatory factor analysis. Qual Life Res 33, 963–973 (2024).
#'
#' The model formula takes the form:
#'
#' \preformatted{
#' anchor ~ item1 + item2 + item3
#' }
#'
#' If the right-hand side is `.` or contains no item variables, all variables
#' in `mydata` except the anchor are treated as MIM items.
#'
#' The `mim_threshold()` function:
#' \enumerate{
#'   \item fits a one-factor CFA model with the MIM items and an anchor item;
#'   \item computes `theta_star`, the latent factor value where the anchor
#'         threshold occurs;
#'   \item maps `theta_star` to each item using CFA-implied category
#'         probabilities;
#'   \item multiplies item category probabilities by item values or bin
#'         midpoints;
#'   \item sums the item-level expected values to obtain the MIM threshold.
#' }
#'
#' @param mydata Data frame.
#' @param var_formula Formula of the form `anchor ~ item1 + item2 + ...`.
#'   Use `anchor ~ .` to use all variables except the anchor as items.
#' @param item_discretize Logical. If `TRUE`, discretize each item using
#'   `var_discretize()`. If `FALSE`, use the item values as observed ordinal
#'   categories.
#' @param item_levels Number of levels to use when `item_discretize = TRUE`.
#'   Must be between 2 and 12.
#' @param require_all_item_levels Logical. Passed to `var_discretize()`.
#' @param anchor_cut Cutpoint for binarizing the anchor if it has more than
#'   two unique non-missing values. Values >= `anchor_cut` are coded 1.
#' @param B Number of bootstrap samples. Bootstrap CI is computed only if
#'   `B >= 100`.
#' @param report_every During bootstrapping, print progress every
#'   `report_every` attempted fits.
#' @param factor_name Name of the latent factor.
#' @param item_suffix Suffix appended to item names when items are discretized
#'   or internally recoded.
#' @param anchor_suffix Suffix appended to the anchor variable name if the
#'   anchor has more than two unique values and is dichotomized.
#' @param std.lv Passed to `lavaan::cfa()`.
#' @param parameterization Passed to `lavaan::cfa()`.
#' @param verbose If `TRUE`, print item names and generated lavaan model.
#' @param ... Additional arguments passed to `lavaan::cfa()`.
#'
#' @return A `mim_threshold` object. Additional details can be retrieved with
#'   `mim_threshold_details()`.
#'
#' @export
mim_threshold <- function(
    mydata,
    var_formula,
    item_discretize = FALSE,
    item_levels = 10L,
    require_all_item_levels = FALSE,
    anchor_cut = 1,
    B = 0L,
    report_every = 50L,
    factor_name = "F1",
    item_suffix = "_ord",
    anchor_suffix = "_bin",
    std.lv = TRUE,
    parameterization = "theta",
    verbose = TRUE,
    ...
) {

  if (!is.data.frame(mydata)) {
    stop("`mydata` must be a data frame.", call. = FALSE)
  }

  if (!inherits(var_formula, "formula")) {
    stop(
      "`var_formula` must be a formula, e.g. anchor ~ item1 + item2.",
      call. = FALSE
    )
  }

  if (length(var_formula) != 3L) {
    stop(
      "`var_formula` must have both left-hand and right-hand sides.",
      call. = FALSE
    )
  }

  anchor_var <- all.vars(var_formula[[2L]])

  if (length(anchor_var) != 1L) {
    stop(
      "The left-hand side of `var_formula` must contain exactly one anchor variable.",
      call. = FALSE
    )
  }

  rhs_text <- paste(deparse(var_formula[[3L]]), collapse = "")
  rhs_vars <- all.vars(var_formula[[3L]])

  if (rhs_text == "." || length(rhs_vars) == 0L) {
    item_vars <- setdiff(names(mydata), anchor_var)
  } else {
    item_vars <- rhs_vars
  }

  if (length(item_vars) < 1L) {
    stop("No MIM items were identified.", call. = FALSE)
  }

  needed_vars <- c(anchor_var, item_vars)
  missing_vars <- setdiff(needed_vars, names(mydata))

  if (length(missing_vars) > 0L) {
    stop(
      "Variables not found in `mydata`: ",
      paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  non_numeric <- needed_vars[!vapply(mydata[needed_vars], is.numeric, logical(1))]

  if (length(non_numeric) > 0L) {
    stop(
      "All anchor and item variables must be numeric. Non-numeric variables: ",
      paste(non_numeric, collapse = ", "),
      call. = FALSE
    )
  }

  report_every <- as.integer(report_every)

  if (is.na(report_every) || report_every < 1L) {
    stop("`report_every` must be a positive integer.", call. = FALSE)
  }

  if (isTRUE(verbose)) {
    message("MIM items: ", paste(item_vars, collapse = ", "))
  }

  fit_once <- function(dat, keep_fit = TRUE, verbose_fit = FALSE) {

    dat <- as.data.frame(dat)

    # ---- Anchor handling ----
    anchor_x <- dat[[anchor_var]]
    anchor_vals <- sort(unique(anchor_x[!is.na(anchor_x)]))

    if (length(anchor_vals) < 2L) {
      stop("Anchor variable must have at least two observed values.", call. = FALSE)
    }

    if (length(anchor_vals) == 2L) {

      anchor_obs_name <- anchor_var

      anchor_bin <- ifelse(
        is.na(anchor_x),
        NA_integer_,
        as.integer(anchor_x == anchor_vals[2L])
      )

    } else {

      anchor_obs_name <- paste0(anchor_var, anchor_suffix)

      anchor_bin <- ifelse(
        is.na(anchor_x),
        NA_integer_,
        as.integer(anchor_x >= anchor_cut)
      )
    }

    if (length(unique(anchor_bin[!is.na(anchor_bin)])) < 2L) {
      stop(
        "Binarized anchor variable has fewer than two observed categories.",
        call. = FALSE
      )
    }

    # ---- Prepare items ----
    cfa_data <- data.frame(anchor_value = anchor_bin, check.names = FALSE)
    names(cfa_data) <- anchor_obs_name

    item_cfa_names <- character(length(item_vars))
    item_score_values <- vector("list", length(item_vars))
    item_discretized <- vector("list", length(item_vars))

    names(item_cfa_names) <- item_vars
    names(item_score_values) <- item_vars
    names(item_discretized) <- item_vars

    for (j in seq_along(item_vars)) {

      item <- item_vars[j]
      x <- dat[[item]]

      if (isTRUE(item_discretize)) {

        disc <- var_discretize(
          x,
          n_levels = item_levels,
          require_all_levels = require_all_item_levels,
          warn_unused = !require_all_item_levels
        )

        item_name <- paste0(item, item_suffix)

        cfa_data[[item_name]] <- disc$score

        item_cfa_names[j] <- item_name
        item_score_values[[j]] <- disc$midpoints
        item_discretized[[j]] <- disc

      } else {

        vals <- sort(unique(x[!is.na(x)]))

        if (length(vals) < 2L) {
          stop("Item `", item, "` has fewer than two observed values.", call. = FALSE)
        }

        if (length(vals) > 12L) {
          stop(
            "Item `", item, "` has ", length(vals),
            " unique values. For ordered lavaan modeling in this workflow, ",
            "use <= 12 categories or set `item_discretize = TRUE`.",
            call. = FALSE
          )
        }

        item_name <- paste0(item, item_suffix)

        # Recode values to consecutive integers for lavaan.
        # Store original values for expected-score calculation.
        map <- seq_along(vals)
        names(map) <- as.character(vals)

        item_score <- unname(map[as.character(x)])
        item_score[is.na(x)] <- NA_integer_

        cfa_data[[item_name]] <- as.integer(item_score)

        item_cfa_names[j] <- item_name

        value_map <- vals
        names(value_map) <- as.character(seq_along(vals))

        item_score_values[[j]] <- value_map

        item_discretized[[j]] <- list(
          score = as.integer(item_score),
          values = vals,
          value_map = value_map,
          used_levels = seq_along(vals)
        )
      }
    }

    # Put items first, anchor last
    cfa_data <- cfa_data[, c(item_cfa_names, anchor_obs_name), drop = FALSE]

    # ---- lavaan model ----
    indicators <- c(
      item_cfa_names,
      paste0("lambda_anchor*", anchor_obs_name)
    )

    lmodel <- paste0(
      factor_name, " =~ ", paste(indicators, collapse = " + "), "\n",
      anchor_obs_name, " | tau_anchor*t1"
    )

    if (isTRUE(verbose_fit)) {
      message("Generated lavaan model:\n", lmodel)
    }

    lavaan_ordered <- c(item_cfa_names, anchor_obs_name)

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

    # ---- Map theta_star to every item ----
    item_expected <- numeric(length(item_vars))
    item_probs <- vector("list", length(item_vars))

    names(item_expected) <- item_vars
    names(item_probs) <- item_vars

    for (j in seq_along(item_vars)) {

      item <- item_vars[j]
      item_name <- item_cfa_names[j]

      observed_levels <- sort(unique(cfa_data[[item_name]][!is.na(cfa_data[[item_name]])]))

      probs <- cfa_cprob(
        fit = fit,
        item = item_name,
        theta = theta_star,
        factor_name = factor_name,
        item_levels = observed_levels
      )

      score_values <- item_score_values[[j]]
      common_levels <- intersect(names(probs), names(score_values))

      if (length(common_levels) == 0L) {
        stop(
          "Could not align probabilities with score values for item `",
          item,
          "`.",
          call. = FALSE
        )
      }

      item_probs[[j]] <- probs
      item_expected[j] <- sum(probs[common_levels] * score_values[common_levels])
    }

    mim_threshold_value <- sum(item_expected)

    list(
      threshold = as.numeric(mim_threshold_value),
      item_expected = item_expected,
      theta_star = as.numeric(theta_star),
      item_probs = item_probs,
      item_score_values = item_score_values,
      item_discretized = item_discretized,
      lavaan_model = lmodel,
      cfa_data = cfa_data,
      fit = if (isTRUE(keep_fit)) fit else NULL,
      anchor_var = anchor_var,
      anchor_obs_name = anchor_obs_name,
      item_vars = item_vars,
      item_cfa_names = item_cfa_names
    )
  }

  main <- fit_once(
    dat = mydata,
    keep_fit = TRUE,
    verbose_fit = verbose
  )

  # ---- Bootstrap ----
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
    item_expected = main$item_expected,
    lavaan_model = main$lavaan_model
  )

  attr(out, "details") <- list(
    call = match.call(),
    fit = main$fit,
    cfa_data = main$cfa_data,
    theta_star = main$theta_star,
    item_probs = main$item_probs,
    item_score_values = main$item_score_values,
    # item_discretized = main$item_discretized,
    anchor_var = main$anchor_var,
    # anchor_obs_name = main$anchor_obs_name,
    item_vars = main$item_vars,
    item_cfa_names = main$item_cfa_names,
    boot_thresholds = boot_thresholds,
    boot_success = boot_success
  )

  class(out) <- "mim_threshold"

  out
}







#' @export
print.mim_threshold <- function(x, ...) {

  cat("CFA-based MIM threshold\n")
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
    cat("Anchor variable:", details$anchor_var, "\n")
    cat("Number of items:", length(details$item_vars), "\n")
    cat("Items:", paste(details$item_vars, collapse = ", "), "\n")
  }

  cat("\nPlease use `mim_threshold_details(x)` to retrieve the lavaan fit, CFA data, probabilities, and bootstrap results.\n")

  invisible(x)
}




#' Extract hidden details from a mim_threshold object
#'
#' @param x A `mim_threshold` object.
#'
#' @return A list containing hidden details.
#'
#' @export
mim_threshold_details <- function(x) {

  if (!inherits(x, "mim_threshold")) {
    stop("`x` must be a mim_threshold object.", call. = FALSE)
  }

  attr(x, "details")
}

