#' Estimate anchor reliability using CFA
#'
#' `tr_reliability()` estimates the reliability of an anchor or transition
#' rating item using confirmatory factor analysis. The reliability estimate is
#' the R-squared value of the anchor item from the fitted CFA model.
#'
#' The function follows the CFA approach described by Griffiths et al.
#' J Clin Epidemiol. 2022;141:36-45. For longitudinal data, Time 1 items
#' load on the first factor, Time 2 items load on the second factor, and
#' the anchor loads on both factors. Residuals of corresponding items across
#' time-points are allowed to correlate. No constraints are placed on loadings
#' or thresholds.
#'
#' For cross-sectional data, a one-factor CFA model is used, with the anchor
#' item included as an indicator of the latent factor.
#'
#' @param data A data frame containing items and an anchor variable.
#' @param model Optional lavaan model syntax. If `NULL`, syntax is generated
#'   automatically using `tr_reliability_model()`.
#' @param anchor Character. Name of the anchor variable. If `NULL`, the final
#'   column of `data` is treated as the anchor.
#' @param xsec Logical. If `TRUE`, estimate cross-sectional anchor reliability.
#'   If `FALSE`, estimate longitudinal anchor / transition-rating reliability.
#' @param pair_by Pairing method for longitudinal data. `"position"` assumes
#'   Time 1 items followed by Time 2 items. `"suffix"` detects pairs using
#'   `t1_suffix` and `t2_suffix`.
#' @param t1_suffix Regex suffix identifying Time 1 items when
#'   `pair_by = "suffix"`.
#' @param t2_suffix Regex suffix identifying Time 2 items when
#'   `pair_by = "suffix"`.
#' @param factor_names Optional character vector of factor name(s). If `NULL`,
#'   defaults to `"F1"` when `xsec = TRUE`, and `c("F1", "F2")` when
#'   `xsec = FALSE`.
#' @param item_type Optional character. Type of items used as indicators.
#'   If `NULL`, defaults to `"ordinal"` unless `continuous_items` is supplied,
#'   in which case it defaults to `"mixed"`. `"ordinal"` treats all items and
#'   the anchor as ordered categorical variables. `"continuous"` treats all
#'   items as continuous while still treating the anchor as ordered. `"mixed"`
#'   treats all items as ordered except those listed in `continuous_items`;
#'   the anchor is always treated as ordered.
#' @param continuous_items Optional character vector of item names to treat as
#'   continuous. If supplied and `item_type = NULL`, `item_type` is automatically
#'   set to `"mixed"`. Do not include the anchor variable; it is always treated
#'   as ordered.
#' @param modification Logical. If `TRUE`, return modification indices with
#'   `sepc.lv > mi_cut`.
#' @param mi_cut Numeric. Cutoff for standardized latent-variable modification
#'   indices.
#' @param complete_cases Logical. If `TRUE`, retain only complete cases before
#'   fitting the model.
#' @param print_model Logical. If `TRUE`, print the generated lavaan syntax.
#' @param verbose Logical. If `TRUE`, print progress messages.
#' @param ... Additional arguments passed to `lavaan::cfa()`.
#'
#' @return An object of class `tr_reliability`.
#'
#' @examples
#' \dontrun{
#' sim <- simdat(N = 300, seed = 123)
#' dat <- sim$datw
#'
#' rel <- tr_reliability(
#'   data = dat[, c(sim$item_names$t1_items,
#'                  sim$item_names$t2_items,
#'                  "trat")],
#'   anchor = "trat",
#'   xsec = FALSE,
#'   pair_by = "suffix",
#'   t1_suffix = "",
#'   t2_suffix = "\\.1",
#'   item_type = "ordinal",
#'   modification = FALSE,
#'   print_model = FALSE
#' )
#'
#' rel
#' }
#'
#' @export
tr_reliability <- function(
    data,
    model = NULL,
    anchor = NULL,
    xsec = FALSE,
    pair_by = c("position", "suffix"),
    t1_suffix = NULL,
    t2_suffix = NULL,
    factor_names = NULL,
    item_type = NULL,
    continuous_items = NULL,
    modification = TRUE,
    mi_cut = 0.30,
    complete_cases = TRUE,
    print_model = TRUE,
    verbose = TRUE,
    ...
) {

  pair_by <- match.arg(pair_by)

  # -------------------------------------------------------------------------
  # Basic checks
  # -------------------------------------------------------------------------

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (ncol(data) < 2L) {
    stop("`data` must contain at least one item and one anchor variable.", call. = FALSE)
  }

  if (is.null(anchor)) {
    anchor <- names(data)[ncol(data)]
    message(
      "`anchor` was not supplied. Treating the last variable in `data` as the anchor: `",
      anchor,
      "`."
    )
  }

  if (!is.character(anchor) || length(anchor) != 1L || is.na(anchor)) {
    stop("`anchor` must be a single character string.", call. = FALSE)
  }

  if (!anchor %in% names(data)) {
    stop("Anchor variable `", anchor, "` not found in `data`.", call. = FALSE)
  }

  non_numeric <- names(data)[!vapply(data, is.numeric, logical(1))]

  if (length(non_numeric) > 0L) {
    stop(
      "All variables must be numeric. Non-numeric variables: ",
      paste(non_numeric, collapse = ", "),
      call. = FALSE
    )
  }

  if (isTRUE(complete_cases)) {
    data <- data[stats::complete.cases(data), , drop = FALSE]
  }

  if (nrow(data) == 0L) {
    stop("No rows remain after applying complete-case filtering.", call. = FALSE)
  }

  item_vars <- setdiff(names(data), anchor)

  # -------------------------------------------------------------------------
  # Item type
  # -------------------------------------------------------------------------

  if (is.null(item_type)) {
    item_type <- if (is.null(continuous_items)) {
      "ordinal"
    } else {
      "mixed"
    }
  }

  item_type <- match.arg(
    item_type,
    choices = c("ordinal", "continuous", "mixed")
  )

  if (!is.null(continuous_items) && item_type != "mixed") {
    warning(
      "`continuous_items` was supplied but `item_type` is not 'mixed'. ",
      "Setting `item_type = 'mixed'.",
      call. = FALSE
    )
    item_type <- "mixed"
  }

  # -------------------------------------------------------------------------
  # Generate model if not supplied
  # -------------------------------------------------------------------------

  model_obj <- NULL

  if (is.null(model)) {

    model_obj <- tr_reliability_model(
      data = data,
      anchor = anchor,
      xsec = xsec,
      pair_by = pair_by,
      t1_suffix = t1_suffix,
      t2_suffix = t2_suffix,
      factor_names = factor_names,
      print_model = FALSE
    )

    model_used <- model_obj$model

    if (isTRUE(print_model)) {
      cat("\n================ ANCHOR RELIABILITY CFA MODEL ================\n\n")
      cat(model_used)
      cat("\n===============================================================\n")
    }

  } else {

    model_used <- model

    if (isTRUE(print_model)) {
      cat("\n================ ANCHOR RELIABILITY CFA MODEL ================\n\n")
      cat(model_used)
      cat("\n===============================================================\n")
    }
  }

  # -------------------------------------------------------------------------
  # Decide which variables are ordered in lavaan
  # -------------------------------------------------------------------------

  if (item_type == "ordinal") {

    # All items and the anchor are ordered
    ordered_vars <- TRUE

  } else if (item_type == "continuous") {

    # All items continuous; anchor remains ordered
    ordered_vars <- anchor

  } else if (item_type == "mixed") {

    if (is.null(continuous_items)) {
      stop(
        "`continuous_items` must be supplied when `item_type = 'mixed'`.",
        call. = FALSE
      )
    }

    if (!is.character(continuous_items)) {
      stop("`continuous_items` must be a character vector.", call. = FALSE)
    }

    missing_continuous <- setdiff(continuous_items, names(data))

    if (length(missing_continuous) > 0L) {
      stop(
        "Variables in `continuous_items` not found in `data`: ",
        paste(missing_continuous, collapse = ", "),
        call. = FALSE
      )
    }

    if (anchor %in% continuous_items) {
      stop(
        "`anchor` should not be included in `continuous_items`; ",
        "the anchor is always treated as ordered.",
        call. = FALSE
      )
    }

    invalid_continuous <- setdiff(continuous_items, item_vars)

    if (length(invalid_continuous) > 0L) {
      stop(
        "`continuous_items` should only contain item variables. Invalid names: ",
        paste(invalid_continuous, collapse = ", "),
        call. = FALSE
      )
    }

    # Most items are ordinal; continuous_items are excluded.
    ordered_items <- setdiff(item_vars, continuous_items)

    ordered_vars <- unique(c(ordered_items, anchor))
  }

  # -------------------------------------------------------------------------
  # Fit CFA
  # -------------------------------------------------------------------------

  fit <- lavaan::cfa(
    model = model_used,
    data = data,
    std.lv = TRUE,
    ordered = ordered_vars,
    test = "mean.var.adjusted",
    ...
  )

  # -------------------------------------------------------------------------
  # Fit measures
  # -------------------------------------------------------------------------

  cfa_fitmeasures <- lavaan::fitMeasures(
    fit,
    fit.measures = c(
      "cfi.scaled",
      "tli.scaled",
      "rmsea.scaled",
      "rmsea.ci.lower.scaled",
      "rmsea.ci.upper.scaled",
      "rmsea.pvalue.scaled",
      "srmr"
    )
  )

  # -------------------------------------------------------------------------
  # Extract reliability = R2 of anchor
  # -------------------------------------------------------------------------

  pe <- lavaan::parameterEstimates(
    fit,
    standardized = TRUE,
    rsquare = TRUE
  )

  rel_anchor <- pe$est[
    pe$lhs == anchor &
      pe$op == "r2"
  ]

  if (length(rel_anchor) != 1L || !is.finite(rel_anchor)) {
    stop(
      "Could not uniquely identify finite R-squared for anchor `",
      anchor,
      "`.",
      call. = FALSE
    )
  }

  # -------------------------------------------------------------------------
  # Modification indices
  # -------------------------------------------------------------------------

  MI <- NULL
  mod_indices <- NULL

  if (isTRUE(modification)) {
    MI <- lavaan::modificationIndices(fit)
    mod_indices <- MI[MI$sepc.lv > mi_cut, , drop = FALSE]
  }

  # -------------------------------------------------------------------------
  # Output
  # -------------------------------------------------------------------------

  out <- list(
    rel_anchor = as.numeric(rel_anchor),
    reliability = as.numeric(rel_anchor),
    xsec = xsec,
    anchor = anchor,
    item_type = item_type,
    continuous_items = continuous_items,
    ordered = ordered_vars,
    model = model_used,
    model_object = model_obj,
    fit = fit,
    cfa_fitmeasures = cfa_fitmeasures,
    parameter_estimates = pe,
    mi = MI,
    mod_indices = mod_indices,
    data = data
  )

  class(out) <- "tr_reliability"

  out
}



#' @export
print.tr_reliability <- function(x, ...) {

  if (isTRUE(x$xsec)) {
    cat("Cross-sectional anchor reliability\n")
  } else {
    cat("Longitudinal anchor / transition-rating reliability\n")
  }

  cat("---------------------------------------------\n")
  cat("Anchor variable:", x$anchor, "\n")
  cat("Item type:", x$item_type, "\n")

  if (!is.null(x$continuous_items)) {
    cat("Continuous items:", paste(x$continuous_items, collapse = ", "), "\n")
  }

  cat("Reliability R-squared:", round(x$rel_anchor, 4), "\n")

  invisible(x)
}
