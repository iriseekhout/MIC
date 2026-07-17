#' Generate CFA syntax for anchor reliability
#'
#' `tr_reliability_model()` generates lavaan syntax for estimating the
#' reliability of an anchor or transition rating item using confirmatory factor
#' analysis.
#'
#' The generated model follows the approach described by Griffiths et al.
#' J Clin Epidemiol. 2022;141:36-45. No constraints are placed on loadings or
#' thresholds. For longitudinal data, residuals of corresponding items across
#' time-points are allowed to correlate to account for item non-independence.
#'
#' For cross-sectional data, a one-factor model is generated. For longitudinal
#' data, a two-factor model is generated, with the anchor loading on both
#' Time 1 and Time 2 factors.
#'
#' @param data A data frame containing items and an anchor variable.
#' @param anchor Character. Name of the anchor variable. If `NULL`, the final
#'   column of `data` is treated as the anchor.
#' @param xsec Logical. If `TRUE`, generate a cross-sectional one-factor model.
#'   If `FALSE`, generate a longitudinal two-factor model.
#' @param pair_by Pairing method for longitudinal data. `"position"` assumes
#'   Time 1 items followed by Time 2 items. `"suffix"` detects pairs using
#'   `t1_suffix` and `t2_suffix`.
#' @param t1_suffix Regex suffix identifying Time 1 items when
#'   `pair_by = "suffix"`. Use `""` when Time 1 items have no suffix.
#' @param t2_suffix Regex suffix identifying Time 2 items when
#'   `pair_by = "suffix"`.
#' @param factor_names Optional character vector of factor name(s). If `NULL`,
#'   defaults to `"F1"` when `xsec = TRUE`, and `c("F1", "F2")` when
#'   `xsec = FALSE`.
#' @param print_model Logical. If `TRUE`, print the generated lavaan syntax.
#'
#' @return An object of class `tr_reliability_model`. The lavaan syntax can be
#'   accessed using `$model`.
#'
#' @examples
#' sim <- simdat(N = 200, seed = 123)
#' dat <- sim$datw
#'
#' tr_reliability_model(
#'   data = dat[, c(sim$item_names$t1_items,
#'                  sim$item_names$t2_items,
#'                  "trat")],
#'   anchor = "trat",
#'   xsec = FALSE,
#'   pair_by = "suffix",
#'   t1_suffix = "",
#'   t2_suffix = "\\.1"
#' )
#' @export
tr_reliability_model <- function(
    data,
    anchor = NULL,
    xsec = FALSE,
    pair_by = c("position", "suffix"),
    t1_suffix = NULL,
    t2_suffix = NULL,
    factor_names = NULL,
    print_model = TRUE
) {

  pair_by <- match.arg(pair_by)

  # ---------------------------------------------------------------------------
  # Basic checks
  # ---------------------------------------------------------------------------

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

  if (is.null(factor_names)) {
    factor_names <- if (isTRUE(xsec)) {
      "F1"
    } else {
      c("F1", "F2")
    }
  }

  if (!is.character(factor_names) || anyNA(factor_names)) {
    stop("`factor_names` must be a non-missing character vector.", call. = FALSE)
  }

  if (isTRUE(xsec) && length(factor_names) != 1L) {
    stop(
      "When `xsec = TRUE`, `factor_names` must contain exactly one factor name.",
      call. = FALSE
    )
  }

  if (!isTRUE(xsec) && length(factor_names) != 2L) {
    stop(
      "When `xsec = FALSE`, `factor_names` must contain exactly two factor names.",
      call. = FALSE
    )
  }

  item_names <- setdiff(names(data), anchor)

  if (length(item_names) < 1L) {
    stop("No item variables found.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  section <- function(title) {
    paste0("\n# ", title, "\n")
  }

  suffix_pattern <- function(x) {

    if (is.null(x)) {
      return(NULL)
    }

    if (identical(x, "")) {
      return("")
    }

    if (grepl("\\$$", x)) {
      x
    } else {
      paste0(x, "$")
    }
  }

  # ---------------------------------------------------------------------------
  # Cross-sectional model
  # ---------------------------------------------------------------------------

  if (isTRUE(xsec)) {

    model_out <- paste0(
      section("Factor"),
      factor_names[1L], " =~ ",
      paste(c(item_names, anchor), collapse = " + "),
      "\n"
    )

    out <- list(
      model = model_out,
      xsec = TRUE,
      anchor = anchor,
      items = item_names,
      t1_items = item_names,
      t2_items = NULL
    )

    class(out) <- "tr_reliability_model"

    if (isTRUE(print_model)) {
      print(out)
    }

    return(invisible(out))
  }

  # ---------------------------------------------------------------------------
  # Longitudinal model: identify item pairs
  # ---------------------------------------------------------------------------

  if (pair_by == "position") {

    if (length(item_names) %% 2L != 0L) {
      stop(
        "When `pair_by = 'position'`, item columns must contain an even ",
        "number of variables: Time 1 items followed by Time 2 items.",
        call. = FALSE
      )
    }

    nitems <- length(item_names) / 2L

    t1_items <- item_names[seq_len(nitems)]
    t2_items <- item_names[nitems + seq_len(nitems)]

  } else {

    if (is.null(t1_suffix) || is.null(t2_suffix)) {
      stop(
        "`t1_suffix` and `t2_suffix` must be supplied when `pair_by = 'suffix'`.",
        call. = FALSE
      )
    }

    t1_pat <- suffix_pattern(t1_suffix)
    t2_pat <- suffix_pattern(t2_suffix)

    nm <- item_names

    if (identical(t1_pat, "")) {

      # Example:
      # Time 1: item1
      # Time 2: item1.1

      t2_items <- nm[grepl(t2_pat, nm)]
      t2_base <- sub(t2_pat, "", t2_items)

      if (length(t2_items) == 0L) {
        stop("No Time 2 items found using `t2_suffix`.", call. = FALSE)
      }

      t1_items <- setdiff(nm, t2_items)

      if (length(t1_items) == 0L) {
        stop("No Time 1 items found.", call. = FALSE)
      }

      missing_t2 <- setdiff(t1_items, t2_base)
      missing_t1 <- setdiff(t2_base, t1_items)

      if (length(missing_t2) > 0L) {
        stop(
          "Time 2 items missing for Time 1 base names: ",
          paste(missing_t2, collapse = ", "),
          call. = FALSE
        )
      }

      if (length(missing_t1) > 0L) {
        stop(
          "Time 1 items missing for Time 2 base names: ",
          paste(missing_t1, collapse = ", "),
          call. = FALSE
        )
      }

      # Preserve Time 1 item order
      t2_items <- t2_items[match(t1_items, t2_base)]

    } else {

      # Example:
      # Time 1: item.t1
      # Time 2: item.t2

      t1_items <- nm[grepl(t1_pat, nm)]
      t2_items <- nm[grepl(t2_pat, nm)]

      if (length(t1_items) == 0L) {
        stop("No Time 1 items found using `t1_suffix`.", call. = FALSE)
      }

      if (length(t2_items) == 0L) {
        stop("No Time 2 items found using `t2_suffix`.", call. = FALSE)
      }

      t1_base <- sub(t1_pat, "", t1_items)
      t2_base <- sub(t2_pat, "", t2_items)

      if (anyDuplicated(t1_base)) {
        stop("Duplicate Time 1 base names detected.", call. = FALSE)
      }

      if (anyDuplicated(t2_base)) {
        stop("Duplicate Time 2 base names detected.", call. = FALSE)
      }

      missing_t2 <- setdiff(t1_base, t2_base)
      missing_t1 <- setdiff(t2_base, t1_base)

      if (length(missing_t2) > 0L) {
        stop(
          "Time 2 items missing for Time 1 base names: ",
          paste(missing_t2, collapse = ", "),
          call. = FALSE
        )
      }

      if (length(missing_t1) > 0L) {
        stop(
          "Time 1 items missing for Time 2 base names: ",
          paste(missing_t1, collapse = ", "),
          call. = FALSE
        )
      }

      # Preserve Time 1 item order
      t2_items <- t2_items[match(t1_base, t2_base)]
    }
  }

  # ---------------------------------------------------------------------------
  # Longitudinal model syntax
  # ---------------------------------------------------------------------------

  f1 <- paste0(
    factor_names[1L], " =~ ",
    paste(c(t1_items, anchor), collapse = " + ")
  )

  f2 <- paste0(
    factor_names[2L], " =~ ",
    paste(c(t2_items, anchor), collapse = " + ")
  )

  error_block <- paste0(
    section("Correlated errors over time"),
    paste0(t1_items, " ~~ ", t2_items, collapse = "\n"),
    "\n"
  )

  model_out <- paste0(
    section("Factors"),
    f1, "\n",
    f2, "\n",
    error_block
  )

  out <- list(
    model = model_out,
    xsec = FALSE,
    anchor = anchor,
    items = item_names,
    t1_items = t1_items,
    t2_items = t2_items
  )

  class(out) <- "tr_reliability_model"

  if (isTRUE(print_model)) {
    print(out)
  }

  invisible(out)
}


#' @export
print.tr_reliability_model <- function(x, ...) {

  cat("\n================ ANCHOR RELIABILITY CFA MODEL ================\n\n")
  cat(x$model)
  cat("\n===============================================================\n")

  invisible(x)
}

