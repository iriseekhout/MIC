#' Generate lavaan syntax for longitudinal CFA MIC estimation
#'
#' `lcfa_model()` generates lavaan model syntax for estimating anchor-based minimal important change (MIC)
#' using longitudinal confirmatory factor analysis.
#'
#' The generated model is intended for ordinal PROM items and a binary
#' transition rating item. Item thresholds are constrained equal across Time 1
#' and Time 2 within each item pair. The number of thresholds is determined
#' automatically from the observed response levels in the supplied data.
#'
#' Item pairs can be detected either by column position or by suffix patterns,
#' using the same logic as `equalize_levels()`.
#'
#' The model defines:
#'
#' \preformatted{
#' psb := (f1/f2) + 1
#' b_param := thr.trt/f2
#' }
#'
#' @param data A data frame containing paired Time 1 and Time 2 PROM items and
#'   a transition rating variable.
#' @param trt Character. Name of the transition rating variable.
#' @param pair_by Pairing method. `"position"` assumes the first half of item
#'   columns are Time 1 items and the second half are Time 2 items. `"suffix"`
#'   detects item pairs using `t1_suffix` and `t2_suffix`.
#' @param t1_suffix regex suffix identifying Time 1 items when
#'   `pair_by = "suffix"`. Use `""` when Time 1 items have no suffix.
#' @param t2_suffix regex suffix identifying Time 2 items when
#'   `pair_by = "suffix"`.
#' @param pair_map Optional data frame describing item pairs, typically from
#'   `equalize_levels()$pair_map`. If supplied, it takes precedence over
#'   `pair_by`, `t1_suffix`, and `t2_suffix`.
#' @param factor_t1 Character. Name of the Time 1 latent factor.
#' @param factor_t2 Character. Name of the Time 2 latent factor.
#' @param loading_prefix Character. Prefix for equality-constrained item
#'   loading labels.
#' @param threshold_prefix Character. Prefix for equality-constrained item
#'   threshold labels.
#' @param trt_loading_t1 Character. Label for the transition rating loading
#'   on the Time 1 factor.
#' @param trt_loading_t2 Character. Label for the transition rating loading
#'   on the Time 2 factor.
#' @param trt_threshold_label Character. Label for the transition rating
#'   threshold.
#' @param psb_label Character. Name of the defined present-state-bias parameter.
#' @param mic_label Character. Name of the defined MIC parameter on the latent
#'   theta scale.
#' @param correlated_errors Logical. If `TRUE`, add correlated residuals
#'   between corresponding Time 1 and Time 2 items.
#' @param threshold_invariance Logical. If `TRUE`, constrain thresholds equal
#'   across Time 1 and Time 2 within each item pair.
#' @param include_residual_variances_t2 Logical. If `TRUE`, frees residual
#'   variances of Time 2 items using `item_t2 ~~ NA*item_t2`.
#' @param include_factor_structure Logical. If `TRUE`, adds factor variances,
#'   covariance, and latent means/intercepts sections.
#' @param include_comments Logical. If `TRUE`, include section comments in the
#'   generated lavaan syntax.
#' @param print_model Logical. If `TRUE`, prints the generated lavaan syntax
#'   for easy inspection.
#'
#' @return An object of class `lcfa_model`, invisibly. The generated lavaan
#'   syntax can be accessed using `$model`.
#'
#' @export
lcfa_model <- function(
    data,
    trt,
    pair_by = c("position", "suffix"),
    t1_suffix = NULL,
    t2_suffix = NULL,
    pair_map = NULL,
    factor_t1 = "F1",
    factor_t2 = "F2",
    loading_prefix = "a",
    threshold_prefix = "b",
    trt_loading_t1 = "f1",
    trt_loading_t2 = "f2",
    trt_threshold_label = "thr.trt",
    psb_label = "psb",
    mic_label = "b_param",
    correlated_errors = TRUE,
    threshold_invariance = TRUE,
    include_residual_variances_t2 = TRUE,
    include_factor_structure = TRUE,
    include_comments = TRUE,
    print_model = TRUE
) {

  pair_by <- match.arg(pair_by)

  # ---------------------------------------------------------------------------
  # Checks
  # ---------------------------------------------------------------------------

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.character(trt) || length(trt) != 1L || is.na(trt)) {
    stop("`trt` must be a single character string.", call. = FALSE)
  }

  if (!trt %in% names(data)) {
    stop("Transition rating variable `", trt, "` not found in `data`.", call. = FALSE)
  }

  non_numeric <- names(data)[!vapply(data, is.numeric, logical(1))]

  if (length(non_numeric) > 0L) {
    stop(
      "All variables used in the LCFA model must be numeric. Non-numeric variables: ",
      paste(non_numeric, collapse = ", "),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Helper: suffix regex
  # ---------------------------------------------------------------------------

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
  # Identify paired items
  # ---------------------------------------------------------------------------

  if (!is.null(pair_map)) {

    required_cols <- c("t1_item", "t2_item")
    missing_cols <- setdiff(required_cols, names(pair_map))

    if (length(missing_cols) > 0L) {
      stop(
        "`pair_map` must contain columns: ",
        paste(required_cols, collapse = ", "),
        call. = FALSE
      )
    }

    t1_items <- pair_map$t1_item
    t2_items <- pair_map$t2_item

    missing_items <- setdiff(c(t1_items, t2_items), names(data))

    if (length(missing_items) > 0L) {
      stop(
        "Items in `pair_map` not found in `data`: ",
        paste(missing_items, collapse = ", "),
        call. = FALSE
      )
    }

    pair_map_out <- pair_map

  } else if (pair_by == "position") {

    item_names <- setdiff(names(data), trt)

    if (length(item_names) %% 2L != 0L) {
      stop(
        "When `pair_by = 'position'`, the item columns must contain an even ",
        "number of variables: Time 1 items followed by Time 2 items.",
        call. = FALSE
      )
    }

    nitems <- length(item_names) / 2L

    t1_items <- item_names[seq_len(nitems)]
    t2_items <- item_names[nitems + seq_len(nitems)]

    pair_map_out <- data.frame(
      item_id = seq_len(nitems),
      base_name = t1_items,
      t1_item = t1_items,
      t2_item = t2_items,
      stringsAsFactors = FALSE
    )

  } else {

    if (is.null(t1_suffix) || is.null(t2_suffix)) {
      stop(
        "`t1_suffix` and `t2_suffix` must be supplied when `pair_by = 'suffix'`.",
        call. = FALSE
      )
    }

    t1_pat <- suffix_pattern(t1_suffix)
    t2_pat <- suffix_pattern(t2_suffix)

    nm <- setdiff(names(data), trt)

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

      if (length(missing_t2) > 0L) {
        stop(
          "Time 2 items missing for Time 1 base names: ",
          paste(missing_t2, collapse = ", "),
          call. = FALSE
        )
      }

      missing_t1 <- setdiff(t2_base, t1_items)

      if (length(missing_t1) > 0L) {
        stop(
          "Time 1 items missing for Time 2 base names: ",
          paste(missing_t1, collapse = ", "),
          call. = FALSE
        )
      }

      t2_items_ordered <- t2_items[match(t1_items, t2_base)]

      pair_map_out <- data.frame(
        item_id = seq_along(t1_items),
        base_name = t1_items,
        t1_item = t1_items,
        t2_item = t2_items_ordered,
        stringsAsFactors = FALSE
      )

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

      if (length(missing_t2) > 0L) {
        stop(
          "Time 2 items missing for Time 1 base names: ",
          paste(missing_t2, collapse = ", "),
          call. = FALSE
        )
      }

      missing_t1 <- setdiff(t2_base, t1_base)

      if (length(missing_t1) > 0L) {
        stop(
          "Time 1 items missing for Time 2 base names: ",
          paste(missing_t1, collapse = ", "),
          call. = FALSE
        )
      }

      t2_items_ordered <- t2_items[match(t1_base, t2_base)]

      pair_map_out <- data.frame(
        item_id = seq_along(t1_items),
        base_name = t1_base,
        t1_item = t1_items,
        t2_item = t2_items_ordered,
        stringsAsFactors = FALSE
      )
    }

    t1_items <- pair_map_out$t1_item
    t2_items <- pair_map_out$t2_item
  }

  nitems <- length(t1_items)

  if (nitems < 1L) {
    stop("No paired items were identified.", call. = FALSE)
  }

  if (length(t2_items) != nitems) {
    stop("Unequal number of Time 1 and Time 2 items.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Helper functions
  # ---------------------------------------------------------------------------

  section <- function(title) {
    if (isTRUE(include_comments)) {
      paste0("\n# ", title, "\n")
    } else {
      "\n"
    }
  }

  present <- function(x) {
    sort(unique(x[!is.na(x)]))
  }

  make_threshold_terms <- function(prefix, item_index, n_thresholds) {
    labels <- paste0(prefix, item_index, "_", seq_len(n_thresholds))
    paste0(labels, "*t", seq_len(n_thresholds))
  }

  # ---------------------------------------------------------------------------
  # Validate paired item levels and determine number of thresholds
  # ---------------------------------------------------------------------------

  n_thresholds <- integer(nitems)
  item_levels <- vector("list", nitems)
  level_summary <- vector("list", nitems)
  unequal_level_pairs <- character(0L)

  names(item_levels) <- t1_items

  for (i in seq_len(nitems)) {

    lev_t1 <- present(data[[t1_items[i]]])
    lev_t2 <- present(data[[t2_items[i]]])

    if (length(lev_t1) < 2L || length(lev_t2) < 2L) {
      stop(
        "Item pair `", t1_items[i], "` / `", t2_items[i],
        "` must each have at least two observed levels.",
        call. = FALSE
      )
    }

    if (!identical(lev_t1, lev_t2)) {
      unequal_level_pairs <- c(
        unequal_level_pairs,
        paste0("`", t1_items[i], "` / `", t2_items[i], "`")
      )
    }

    lev_union <- sort(unique(c(lev_t1, lev_t2)))

    item_levels[[i]] <- lev_union
    n_thresholds[i] <- length(lev_union) - 1L

    level_summary[[i]] <- data.frame(
      item_pair = i,
      base_name = pair_map_out$base_name[i],
      t1_item = t1_items[i],
      t2_item = t2_items[i],
      n_levels_t1 = length(lev_t1),
      n_levels_t2 = length(lev_t2),
      n_levels_union = length(lev_union),
      levels_t1 = paste(lev_t1, collapse = ", "),
      levels_t2 = paste(lev_t2, collapse = ", "),
      levels_union = paste(lev_union, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }

  if (length(unequal_level_pairs) > 0L) {
    warning(
      "Item pairs without same observed values: ",
      paste(unequal_level_pairs, collapse = ", "),
      ". Consider running `equalize_levels()` before `lcfa_model()`.",
      call. = FALSE
    )
  }

  level_summary <- do.call(rbind, level_summary)

  trt_levels <- present(data[[trt]])

  if (length(trt_levels) != 2L) {
    stop(
      "Transition rating variable `", trt,
      "` must have exactly two observed levels in the model data. ",
      "Dichotomize it before calling `lcfa_model()`.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Factors
  # ---------------------------------------------------------------------------

  loading_labels <- paste0(loading_prefix, seq_len(nitems))

  f1_terms <- paste0(loading_labels, "*", t1_items)
  f2_terms <- paste0(loading_labels, "*", t2_items)

  factor_block <- paste0(
    section("Factors"),
    factor_t1, " =~ ",
    paste(c(f1_terms, paste0(trt_loading_t1, "*", trt)), collapse = " + "),
    "\n",
    factor_t2, " =~ ",
    paste(c(f2_terms, paste0(trt_loading_t2, "*", trt)), collapse = " + "),
    "\n"
  )

  # ---------------------------------------------------------------------------
  # Correlated errors over time
  # ---------------------------------------------------------------------------

  if (isTRUE(correlated_errors)) {

    error_block <- paste0(
      section("Correlated errors over time"),
      paste0(t1_items, " ~~ ", t2_items, collapse = "\n"),
      "\n"
    )

  } else {
    error_block <- ""
  }

  # ---------------------------------------------------------------------------
  # Thresholds
  # ---------------------------------------------------------------------------

  threshold_lines <- character(nitems)

  for (i in seq_len(nitems)) {

    threshold_terms <- make_threshold_terms(
      prefix = threshold_prefix,
      item_index = i,
      n_thresholds = n_thresholds[i]
    )

    if (isTRUE(threshold_invariance)) {

      threshold_lines[i] <- paste0(
        t1_items[i], " + ", t2_items[i], " | ",
        paste(threshold_terms, collapse = " + ")
      )

    } else {

      threshold_terms_t1 <- paste0(
        threshold_prefix, i, "_t1_", seq_len(n_thresholds[i]),
        "*t", seq_len(n_thresholds[i])
      )

      threshold_terms_t2 <- paste0(
        threshold_prefix, i, "_t2_", seq_len(n_thresholds[i]),
        "*t", seq_len(n_thresholds[i])
      )

      threshold_lines[i] <- paste0(
        t1_items[i], " | ", paste(threshold_terms_t1, collapse = " + "),
        "\n",
        t2_items[i], " | ", paste(threshold_terms_t2, collapse = " + ")
      )
    }
  }

  threshold_block <- paste0(
    section("Thresholds"),
    paste(threshold_lines, collapse = "\n"),
    "\n",
    trt, " | ", trt_threshold_label, "*t1\n"
  )

  # ---------------------------------------------------------------------------
  # Variances / covariances
  # ---------------------------------------------------------------------------

  if (isTRUE(include_factor_structure)) {

    variance_lines <- c(
      paste0(factor_t1, " ~~ 1*", factor_t1),
      paste0(factor_t2, " ~~ NA*", factor_t2),
      paste0(factor_t1, " ~~ NA*", factor_t2)
    )

    if (isTRUE(include_residual_variances_t2)) {
      variance_lines <- c(
        variance_lines,
        paste0(t2_items, " ~~ NA*", t2_items)
      )
    }

    variance_block <- paste0(
      section("Variances/covariances"),
      paste(variance_lines, collapse = "\n"),
      "\n"
    )

    mean_block <- paste0(
      section("Means/intercepts"),
      factor_t1, " ~ 0*1\n",
      factor_t2, " ~ NA*1\n"
    )

  } else {

    variance_block <- ""
    mean_block <- ""
  }

  # ---------------------------------------------------------------------------
  # Derived parameters
  # ---------------------------------------------------------------------------

  derived_block <- paste0(
    section("Derived parameters"),
    psb_label, " := (", trt_loading_t1, "/", trt_loading_t2, ") + 1\n",
    mic_label, " := ", trt_threshold_label, "/", trt_loading_t2, "\n"
  )

  # ---------------------------------------------------------------------------
  # Assemble model
  # ---------------------------------------------------------------------------

  model_out <- paste0(
    factor_block,
    error_block,
    threshold_block,
    variance_block,
    mean_block,
    derived_block
  )

  out <- list(
    model = model_out,
    t1_items = t1_items,
    t2_items = t2_items,
    trt = trt,
    pair_map = pair_map_out,
    item_levels = item_levels,
    n_thresholds = n_thresholds,
    level_summary = level_summary
  )

  class(out) <- "lcfa_model"

  if (isTRUE(print_model)) {
    print(out)
  }

  invisible(out)
}





#' @export
print.lcfa_model <- function(x, ...) {

  cat("\n================ LCFA LAVAAN MODEL ================\n\n")
  cat(x$model)
  cat("\n====================================================\n")

  invisible(x)
}

