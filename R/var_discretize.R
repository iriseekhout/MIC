#' Discretize a continuous variable into equal-width ordered categories
#'
#' `var_discretize()` converts a continuous numeric vector into equal-width
#' integer categories. It is intended for creating ordered indicators for
#' lavaan-based CFA workflows. To be used with `sim_threshold_cfa`
#'
#' Missing values are allowed. They are ignored when computing the
#' discretization range and are preserved as `NA_integer_` in the returned
#' score vector.
#'
#' @param x Numeric vector to discretize.
#' @param n_levels Integer. Number of equal-width categories to create.
#'   Must be between 2 and 12.
#' @param min_level Integer. Lowest score value. Defaults to `1L`.
#' @param require_all_levels Logical. If `TRUE`, stop when the discretized
#'   variable does not use all requested levels.
#' @param warn_unused Logical. If `TRUE`, warn when some levels are unused.
#'   Ignored when `require_all_levels = TRUE`.
#'
#' @return A list containing the discretized score, bin midpoints, bin edges,
#'   used levels, and unused levels.
#'
#' @export
var_discretize <- function(
    x,
    n_levels = 10L,
    min_level = 1L,
    require_all_levels = TRUE,
    warn_unused = TRUE
) {

  if (!is.numeric(n_levels) || length(n_levels) != 1L || is.na(n_levels)) {
    stop("`n_levels` must be a single non-missing numeric value.", call. = FALSE)
  }

  n_levels <- as.integer(n_levels)

  if (n_levels < 2L) {
    stop("`n_levels` must be at least 2.", call. = FALSE)
  }

  if (n_levels > 12L) {
    stop(
      "`n_levels` must be <= 12 for this lavaan ordered-indicator workflow.",
      call. = FALSE
    )
  }

  if (!is.numeric(min_level) || length(min_level) != 1L || is.na(min_level)) {
    stop("`min_level` must be a single non-missing numeric value.", call. = FALSE)
  }

  min_level <- as.integer(min_level)
  max_level <- min_level + n_levels - 1L

  if (!is.numeric(x)) {
    stop("`x` must be numeric.", call. = FALSE)
  }

  if (length(x) == 0L) {
    stop("`x` has length 0.", call. = FALSE)
  }

  x_nonmiss <- x[!is.na(x)]

  if (length(x_nonmiss) == 0L) {
    stop("`x` contains no non-missing values.", call. = FALSE)
  }

  if (!all(is.finite(x_nonmiss))) {
    stop("`x` must contain only finite values or NA.", call. = FALSE)
  }

  n_unique <- length(unique(x_nonmiss))

  if (n_unique < 2L) {
    stop("`x` must have at least 2 unique non-missing values.", call. = FALSE)
  }

  if (n_unique < n_levels) {
    stop(
      "`x` has only ", n_unique, " unique non-missing values, ",
      "but `n_levels = ", n_levels, "` was requested. ",
      "Use fewer levels or provide a variable with more unique values.",
      call. = FALSE
    )
  }

  xmin <- min(x_nonmiss)
  xmax <- max(x_nonmiss)

  if (xmax <= xmin) {
    stop("`x` has no usable variation.", call. = FALSE)
  }

  edges <- seq(
    from = xmin,
    to = xmax,
    length.out = n_levels + 1L
  )

  score_raw <- cut(
    x,
    breaks = edges,
    include.lowest = TRUE,
    labels = FALSE,
    right = TRUE
  )

  score <- as.integer(score_raw) + min_level - 1L
  score[is.na(x)] <- NA_integer_

  all_levels <- seq.int(min_level, max_level)
  used_levels <- sort(unique(score[!is.na(score)]))
  unused_levels <- setdiff(all_levels, used_levels)

  if (isTRUE(require_all_levels) && length(unused_levels) > 0L) {
    stop(
      "Discretization did not produce all requested levels. ",
      "Requested levels: ", paste(all_levels, collapse = ", "), ". ",
      "Observed levels: ", paste(used_levels, collapse = ", "), ". ",
      "Unused levels: ", paste(unused_levels, collapse = ", "), ". ",
      "Consider using fewer levels or setting `require_all_levels = FALSE`.",
      call. = FALSE
    )
  }

  if (!isTRUE(require_all_levels) &&
      isTRUE(warn_unused) &&
      length(unused_levels) > 0L) {

    warning(
      "var_discretize(): unused levels detected: ",
      paste(unused_levels, collapse = ", "),
      ". This is usually acceptable if the variable is passed to lavaan ",
      "as numeric and declared ordinal using `ordered = ...`.",
      call. = FALSE
    )
  }

  midpoints_all <- (edges[-length(edges)] + edges[-1L]) / 2
  names(midpoints_all) <- as.character(all_levels)

  midpoints <- midpoints_all[as.character(used_levels)]

  list(
    score = as.integer(score),
    midpoints = midpoints,
    midpoints_all = midpoints_all,
    edges = edges,
    used_levels = used_levels,
    unused_levels = unused_levels,
    n_missing = sum(is.na(x)),
    n_nonmissing = sum(!is.na(x))
  )
}
