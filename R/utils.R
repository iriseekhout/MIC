#' Compute ordered item category probabilities
#'
#' Internal helper: Computes category probabilities for an ordered lavaan indicator at a given
#' latent factor value. Used in `sim_threshold_cfa` and `mim_threshold`
#'
#' @param fit A fitted lavaan object.
#' @param item Ordered item name.
#' @param theta Latent factor value. If `NULL`, uses `cfa_theta_star(fit)`.
#' @param factor_name Latent factor name.
#' @param item_levels Optional category labels for returned probabilities.
#'
#' @return Named numeric vector of category probabilities.
#'
#'
#' @noRd
cfa_cprob <- function(
    fit,
    item,
    theta = NULL,
    factor_name = "F1",
    item_levels = NULL
) {
  if (is.null(theta)) {
    theta <- cfa_theta_star(fit)
  }

  pe <- lavaan::parameterEstimates(fit)

  lambda <- pe$est[
    pe$lhs == factor_name &
      pe$op == "=~" &
      pe$rhs == item
  ]

  if (length(lambda) != 1L) {
    stop("Could not uniquely identify loading for item: ", item, call. = FALSE)
  }

  tau_tab <- pe[pe$lhs == item & pe$op == "|", , drop = FALSE]

  if (nrow(tau_tab) == 0L) {
    stop("No thresholds found for item: ", item, call. = FALSE)
  }

  tau_order <- suppressWarnings(as.integer(sub("^t", "", tau_tab$rhs)))

  if (all(is.na(tau_order))) {
    tau <- sort(tau_tab$est)
  } else {
    tau <- tau_tab$est[order(tau_order)]
  }

  eta <- lambda * theta
  z <- stats::pnorm(tau - eta)

  p <- c( z[1L], diff(z),1 - z[length(z)])

  p[p < 0 & p > -1e-12] <- 0
  p[p > 1 & p < 1 + 1e-12] <- 1
  p <- p / sum(p)

  if (!is.null(item_levels)) {
    if (length(item_levels) != length(p)) {
      stop(
        "`item_levels` must have length ", length(p),
        " for item `", item, "`.",
        call. = FALSE
      )
    }

    names(p) <- as.character(item_levels)
  } else {
    names(p) <- as.character(seq_along(p))
  }

  p
}



#' Extract latent threshold location
#'
#' Internal helper. Computes theta_star as the ratio of a labelled anchor
#' threshold to a labelled anchor loading.
#'
#' @noRd
cfa_theta_star <- function(
    fit,
    threshold_label = "tau_anchor",
    loading_label = "lambda_anchor") {
  pe <- lavaan::parameterEstimates(fit)

  tau <- pe$est[pe$label == threshold_label]
  lambda <- pe$est[pe$label == loading_label]

  if (length(tau) != 1L) {
    stop("Could not uniquely identify threshold label: ", threshold_label, call. = FALSE)
  }

  if (length(lambda) != 1L) {
    stop("Could not uniquely identify loading label: ", loading_label, call. = FALSE)
  }

  if (!is.finite(tau) || !is.finite(lambda) ||
      abs(lambda) < .Machine$double.eps^0.5) {
    stop("Invalid threshold/loading estimates.", call. = FALSE)
  }

  as.numeric(tau / lambda)
}
