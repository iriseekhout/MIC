#' ROC-Based Minimal Important Change
#'
#' Estimates the minimal important change (MIC) using receiver operating
#' characteristic (ROC) analysis. The MIC is estimated as the change-score
#' threshold that optimizes the Youden criterion.
#'
#' Optional bootstrap confidence intervals can be requested by setting
#' `nboot >= 100`.
#'
#' @param data Optional data frame containing the Time 1 score, Time 2 score,
#'   and binary anchor.
#' @param x Numeric vector of scores at Time 1, or character name of the Time 1
#'   score column in `data`.
#' @param y Numeric vector of scores at Time 2, or character name of the Time 2
#'   score column in `data`.
#' @param tr Binary anchor vector, or character name of the anchor column in
#'   `data`. The anchor must be coded as 0/1 or TRUE/FALSE.
#' @param nboot Integer. Number of bootstrap samples for estimating the 95%
#'   confidence interval. Bootstrapping is performed only when `nboot >= 100`.
#' @param report_every Integer. The interval at which bootstrap progress should
#'   be printed.
#' @param verbose Logical. If `TRUE`, progress messages are printed.
#' @param seed Optional integer seed for reproducible bootstrap confidence
#'   intervals.
#' @param max_attempts Integer. Maximum number of bootstrap attempts. This avoids
#'   an infinite loop when many bootstrap samples fail.
#'
#' @return A `mic_roc` object containing the ROC-based MIC and, optionally,
#'   bootstrap confidence intervals.
#'
#' @importFrom pROC roc coords
#'
#' @examples
#' data(example)
#'
#' nitems <- 10
#'
#' example$score_t1 <- rowSums(example[, paste0("v1_", seq_len(nitems))])
#' example$score_t2 <- rowSums(example[, paste0("v2_", seq_len(nitems))])
#'
#' mic_roc(
#'   data = example,
#'   x = "score_t1",
#'   y = "score_t2",
#'   tr = "trat",
#'   nboot = 0
#' )
#'
#' \dontrun{
#' mic_roc(
#'   data = example,
#'   x = "score_t1",
#'   y = "score_t2",
#'   tr = "trat",
#'   nboot = 500,
#'   seed = 123
#' )
#' }
#'
#' @export
mic_roc <- function(
    data = NULL,
    x,
    y,
    tr,
    nboot = 0,
    report_every = 100,
    verbose = FALSE,
    seed = NULL,
    max_attempts = nboot * 5
) {

  # -------------------------------------------------------------------------
  # Validate general arguments
  # -------------------------------------------------------------------------

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be either TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(nboot) || length(nboot) != 1L || is.na(nboot) ||
      !is.finite(nboot) || nboot < 0 || nboot != floor(nboot)) {
    stop("`nboot` must be a single non-negative integer.", call. = FALSE)
  }

  nboot <- as.integer(nboot)

  if (!is.numeric(report_every) || length(report_every) != 1L ||
      is.na(report_every) || !is.finite(report_every) ||
      report_every < 1 || report_every != floor(report_every)) {
    stop("`report_every` must be a single positive integer.", call. = FALSE)
  }

  report_every <- as.integer(report_every)

  if (!is.numeric(max_attempts) || length(max_attempts) != 1L ||
      is.na(max_attempts) || !is.finite(max_attempts) ||
      max_attempts < 0 || max_attempts != floor(max_attempts)) {
    stop("`max_attempts` must be a single non-negative integer.", call. = FALSE)
  }

  max_attempts <- as.integer(max_attempts)

  if (nboot >= 100L && max_attempts < nboot) {
    stop(
      "`max_attempts` must be greater than or equal to `nboot` when bootstrapping.",
      call. = FALSE
    )
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
        !is.finite(seed) || seed != floor(seed)) {
      stop("`seed` must be NULL or a single integer.", call. = FALSE)
    }
  }

  # -------------------------------------------------------------------------
  # Resolve input vectors
  # -------------------------------------------------------------------------

  if (!is.null(data)) {

    data <- as.data.frame(data)

    if (is.character(x) && is.character(y) && is.character(tr)) {

      if (!all(c(x, y, tr) %in% names(data))) {
        stop("`x`, `y`, and/or `tr` were not found in `data`.", call. = FALSE)
      }

      x_vec <- data[[x]]
      y_vec <- data[[y]]
      tr_vec <- data[[tr]]

    } else {

      warning(
        "`data` was supplied but `x`, `y`, and `tr` were not all character ",
        "column names. Using the supplied vectors directly.",
        call. = FALSE
      )

      x_vec <- x
      y_vec <- y
      tr_vec <- tr
    }

  } else {

    x_vec <- x
    y_vec <- y
    tr_vec <- tr
  }

  # -------------------------------------------------------------------------
  # Validate vectors
  # -------------------------------------------------------------------------

  if (!is.numeric(x_vec)) {
    stop("`x` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(y_vec)) {
    stop("`y` must be numeric.", call. = FALSE)
  }

  if (length(x_vec) != length(y_vec) || length(x_vec) != length(tr_vec)) {
    stop("`x`, `y`, and `tr` must have the same length.", call. = FALSE)
  }

  if (is.logical(tr_vec)) {
    tr_vec <- as.integer(tr_vec)
  }

  if (!is.numeric(tr_vec) && !is.integer(tr_vec)) {
    stop("`tr` must be binary and coded as 0/1 or TRUE/FALSE.", call. = FALSE)
  }

  if (!all(tr_vec %in% c(0, 1), na.rm = TRUE)) {
    stop("`tr` must be binary and coded as 0/1 or TRUE/FALSE.", call. = FALSE)
  }

  tmpdata <- data.frame(
    x = x_vec,
    y = y_vec,
    tr = tr_vec
  )

  complete_rows <- stats::complete.cases(tmpdata)
  n_removed <- sum(!complete_rows)

  if (n_removed > 0L) {
    warning(
      n_removed,
      " row(s) with missing values were removed.",
      call. = FALSE
    )
  }

  tmpdata <- tmpdata[complete_rows, , drop = FALSE]

  if (nrow(tmpdata) < 2L) {
    stop("At least two complete observations are required.", call. = FALSE)
  }

  if (length(unique(tmpdata$tr)) < 2L) {
    stop("`tr` must contain both 0 and 1 values after removing missing data.",
         call. = FALSE)
  }

  # -------------------------------------------------------------------------
  # Internal ROC MIC function
  # -------------------------------------------------------------------------

  compute_roc_mic <- function(d) {

    xoc <- d$y - d$x

    if (stats::sd(xoc) == 0) {
      stop("The change score `y - x` must have non-zero variance.",
           call. = FALSE)
    }

    rocobj <- pROC::roc(
      response = d$tr,
      predictor = xoc,
      quiet = TRUE
    )

    mic <- pROC::coords(
      rocobj,
      x = "best",
      input = "threshold",
      ret = "threshold",
      best.method = "youden",
      transpose = TRUE
    )

    mic <- as.numeric(mic)

    if (length(mic) > 1L) {
      mic <- mean(mic, na.rm = TRUE)
    }

    if (length(mic) != 1L || !is.finite(mic)) {
      stop("Could not obtain a finite ROC-based MIC.", call. = FALSE)
    }

    mic
  }

  mic_value <- compute_roc_mic(tmpdata)

  # -------------------------------------------------------------------------
  # Bootstrap
  # -------------------------------------------------------------------------

  mic_ci <- NULL
  mic_roc_ci <- NULL
  n_successful_boot <- 0L

  if (nboot >= 100L) {

    if (!is.null(seed)) {

      old_seed_exists <- exists(
        ".Random.seed",
        envir = .GlobalEnv,
        inherits = FALSE
      )

      if (old_seed_exists) {
        old_seed <- get(
          ".Random.seed",
          envir = .GlobalEnv,
          inherits = FALSE
        )
      }

      set.seed(seed)

      on.exit({
        if (old_seed_exists) {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      }, add = TRUE)
    }

    boot_values <- numeric(nboot)
    attempts <- 0L

    if (isTRUE(verbose)) {
      message("Starting bootstrap with nboot = ", nboot, ".")
    }

    while (n_successful_boot < nboot && attempts < max_attempts) {

      attempts <- attempts + 1L

      sample_indices <- sample.int(
        n = nrow(tmpdata),
        size = nrow(tmpdata),
        replace = TRUE
      )

      boot_data <- tmpdata[sample_indices, , drop = FALSE]

      if (length(unique(boot_data$tr)) < 2L) {
        next
      }

      if (stats::sd(boot_data$y - boot_data$x) == 0) {
        next
      }

      boot_mic <- try(
        compute_roc_mic(boot_data),
        silent = TRUE
      )

      if (inherits(boot_mic, "try-error")) {
        next
      }

      if (!is.finite(boot_mic)) {
        next
      }

      n_successful_boot <- n_successful_boot + 1L
      boot_values[n_successful_boot] <- boot_mic

      if (isTRUE(verbose) && n_successful_boot %% report_every == 0L) {
        message(
          "Successfully estimated ",
          n_successful_boot,
          " bootstrapped ROC MICs."
        )
      }
    }

    if (n_successful_boot < nboot) {

      warning(
        "Only ",
        n_successful_boot,
        " successful bootstrap samples were obtained after ",
        attempts,
        " attempts.",
        call. = FALSE
      )

      boot_values <- boot_values[seq_len(n_successful_boot)]

    } else {

      boot_values <- boot_values[seq_len(n_successful_boot)]
    }

    if (n_successful_boot > 0L) {

      qu <- unname(
        stats::quantile(
          boot_values,
          probs = c(0.025, 0.975),
          na.rm = TRUE
        )
      )

      mic_ci <- c(lower = qu[1L], upper = qu[2L])
      mic_roc_ci <- c(mic = mic_value, mic_ci)

    } else {

      mic_ci <- NULL
      mic_roc_ci <- NULL
    }

  } else if (nboot > 0L && nboot < 100L) {

    message(
      "Bootstrap CI not computed because `nboot < 100`. ",
      "Set `nboot >= 100` to request bootstrapping."
    )
  }

  # -------------------------------------------------------------------------
  # Output
  # -------------------------------------------------------------------------

  out <- list(
    mic_roc = as.numeric(mic_value),
    boot_CI = mic_ci,
    mic_ci = mic_roc_ci,
    nboot = nboot,
    n_successful_boot = n_successful_boot
  )

  class(out) <- "mic_roc"

  out
}




#' @export
print.mic_roc <- function(x, digits = 3, ...) {

  cat("ROC-based MIC estimation\n")
  cat("------------------------\n")
  cat("MIC ROC:", formatC(x$mic_roc, format = "f", digits = digits), "\n")

  if (!is.null(x$mic_ci)) {
    cat(
      "Bootstrap 95% CI:",
      formatC(x$mic_ci["lower"], format = "f", digits = digits),
      "to",
      formatC(x$mic_ci["upper"], format = "f", digits = digits),
      "\n"
    )
  }

  if (!is.null(x$nboot) && x$nboot >= 100L) {
    cat("Successful bootstrap estimates:", x$n_successful_boot, "\n")
  }

  invisible(x)
}


