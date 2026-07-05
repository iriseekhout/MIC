#' Estimate Predictive Modeling-Based MICs
#'
#' Estimates predictive modeling-based, adjusted predictive modeling-based, and
#' improved adjusted predictive modeling-based MICs, with optional bootstrap
#' confidence intervals. This function can also be used to estimate the
#' interpretation threshold of a predictor.
#'
#' Based on methods developed by Terluin et al. (2015), Terluin et al. (2017),
#' and Terluin et al. (2022).
#'
#' @param mypred Character string; name of the column containing the change
#'   score or predictor score.
#' @param anchor Character string; name of the column containing the binary
#'   anchor. The anchor must be binary and coded as 0/1 or TRUE/FALSE.
#' @param mydata Data frame with the change score or predictor score and the
#'   anchor in separate columns.
#' @param anchor_reliability Optional anchor reliability. Can be either a single
#'   numeric value between 0 and 1, or an object returned by `tr_reliability()`.
#'   If supplied, the improved adjusted predictive modeling-based MIC is also
#'   calculated.
#' @param nboot Integer; number of bootstrap samples for estimating the 95%
#'   confidence interval. Bootstrapping is performed only when `nboot >= 100`.
#' @param report_every Integer. The interval at which the bootstrap progress
#'   counter should be printed.
#' @param verbose Logical. If `TRUE`, progress messages are printed.
#' @param seed Optional integer seed for reproducible bootstrap confidence
#'   intervals.
#' @param max_attempts Integer; maximum number of bootstrap attempts. This avoids
#'   an infinite loop when many bootstrap samples fail.
#'
#' @return A list containing predictive modeling-based MIC estimates and,
#'   optionally, bootstrap confidence intervals.
#'
#' @references
#' Terluin B, Eekhout I, Terwee CB, de Vet HCW. Minimal important change
#' (MIC) based on a predictive modeling approach was more precise than MIC
#' based on ROC analysis. J Clin Epidemiol. 2015;68(12):1388-1396.
#' doi:10.1016/j.jclinepi.2015.03.015
#'
#' Terluin B, Eekhout I, Terwee CB. The anchor-based minimal important change,
#' based on receiver operating characteristic analysis or predictive modeling,
#' may need to be adjusted for the proportion of improved patients.
#' J Clin Epidemiol. 2017;83:90-100.
#' doi:10.1016/j.jclinepi.2016.12.015
#'
#' Terluin B, Eekhout I, Terwee CB. Improved adjusted minimal important change
#' took reliability of transition ratings into account. J Clin Epidemiol.
#' 2022;148:48-53. doi:10.1016/j.jclinepi.2022.04.018
#'
#' @seealso [tr_reliability()] for estimating anchor reliability using CFA.
#'
#' @export
mic_iapm <- function(
    mypred,
    anchor,
    mydata,
    anchor_reliability = NULL,
    nboot = 0,
    report_every = 100,
    verbose = FALSE,
    seed = NULL,
    max_attempts = nboot * 5
) {

  mic_ci <- NULL
  mic_adj_ci <- NULL
  n_successful_boot <- 0

  # -------------------------------------------------------------------------
  # Validate `mydata`
  # -------------------------------------------------------------------------

  if (missing(mydata) || is.null(mydata)) {
    stop("`mydata` must be supplied.", call. = FALSE)
  }

  mydata <- as.data.frame(mydata)

  # -------------------------------------------------------------------------
  # Validate column names
  # -------------------------------------------------------------------------

  if (!is.character(mypred) || length(mypred) != 1) {
    stop(
      "`mypred` must be a single character string naming a column in `mydata`.",
      call. = FALSE
    )
  }

  if (!is.character(anchor) || length(anchor) != 1) {
    stop(
      "`anchor` must be a single character string naming a column in `mydata`.",
      call. = FALSE
    )
  }

  if (!all(c(mypred, anchor) %in% colnames(mydata))) {
    stop(
      "`anchor` and/or `mypred` were not found in `mydata`.",
      call. = FALSE
    )
  }

  mypred_vec <- mydata[[mypred]]
  anchor_vec <- mydata[[anchor]]

  # -------------------------------------------------------------------------
  # Validate general arguments
  # -------------------------------------------------------------------------

  if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
    stop("`verbose` must be either TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(nboot) || length(nboot) != 1 || is.na(nboot) ||
      !is.finite(nboot) || nboot < 0 || nboot != floor(nboot)) {
    stop("`nboot` must be a single non-negative integer.", call. = FALSE)
  }

  if (!is.numeric(report_every) || length(report_every) != 1 ||
      is.na(report_every) || !is.finite(report_every) ||
      report_every < 1 || report_every != floor(report_every)) {
    stop("`report_every` must be a single positive integer.", call. = FALSE)
  }

  if (!is.numeric(max_attempts) || length(max_attempts) != 1 ||
      is.na(max_attempts) || !is.finite(max_attempts) ||
      max_attempts < 0 || max_attempts != floor(max_attempts)) {
    stop(
      "`max_attempts` must be a single non-negative integer.",
      call. = FALSE
    )
  }

  if (nboot >= 100 && max_attempts < nboot) {
    stop(
      "`max_attempts` must be greater than or equal to `nboot` when bootstrapping.",
      call. = FALSE
    )
  }

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1 || is.na(seed) ||
        !is.finite(seed) || seed != floor(seed)) {
      stop("`seed` must be NULL or a single integer.", call. = FALSE)
    }
  }

  # -------------------------------------------------------------------------
  # Anchor reliability
  # -------------------------------------------------------------------------

  rel_anchor_value <- get_anchor_reliability(anchor_reliability)

  if (!is.null(rel_anchor_value)) {

    if (!is.numeric(rel_anchor_value) ||
        length(rel_anchor_value) != 1 ||
        is.na(rel_anchor_value) ||
        !is.finite(rel_anchor_value)) {
      stop(
        "`anchor_reliability` must resolve to a single finite numeric value.",
        call. = FALSE
      )
    }

    if (rel_anchor_value <= 0 || rel_anchor_value > 1) {
      stop(
        "`anchor_reliability` must be greater than 0 and less than or equal to 1.",
        call. = FALSE
      )
    }
  }

  # -------------------------------------------------------------------------
  # Validate predictor and anchor
  # -------------------------------------------------------------------------

  if (!is.numeric(mypred_vec)) {
    stop("`mypred` must refer to a numeric column.", call. = FALSE)
  }

  if (is.logical(anchor_vec)) {
    anchor_vec <- as.integer(anchor_vec)
  }

  if (!is.numeric(anchor_vec) && !is.integer(anchor_vec)) {
    stop(
      "`anchor` must refer to a binary column coded as 0/1 or TRUE/FALSE.",
      call. = FALSE
    )
  }

  if (!all(anchor_vec %in% c(0, 1), na.rm = TRUE)) {
    stop(
      "`anchor` must be binary and coded as 0/1 or TRUE/FALSE.",
      call. = FALSE
    )
  }

  tmpdata <- data.frame(
    anchor = anchor_vec,
    mypred = mypred_vec
  )

  # -------------------------------------------------------------------------
  # Missing data
  # -------------------------------------------------------------------------

  complete_rows <- stats::complete.cases(tmpdata)
  n_removed <- sum(!complete_rows)

  if (n_removed > 0) {
    warning(
      n_removed,
      " row(s) with missing values were removed.",
      call. = FALSE
    )
  }

  tmpdata <- tmpdata[complete_rows, , drop = FALSE]

  if (nrow(tmpdata) < 2) {
    stop("At least two complete observations are required.", call. = FALSE)
  }

  if (length(unique(tmpdata$anchor)) < 2) {
    stop(
      "`anchor` must contain both 0 and 1 values after removing missing data.",
      call. = FALSE
    )
  }

  if (stats::sd(tmpdata$mypred) == 0) {
    stop("`mypred` must have non-zero variance.", call. = FALSE)
  }

  if (verbose) {
    message("Working on predictor `", mypred, "` and anchor `", anchor, "`.")
  }

  # -------------------------------------------------------------------------
  # MIC computation
  # -------------------------------------------------------------------------

  compute_mic <- function(data, rel_anchor_value = NULL) {

    mylrm <- stats::glm(
      anchor ~ mypred,
      data = data,
      family = stats::binomial()
    )

    coefs <- stats::coef(mylrm)

    if (any(!is.finite(coefs))) {
      stop(
        "Logistic regression produced non-finite coefficients.",
        call. = FALSE
      )
    }

    C <- as.vector(coefs[1])
    B <- as.vector(coefs[2])

    if (!is.finite(B) || abs(B) < .Machine$double.eps) {
      stop(
        "The logistic regression slope is zero or non-finite.",
        call. = FALSE
      )
    }

    q <- mean(data$anchor)

    if (!is.finite(q) || q <= 0 || q >= 1) {
      stop(
        "The proportion of improved patients must be strictly between 0 and 1.",
        call. = FALSE
      )
    }

    p <- log(q / (1 - q))

    r_anchor_pred <- stats::cor(data$anchor, data$mypred)

    if (!is.finite(r_anchor_pred)) {
      stop(
        "Correlation between `anchor` and `mypred` is non-finite.",
        call. = FALSE
      )
    }

    sdchange <- stats::sd(data$mypred)

    if (!is.finite(sdchange) || sdchange <= 0) {
      stop(
        "Standard deviation of `mypred` must be positive and finite.",
        call. = FALSE
      )
    }

    mic_pm <- (p - C) / B

    mic_apm <- mic_pm -
      ((0.09 + 0.103 * r_anchor_pred) * sdchange * p)

    mic_iapm_value <- NULL

    if (!is.null(rel_anchor_value)) {
      mic_iapm_value <- mic_pm -
        ((0.8 / rel_anchor_value - 0.5) *
           sdchange *
           r_anchor_pred *
           p)
    }

    list(
      mic_pm = mic_pm,
      mic_apm = mic_apm,
      mic_iapm = mic_iapm_value
    )
  }

  estimate <- compute_mic(
    tmpdata,
    rel_anchor_value = rel_anchor_value
  )

  mic_pm <- estimate$mic_pm
  mic_apm <- estimate$mic_apm
  mic_iapm_value <- estimate$mic_iapm

  # -------------------------------------------------------------------------
  # Bootstrap CIs
  # -------------------------------------------------------------------------

  if (nboot >= 100) {

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

    apm_ci <- numeric(nboot)
    attempts <- 0

    while (n_successful_boot < nboot && attempts < max_attempts) {

      attempts <- attempts + 1

      sample_indices <- sample(
        seq_len(nrow(tmpdata)),
        replace = TRUE
      )

      boot_data <- tmpdata[sample_indices, , drop = FALSE]

      if (length(unique(boot_data$anchor)) < 2) {
        next
      }

      if (stats::sd(boot_data$mypred) == 0) {
        next
      }

      boot_estimate <- try(
        compute_mic(
          boot_data,
          rel_anchor_value = rel_anchor_value
        ),
        silent = TRUE
      )

      if (inherits(boot_estimate, "try-error")) {
        next
      }

      if (!is.null(rel_anchor_value)) {
        mic_adj <- boot_estimate$mic_iapm
      } else {
        mic_adj <- boot_estimate$mic_apm
      }

      if (!is.finite(mic_adj)) {
        next
      }

      n_successful_boot <- n_successful_boot + 1
      apm_ci[n_successful_boot] <- mic_adj

      if (verbose && n_successful_boot %% report_every == 0) {
        message(
          "Successfully simulated ",
          n_successful_boot,
          " bootstrapped MICs."
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

      apm_ci <- apm_ci[seq_len(n_successful_boot)]
    }

    if (n_successful_boot > 0) {

      cl <- function(x) {
        qu <- unname(
          stats::quantile(
            x,
            c(0.025, 0.975),
            na.rm = TRUE
          )
        )
        c(lower = qu[1], upper = qu[2])
      }

      mic_ci <- cl(apm_ci)

      if (!is.null(rel_anchor_value)) {
        mic_adj_ci <- c(mic = mic_iapm_value, mic_ci)
      } else {
        mic_adj_ci <- c(mic = mic_apm, mic_ci)
      }

    } else {

      mic_ci <- NULL
      mic_adj_ci <- NULL
    }
  }

  # -------------------------------------------------------------------------
  # Output
  # -------------------------------------------------------------------------

  out <- list(
    mic_pm = mic_pm,
    mic_apm = mic_apm,
    mic_iapm = mic_iapm_value,
    anchor_reliability = rel_anchor_value,
    boot_CI = mic_ci,
    mic_ci = mic_adj_ci,
    nboot = nboot,
    n_successful_boot = n_successful_boot
  )

  if (inherits(anchor_reliability, "tr_reliability")) {
    out$tr_reliability <- anchor_reliability
  }

  class(out) <- "mic_iapm"

  out
}
