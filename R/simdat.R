#' Simulate Longitudinal PROM Data and a Binary Anchor
#'
#' `simdat()` simulates Time 1 and Time 2 item responses for a 10-item PROM
#' and a binary anchor / transition rating from an item response theory context.
#' Each item has four ordered response categories scored 0, 1, 2, and 3, so
#' the total PROM score ranges from 0 to 30 at each time point.
#'
#' The returned data frame includes item-level responses, the binary anchor
#' `trat`, the Time 1 summed PROM score `score_t1`, and the Time 2 summed PROM
#' score `score_t2`. If `add_change = TRUE`, the observed change score
#' `change = score_t2 - score_t1` is also added.
#'
#' The R code is adapted from supplementary materials of Terluin et al.
#' Qual Life Res. 2024;33:963-973.
#'
#' @param N Integer. Sample size for simulation.
#' @param mn_imic Numeric. Mean individual MIC on the latent theta-change scale.
#'   For context, `mn_imic = 0.5` corresponds approximately to a raw-score MIC
#'   of about 2.8 points, while `mn_imic = 0.37425` corresponds approximately
#'   to a raw-score MIC of about 2.5 points on the 0-30 PROM scale.
#' @param sd_imic Numeric. Standard deviation of individual MICs on the latent
#'   theta-change scale.
#' @param cor_t1_change Numeric. Correlation between baseline theta and latent
#'   change.
#' @param mean_tetch Numeric. Mean latent change.
#' @param sd_tetch Numeric. Standard deviation of latent change.
#' @param rel_trt Numeric. Target reliability of perceived change used to
#'   generate the binary anchor / transition rating.
#' @param seed Optional integer. Random seed used to make the simulated data
#'   reproducible. If `NULL`, the current random-number generator state is used.
#' @param return_latent Logical. If `TRUE`, returns latent variables and item
#'   parameters in the output object.
#' @param add_change Logical. If `TRUE`, adds `change = score_t2 - score_t1`
#'   to the returned data frame. Defaults to `FALSE`.
#'
#' @return A list containing:
#' \describe{
#'   \item{seed}{The random seed used.}
#'   \item{settings}{Simulation settings.}
#'   \item{item_names}{Names of Time 1 items, Time 2 items, and anchor.}
#'   \item{truth}{Truth / diagnostic quantities, including `target_rel_trt`
#'   and `observed_rel_trt`.}
#'   \item{datw}{The simulated wide-format data frame.}
#' }
#'
#' If `return_latent = TRUE`, the output also includes item parameters,
#' latent variables, perceived change, and individual MICs.
#'
#' @examples
#' sim <- simdat(N = 200, seed = 123, add_change = TRUE)
#'
#' names(sim$datw)
#' sim$truth
#'
#' @export
simdat <- function(
    N = 2000,
    mn_imic = 0.37425,
    sd_imic = 0.05,
    cor_t1_change = -0.5,
    mean_tetch = 0.3,
    sd_tetch = 1.0,
    rel_trt = 0.7,
    seed = 1234,
    return_latent = TRUE,
    add_change = FALSE
) {

  # -------------------------------------------------------------------------
  # Reproducibility
  # -------------------------------------------------------------------------

  if (!is.null(seed)) {

    if (!is.numeric(seed) ||
        length(seed) != 1L ||
        is.na(seed) ||
        !is.finite(seed) ||
        seed != floor(seed)) {
      stop("`seed` must be NULL or a single integer.", call. = FALSE)
    }

    old_seed_exists <- exists(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )

    if (old_seed_exists) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv)
    }

    set.seed(as.integer(seed))

    on.exit({
      if (old_seed_exists) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
  }

  # -------------------------------------------------------------------------
  # Argument checks
  # -------------------------------------------------------------------------

  if (!is.numeric(N) ||
      length(N) != 1L ||
      is.na(N) ||
      !is.finite(N) ||
      N < 1L ||
      N != floor(N)) {
    stop("`N` must be a positive integer.", call. = FALSE)
  }

  N <- as.integer(N)

  if (!is.numeric(mn_imic) ||
      length(mn_imic) != 1L ||
      is.na(mn_imic) ||
      !is.finite(mn_imic)) {
    stop("`mn_imic` must be a single finite numeric value.", call. = FALSE)
  }

  if (!is.numeric(sd_imic) ||
      length(sd_imic) != 1L ||
      is.na(sd_imic) ||
      !is.finite(sd_imic) ||
      sd_imic < 0) {
    stop("`sd_imic` must be a single non-negative finite numeric value.",
         call. = FALSE)
  }

  if (!is.numeric(cor_t1_change) ||
      length(cor_t1_change) != 1L ||
      is.na(cor_t1_change) ||
      !is.finite(cor_t1_change) ||
      cor_t1_change <= -1 ||
      cor_t1_change >= 1) {
    stop("`cor_t1_change` must be a single finite value between -1 and 1.",
         call. = FALSE)
  }

  if (!is.numeric(mean_tetch) ||
      length(mean_tetch) != 1L ||
      is.na(mean_tetch) ||
      !is.finite(mean_tetch)) {
    stop("`mean_tetch` must be a single finite numeric value.", call. = FALSE)
  }

  if (!is.numeric(sd_tetch) ||
      length(sd_tetch) != 1L ||
      is.na(sd_tetch) ||
      !is.finite(sd_tetch) ||
      sd_tetch <= 0) {
    stop("`sd_tetch` must be a single positive finite numeric value.",
         call. = FALSE)
  }

  if (!is.numeric(rel_trt) ||
      length(rel_trt) != 1L ||
      is.na(rel_trt) ||
      !is.finite(rel_trt) ||
      rel_trt <= 0 ||
      rel_trt > 1) {
    stop("`rel_trt` must be > 0 and <= 1.", call. = FALSE)
  }

  if (!is.logical(return_latent) ||
      length(return_latent) != 1L ||
      is.na(return_latent)) {
    stop("`return_latent` must be either TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(add_change) ||
      length(add_change) != 1L ||
      is.na(add_change)) {
    stop("`add_change` must be either TRUE or FALSE.", call. = FALSE)
  }

  # -------------------------------------------------------------------------
  # Item parameters
  # -------------------------------------------------------------------------

  b2 <- c(-0.8, -0.8, -0.4, -0.4, 0, 0, 0.4, 0.4, 0.8, 0.8)
  bc <- b2 / 4

  b1 <- b2 - 1 + sample(bc)
  b3 <- b2 + 1 + sample(bc)
  a1 <- sample(1.7 + b2 / 2)

  cf_simb <- as.data.frame(
    data.frame(
      a1 = a1,
      b1 = b1,
      b2 = b2,
      b3 = b3
    )
  )

  # Transform b-parameters to d-parameters for mirt.
  # difficulty b = easiness d / -a
  cf_sim <- transform(
    cf_simb,
    b1 = -b1 * a1,
    b2 = -b2 * a1,
    b3 = -b3 * a1
  )

  colnames(cf_sim) <- c("a1", "d1", "d2", "d3")

  a_mat <- as.matrix(cf_sim[, "a1", drop = FALSE])
  d_mat <- as.matrix(cf_sim[, c("d1", "d2", "d3")])

  # -------------------------------------------------------------------------
  # Baseline theta and latent change
  # -------------------------------------------------------------------------

  Sigma <- matrix(
    c(
      1, cor_t1_change,
      cor_t1_change, 1
    ),
    nrow = 2,
    ncol = 2
  )

  tets <- MASS::mvrnorm(
    n = N,
    mu = rep(0, 2),
    Sigma = Sigma
  )

  theta_t1 <- tets[, 1]
  theta_t1 <- theta_t1 / stats::sd(theta_t1)
  theta_t1 <- theta_t1 - mean(theta_t1)
  theta_t1 <- as.matrix(theta_t1)

  theta_change <- tets[, 2]
  theta_change <- theta_change - mean(theta_change)
  theta_change <- theta_change / stats::sd(theta_change)
  theta_change <- theta_change * sd_tetch
  theta_change <- theta_change + mean_tetch

  theta_t2 <- as.matrix(theta_t1 + theta_change)

  # -------------------------------------------------------------------------
  # Item responses
  # -------------------------------------------------------------------------

  dat1 <- mirt::simdata(
    a = a_mat,
    d = d_mat,
    N = N,
    itemtype = "graded",
    Theta = theta_t1
  )

  dat2 <- mirt::simdata(
    a = a_mat,
    d = d_mat,
    N = N,
    itemtype = "graded",
    Theta = theta_t2
  )

  dat1 <- as.data.frame(dat1)
  dat2 <- as.data.frame(dat2)

  score_t1 <- rowSums(dat1)
  score_t2 <- rowSums(dat2)

  # -------------------------------------------------------------------------
  # Anchor / transition rating
  # -------------------------------------------------------------------------

  sd_change_error <- sqrt(
    ((1 - rel_trt) / rel_trt) *
      stats::sd(theta_change)^2
  )

  theta_change_error <- stats::rnorm(N, 0, sd_change_error)
  perceived_change <- theta_change + theta_change_error

  individual_mic <- stats::rnorm(N, mn_imic, sd_imic)

  trat <- numeric(N)
  trat[perceived_change > individual_mic] <- 1

  observed_rel_trt <- stats::var(theta_change) / stats::var(perceived_change)

  # -------------------------------------------------------------------------
  # Assemble data
  # -------------------------------------------------------------------------

  datw <- data.frame(dat1, dat2, trat)

  nitems <- 10L

  t1_items <- paste0("item", seq_len(nitems))
  t2_items <- paste0("item", seq_len(nitems), ".1")

  names(datw)[seq_len(nitems)] <- t1_items
  names(datw)[nitems + seq_len(nitems)] <- t2_items
  names(datw)[2L * nitems + 1L] <- "trat"

  datw$score_t1 <- score_t1
  datw$score_t2 <- score_t2

  if (isTRUE(add_change)) {
    datw$change <- datw$score_t2 - datw$score_t1
  }

  # -------------------------------------------------------------------------
  # Verification quantities
  # -------------------------------------------------------------------------

  raw_change <- score_t2 - score_t1

  truth <- list(
    target_rel_trt = rel_trt,
    observed_rel_trt = as.numeric(observed_rel_trt),
    latent_mic = mn_imic,
    mean_individual_mic = mean(individual_mic),
    sd_individual_mic = stats::sd(individual_mic),
    mean_theta_change = mean(theta_change),
    sd_theta_change = stats::sd(theta_change),
    prop_improved = mean(trat == 1),
    cor_change_anchor = stats::cor(raw_change, trat),
    cor_theta_t1_theta_t2 = stats::cor(
      as.vector(theta_t1),
      as.vector(theta_t2)
    ),
    cor_theta_change_anchor = stats::cor(theta_change, trat),
    raw_score_range = c(0, 30)
  )

  out <- list(
    seed = seed,
    settings = list(
      N = N,
      mn_imic = mn_imic,
      sd_imic = sd_imic,
      cor_t1_change = cor_t1_change,
      mean_tetch = mean_tetch,
      sd_tetch = sd_tetch,
      rel_trt = rel_trt,
      add_change = add_change
    ),
    item_names = list(
      t1_items = t1_items,
      t2_items = t2_items,
      anchor = "trat"
    ),
    truth = truth,
    datw = datw
  )

  if (isTRUE(return_latent)) {
    out$item_parameters_b <- cf_simb
    out$item_parameters_d <- cf_sim
    out$theta_t1 <- theta_t1
    out$theta_t2 <- theta_t2
    out$theta_change <- theta_change
    out$perceived_change <- perceived_change
    out$individual_mic <- individual_mic
  }

  out
}
