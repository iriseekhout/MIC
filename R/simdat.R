#' Simulates Time 1 and Time 2 PROM items and a transition rating vector
#'
#' Simulates Time 1 and Time 2 PROM item responses and a transition rating
#' vector from an item response theory context.
#'
#' R code adapted from supplementary materials of Terluin et al.
#' Qual Life Res. 2024;33:963–973.
#'
#' @param N Integer. Sample size for simulation.
#' @param par.mn.imic Numeric. Mean individual MIC. When 0.37425, corresponds
#'   approximately to MIC 2.5 points on the raw scale.
#' @param par.sd.imic Numeric. Standard deviation of individual MICs.
#' @param rt1ch Numeric. Correlation between baseline theta and latent change.
#' @param mean.tetchs Numeric. Mean latent change.
#' @param par.sd.tetchs Numeric. Standard deviation of latent change.
#' @param par.rel.trt Numeric. Transition rating reliability.
#' @param seed Optional integer. Random seed used to make the simulated data
#'   reproducible. If `NULL`, the current random-number generator state is used.
#'
#' @return A list of simulation summaries and a data frame with Time 1 items,
#'   Time 2 items, transition rating `trat`, and observed PROM change score
#'   `xoc`.
#'
#' @export
simdat <- function(
    N = 2000,
    par.mn.imic = 0.37425,
    par.sd.imic = 0.05,
    rt1ch = -0.5,
    mean.tetchs = 0.3,
    par.sd.tetchs = 1.0,
    par.rel.trt = 0.7,
    seed = 1234
) {

  # -------------------------------------------------------------------------
  # Optional reproducibility
  # -------------------------------------------------------------------------
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
      stop("`seed` must be a single non-missing numeric value.", call. = FALSE)
    }

    old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)

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
  # Item parameters
  # -------------------------------------------------------------------------
  b2 <- c(-0.8, -0.8, -0.4, -0.4, 0, 0, 0.4, 0.4, 0.8, 0.8)
  bc <- b2 / 4

  b1 <- b2 - 1 + sample(bc)
  b3 <- b2 + 1 + sample(bc)
  a1 <- sample(1.7 + b2 / 2)

  cf.simb <- as.matrix(data.frame(a1, b1, b2, b3))
  cf.simb <- as.data.frame(cf.simb)

  # Transform b-parameters to d-parameters.
  # difficulty b = easiness d / -a
  cf.sim <- transform(
    cf.simb,
    b1 = -b1 * a1,
    b2 = -b2 * a1,
    b3 = -b3 * a1
  )

  colnames(cf.sim) <- c("a1", "d1", "d2", "d3")

  a1 <- as.matrix(cf.sim[, 1])
  d1 <- as.matrix(cf.sim[, -1])

  # -------------------------------------------------------------------------
  # Baseline theta and latent change
  # -------------------------------------------------------------------------
  par.mn.tet1s <- 0
  par.sd.tet1s <- 1

  Sigma <- matrix(c(1, rt1ch, rt1ch, 1), 2, 2)

  tets <- MASS::mvrnorm(
    n = N,
    mu = rep(0, 2),
    Sigma = Sigma
  )

  tet1s <- tets[, 1] * par.sd.tet1s / stats::sd(tets[, 1])
  tet1s <- tet1s - mean(tet1s) + par.mn.tet1s
  tet1s <- as.matrix(tet1s)

  dat1 <- mirt::simdata(
    a = a1,
    d = d1,
    N = N,
    itemtype = "graded",
    Theta = tet1s
  )

  dat1 <- as.data.frame(dat1)
  xo1 <- rowSums(dat1)

  # -------------------------------------------------------------------------
  # Create theta change and Time 2 responses
  # -------------------------------------------------------------------------
  tetchs <- tets[, 2]

  tetchs <- tetchs - mean(tetchs)
  tetchs <- (tetchs / stats::sd(tetchs)) * par.sd.tetchs
  tetchs <- tetchs + mean.tetchs

  tet2s <- as.matrix(tet1s + tetchs)

  dat2 <- mirt::simdata(
    a = a1,
    d = d1,
    N = N,
    itemtype = "graded",
    Theta = tet2s
  )

  dat2 <- as.data.frame(dat2)
  xo2 <- rowSums(dat2)

  xoc <- xo2 - xo1

  # -------------------------------------------------------------------------
  # Transition rating
  # -------------------------------------------------------------------------
  rel.trt <- par.rel.trt

  sd.ch.error <- sqrt(((1 - rel.trt) / rel.trt) * stats::sd(tetchs)^2)

  tetch.error <- stats::rnorm(N, 0, sd.ch.error)
  tetch.prc <- tetchs + tetch.error

  imic <- stats::rnorm(N, par.mn.imic, par.sd.imic)

  trt <- numeric(N)
  trt[tetch.prc > imic] <- 1

  # -------------------------------------------------------------------------
  # Assemble output data
  # -------------------------------------------------------------------------
  datw <- data.frame(dat1, dat2, trt)

  nitems <- 0.5 * (ncol(datw) - 1)

  item_names <- paste0(
    "Item_",
    c(seq_len(nitems), paste0(seq_len(nitems), ".1"))
  )

  names(datw) <- c(item_names, "trat")

  datw$xoc <- xoc

  # -------------------------------------------------------------------------
  # Output
  # -------------------------------------------------------------------------
  structure(
    list(
      seed = seed,
      corr_tet1s_tet2s = stats::cor(tet1s, tet2s),
      rel_prc = stats::var(tetchs) / stats::var(tetch.prc),
      prop_improve = mean(trt > 0),
      cor_xoc_trt = stats::cor(xoc, trt),
      datw = datw
    )
  )
}

