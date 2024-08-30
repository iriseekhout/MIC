#' Simulates Time 1 and Time 2 PROM items and a transition rating vector from an item response theory context.
#'
#' R codes taken from supplementary materials of Terluin et al Qual Life Res. 2024;33:963–73
#'
#'
#' @param N sample size for simulation
#' @param par.mn.imic Numeric vector; mean individual MICs. When 0.37425, corresponds to MIC 2.5 points on the raw scale.
#' @param par.sd.imic Numeric vector; sd of individual MICs. Cannot be too large lest MIC is negative
#' @param rt1ch Numeric vector; correlation between tet1s and latent change
#' @param mean.tetchs Numeric vector; mean latent change
#' @param par.sd.tetchs Numeric vector; standard deviation of `tetchs`
#' @param par.rel.trt Numeric vector; transition rating reliability
#'
#' @return a list of estimates and a dataframe with Time 1 and Time 2 PROM items and a transition rating vector (`trat`)
#' @export
#' @seealso Terluin B, Trigg A, Fromy P, Schuller W, Terwee CB, Bjorner JB. Estimating anchor-based minimal
#' important change using longitudinal confirmatory factor analysis. Qual Life Res. 2024;33:963–73.
#' @examples


simdat <- function(N = 2000,
                   par.mn.imic = 0.37425,
                   par.sd.imic = 0.05,
                   rt1ch = -0.5,
                   mean.tetchs = 0.3,
                   par.sd.tetchs = 1.0,
                   par.rel.trt  = 0.7

){

  b2  <- c(-0.8, -0.8, -0.4, -0.4, 0, 0, 0.4, 0.4, 0.8, 0.8)
  bc <- b2/4
  b1 <- b2 - 1 + sample(bc)
  b3 <- b2 + 1 + sample(bc)
  a1 <- sample( 1.7+b2/2 )

  cf.simb <- as.matrix( data.frame(a1,b1,b2,b3) )
  round(cf.simb, 3)
  round(colMeans(cf.simb), 3)

  cf.simb <- as.data.frame(cf.simb)

  # Transform b-parameters to d-parameters ('mirt' works with d-parameters)
  # difficulty (b) = easiness (d) / -a

  cf.sim <-  transform(cf.simb,
                       b1 = -b1 * a1,
                       b2 = -b2 * a1,
                       b3 = -b3 * a1)
  colnames(cf.sim) <- c("a1","d1","d2","d3")

  a1 <- as.matrix(cf.sim[ , 1])
  d1 <- as.matrix(cf.sim[ , -1])


  #' Simulate baseline data-set and change (theta) scores
  par.mn.tet1s <- 0                         # mean parameter of tet1s
  par.sd.tet1s <- 1                         # sd   parameter of tet1s
  Sigma <- matrix(c(1, rt1ch, rt1ch, 1), 2 , 2)
  tets <- MASS::mvrnorm(N, rep(0, 2), Sigma) # simulate 2 variables with correlation -0.5

  tet1s <- tets[,1] * par.sd.tet1s /sd(tets[,1])    # adjust SD
  tet1s <- tet1s-mean(tet1s) + par.mn.tet1s         # adjust mean
  tet1s <- as.matrix(tet1s)
  dat1 <- simdata(a1, d1, N, itemtype="graded", Theta=tet1s)
  dat1 <- as.data.frame(dat1)

  xo1 <- rowSums(dat1)   # This is the baseline PROM score


  ### Create theta change
  tetchs <- tets[,2]

  tetchs <- tetchs - mean(tetchs)                     # make mean = 0
  tetchs <- (tetchs/sd(tetchs))* par.sd.tetchs        # make SD of tetchs = par.sd.tetchs
  tetchs <- tetchs + mean.tetchs
  tet2s  <- as.matrix(tet1s + tetchs)                 # theta T2
  dat2 <- simdata(a1, d1, N, itemtype="graded", Theta=tet2s)
  dat2 <- as.data.frame(dat2)
  xo2 <- rowSums(dat2)    # This is the follow-up PROM score

  ### Create observed PROM change score
  xoc <- xo2 - xo1


  ### create perceived change

  # Add ERROR to the latent change
  rel.trt <- par.rel.trt    # reliability of the TRT
  sd.ch.error <- sqrt(((1-rel.trt)/rel.trt)*sd(tetchs)^2)
  tetch.error <- rnorm(N, 0, sd.ch.error)
  tetch.prc <- tetchs + tetch.error


  imic <- rnorm(N, par.mn.imic, par.sd.imic)
  trt <- numeric(N)
  trt[tetch.prc > imic] <- 1



  datw <- data.frame(dat1,dat2,trt)
  nitems <- 0.5*(ncol(datw)-1)
  item_names <- paste0("Item_", c(c(1:nitems), paste0(c(1:nitems), c(".1"))))
  names(datw) <- c(item_names, "trat")

  datw$xoc <- xoc






  #' generate output
  structure(list(
    corr_tet1s_tet2s    = cor(tet1s, tet2s),
    rel_prc             = var(tetchs)/var(tetch.prc),
    prop_improve        = length(trt[trt>0])/length(trt),
    cor_xoc_trt         = cor(xoc,trt) ,
    datw                = datw
  )

  )



}
