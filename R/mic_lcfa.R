#' Estimates (i) present state bias in transition ratings and (ii) anchor-based minimal important change using longitudinal confirmatory factor analysis
#'
#' Based on methods developed by Terluin et al J Clin Epidemiol. 2022;143:128-136. and Qual Life Res. 2024;33:963–73
#'
#' @param mydat A data frame in which the variables are ordered to include Time 1 items, Time 2 items, and a single transition rating variable. User needs to ensure that each PROM item has the same number of levels at both time-points.
#' @param model Model specification based on `lavaan` model syntax
#' @param trt_cut Numeric vector; cutpoint to define a dichotomized transition rating variable
#' @param report_every Integer. The interval at which the counter should be printed.
#' @param B  Integer; number of bootstrapped samples. `B`>0 triggers the bootstrapping process.
#'
#' @return a list of estimates including the present state bias and the MIC with its bootstrapped 95% CI.
#' @export
#' @seealso Terluin B, Trigg A, Fromy P, Schuller W, Terwee CB, Bjorner JB. Estimating anchor-based minimal
#' important change using longitudinal confirmatory factor analysis. Qual Life Res. 2024;33:963–73.
#' Terluin B, Griffiths P, Trigg A, Terwee CB, Bjorner JB.
#' Present state bias in transition ratings was accurately estimated in simulated and real data. J Clin Epidemiol. 2022;143:128-136. doi:10.1016/j.jclinepi.2021.12.024
#'
#'
#' @examples

mic_lcfa  <- function (mydat,
                       model,
                       trt_cut = 1,
                       B = 0,
                       report_every = 100) {


  nitems <- 0.5*(ncol(mydat)-1)        # number of items per scale

  # dichotomize trt variable
  mydat[[ncol(mydat)]] <- ifelse(mydat[[ncol(mydat)]] >= trt_cut, 1, 0)

  fit_mic <- cfa(model,
                 data=mydat,
                 std.lv=T,
                 ordered=T,
                 parameterization="theta")

  fit_psb <- cfa(gsub("f1 == -f2", "", model),
                 data=mydat,
                 std.lv=T,
                 ordered=T,
                 parameterization="theta")

  pe <- parameterEstimates(fit_psb)
  psb_est <- pe$est[pe$label=="psb"]


  pe <- parameterEstimates(fit_mic, rsquare=T)
  MIC.theta <- pe$est[pe$label=="b_param"] ## difficulty parameter (MIC)


  ### MIRT, MIC IN TERMS OF PROM CHANGE SCORE
  #' single factor IRT model
  mod1 <- mirt(mydat[, 1:nitems], verbose = FALSE, itemtype='graded')

  N.ets = 5000

  # MIC.ets
  theta1 <- as.matrix( rnorm(N.ets, 0, 1) )
  theta2 <- theta1 + MIC.theta
  ( mean.ets1 <- mean(expected.test(mod1, theta1)) )
  ( mean.ets2 <- mean(expected.test(mod1, theta2)) )
  ( MIC.ets <- mean.ets2 - mean.ets1 )





  nboot <- 0
  boot  <- array(NA, c(B, 1),dimnames=list(NULL, c('MIC.ets_boot')))

  if(B > 0) {
    for(i in 1 : B) {

      j   <- sample(nrow(mydat), nrow(mydat), replace=TRUE)
      mydat_boot <- mydat[j, ]

     # fit cfa models
      mcfa1_boot <-try(
        cfa(model,
            data = mydat_boot,
            std.lv = T,
            ordered = T,
            parameterization="theta"),
        silent = TRUE)

      if (inherits(mcfa1_boot, "try-error"))  next


      pe_boot <- parameterEstimates(mcfa1_boot, rsquare=T)
      MIC.theta_boot <- pe_boot$est[pe_boot$label=="b_param"] ## difficulty parameter (MIC)
      #' single factor IRT model
      mod1_boot <- mirt(mydat_boot[, 1:nitems], itemtype='graded', verbose = FALSE)

      N.ets = 5000

      # MIC.ets
      theta1 <- as.matrix( rnorm(N.ets, 0, 1) )
      theta2 <- theta1 + MIC.theta_boot
      ( mean.ets1 <- mean(expected.test(mod1_boot, theta1)) )
      ( mean.ets2 <- mean(expected.test(mod1_boot, theta2)) )
      ( MIC.ets_boot <- mean.ets2 - mean.ets1 )



      nboot <- nboot + 1

      # progress counter
      if(nboot %% report_every == 0) message("Successfully simulated ", nboot, " MICs")
      flush.console()
      boot[nboot, ]         <-  as.matrix(MIC.ets_boot)
    }

    if(nboot < B) boot <- boot[1 : nboot,  , drop=FALSE]

  }
  # print(boot)
  cl <- function(x) {
    qu <- unname(quantile(x, c(0.025, 0.975)))
    c(Lower=qu[1], Upper=qu[2]) }
  bb     <- round (apply(boot, 2, function(x) cl(x) ) , 3)
  cistats <- t(bb)



  structure(
    list(
      psb       = psb_est,
      MIC.theta = MIC.theta,
      MIC.ets   = MIC.ets,
      MIC_CI    = cistats)
  )
}


