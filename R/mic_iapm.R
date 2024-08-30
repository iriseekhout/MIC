#' Predictive modeling-based, adjusted predictive modeling-based, and "improved" adjusted predictive modeling-based MICs and their attendant bootstrapped 95% CIs
#' This function could also be used to estimate the interpretation threshold of a predictor
#'
#' Based on methods developed by Terluin et al (2015), Terluin et al (2017), and Terluin et al (2022).
#'
#' @param mypred Character vector; name of the column containing the change score or predictor score.
#' @param anchor Character vector; name of the column containing the transition rating (perceived change).
#' @param mydata Data.frame with the change score (or predictor score) and the transition rating in separate columns.
#' @param tr_rel Reliability of the transition rating estimated using \code{tr_reliability}.
#' @param nboot Integer; number of bootstrapped samples when estimating the 95% CI.  `nboot` must  > 100 to trigger the bootstrapping process.
#' @param report_every Integer. The interval at which the counter should be printed.
#' @param verbose Logical. If TRUE, the counter is printed every `reoprt_every` iterations. If FALSE, no counter is printed.
#'
#' @return A list of Predictive modeling-based MIC estimates
#' @export
#' @seealso
#' Terluin B, Eekhout I, Terwee CB, de Vet HCW. Minimal important change (MIC) based on a predictive modeling approach was more precise than MIC based on ROC analysis. J Clin Epidemiol. 2015;68(12):1388-1396. doi:10.1016/j.jclinepi.2015.03.015
#'
#' Terluin B, Eekhout I, Terwee CB. The anchor-based minimal important change, based on receiver operating characteristic analysis or predictive modeling, may need to be adjusted for the proportion of improved patients. J Clin Epidemiol. 2017;83:90-100. doi:10.1016/j.jclinepi.2016.12.015
#'
#' Terluin B, Eekhout I, Terwee CB. Improved adjusted minimal important change took reliability of transition ratings into account. J Clin Epidemiol. 2022;148:48-53. doi:10.1016/j.jclinepi.2022.04.018
#'
#'
#' @examples
#'
#'
mic_iapm <- function(
    mypred,
    anchor,
    mydata,
    tr_rel,
    nboot = 0 ,
    report_every = 100,
    verbose=FALSE) {


  #' drop additional classes (e.g. tbl_df and data.table
  mydata <- as.data.frame(mydata)


  if(!is.null(mydata) & is.character(mypred) & is.character(anchor)){
    if(!all(c(mypred, anchor) %in% colnames(mydata))){stop("anchor and/or mypred were not found in the data.")}
    mypred      <- mydata[, mypred]
    anchor      <- mydata[, anchor]
  }

  #' progress indicator
  if(verbose) sprintf("working on %s and %s", mypred, anchor) %>% print

  tmpdata <- data.frame(anchor = anchor , mypred = mypred)
  mylrm   <- glm(anchor ~ mypred, data = tmpdata, family = "binomial")
  C <- as.vector(coef(mylrm)[1])      # intercept coefficient C
  B <- as.vector(coef(mylrm)[2])      # regression coefficient B
  q <- mean(anchor)                   # proportion improved (perceived)
  p <- log(q/(1-q))                   # p = logodds(pre)


  #' prevalence adjustment
  cor       <- cor(anchor, mypred)
  sdchange  <- sd(mypred)

  #' MICs
  mic_pm <- (p-C)/B                                                                     # PM-based MIC
  mic_apm <- mic_pm - ((0.09 + 0.103 * cor) * sdchange * p)                             # APM-based MIC
  if(!missing(tr_rel)) mic_iapm <- mic_pm - ((0.8/tr_rel - 0.5) * sdchange * cor * p)   # iAPM-based MIC


  #' bootstrapped CIs for APM or iAPM
  #' bootstrapping initiated when nboot > 100

  if(nboot > 100)  {
    apm.ci <- numeric(nboot)
    success_count <- 0

    while (success_count < nboot) {
      sample_indices <- sample(1:nrow(tmpdata), replace = TRUE)
      boot_data <- tmpdata[sample_indices, ]
      boot_mylrm   <- try(glm(anchor ~ mypred, data = boot_data, family = "binomial"), silent = TRUE)
      if(any(class(boot_mylrm) == "try-error")) next

      C <- as.vector(coef(boot_mylrm)[1])      # intercept coefficient C
      B <- as.vector(coef(boot_mylrm)[2])      # regression coefficient B
      q <- mean(boot_data$anchor)              # proportion improved (perceived)
      p <- log(q/(1-q))                        # p = logodds(pre)
      cor       <- cor(boot_data$anchor, boot_data$mypred)
      sdchange  <- sd(boot_data$mypred)
      mic_pm    <- (p-C)/B                                                      # PM-based MIC
      mic_adj   <- mic_pm - ((0.09 + 0.103 * cor) * sdchange * p)               # APM-based MIC
      if(!missing(tr_rel))
        mic_adj <- mic_pm - ((0.8/tr_rel - 0.5) * sdchange * cor * p)      # iAPM-based MIC
      apm.ci[success_count + 1] <-  mic_adj
      success_count <- success_count + 1
      if (success_count %% report_every == 0) {
        message("Successfully simulated ", success_count, " bootstrapped MICs")
      }
    }

    cl <- function(x) {
      qu <- unname(quantile(x, c(0.025, 0.975)))
      c(lower=qu[1], upper=qu[2])
    }

    mic_ci   <- cl(apm.ci)
    if(!missing(nboot)) if(!missing(tr_rel)) {
      mic_adj_ci <- c(mic = mic_iapm, mic_ci)} else
        mic_adj_ci <- c(mic = mic_apm, mic_ci)
  }

  structure(list(
    mic_pm    = mic_pm,
    mic_apm   = mic_apm,
    mic_iapm  = if(!missing(tr_rel)) mic_iapm,
    boot_CI   = if(nboot>100)  mic_ci,
    mic_ci    = if(nboot>100)  mic_adj_ci
  ))
}
