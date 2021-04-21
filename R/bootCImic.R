#' Bootstrap confidence intervals for MIC
#'
#' @param data data.frame to bootstrap
#' @param b number of bootstrap samples
#' @param mic mic method to apply bootstraps for `c("roc", "predict", "adjust")`
#' @param x vector with score at time point 1, or column name in the data if
#' `!is.null(data)`
#' @param y vector with score at time point 2, or column name in the data if
#' `!is.null(data)`
#' @param tr vector with transition rates (perceived change), or column name in
#' the data if `!is.null(data)`
#' @param model only used for `mic = "adjust"` the model described using the
#' `lavaan` model syntax. see examples in [tr_reliability] or `?lavaan::model.syntax`
#' @importFrom rsample bootstraps int_pctl analysis
#' @importFrom dplyr %>% mutate
#' @importFrom purrr map map_dbl
#' @importFrom tidyr tibble
#' @importFrom rlang .data

#'
#' @return a tibble with columns .lower, .estimate, .upper, .alpha, .method, and
#' term. The term is the mic method.
#' @export
#'
#' @examples
#' nitems <- 10
#' example$x <- rowSums(example[,1:nitems])                 # sumscore T1
#' example$y <- rowSums(example[,(nitems+1):(2*nitems)])    # sumscore T2
#' model <- '
#' # Factors
#' F1 =~ a1*v1_1+a2*v1_2+a3*v1_3+a4*v1_4+a5*v1_5+a6*v1_6+a7*v1_7+a8*v1_8+
#'   a9*v1_9+a10*v1_10+trat
#' F2 =~ a1*v2_1+a2*v2_2+a3*v2_3+a4*v2_4+a5*v2_5+a6*v2_6+a7*v2_7+a8*v2_8+
#'   a9*v2_9+a10*v2_10+trat
#'   '
#' #start <- Sys.time()
#' #bootCImic(example, model = model)
#' #Sys.time()-start
#'
#' start <- Sys.time()
#' bootCImic(example, mic = c("roc", "pred"), x = "x",  y = "y", tr = "trat")
#' Sys.time()-start
#'
bootCImic <- function(data,
                      b = 1000,
                      mic = c("roc", "predict", "adjust"),
                      x,
                      y,
                      tr,
                      model = NULL){
  bootCI <- vector()
  #rsample bootstraps
  boots <- bootstraps(data, times = b, apparent = TRUE)


  boot_mic_roc <- function(split, x, y, tr){
    dat <- analysis(split)
    tibble(
      term = "mic_roc",
      estimate = mic_roc(data = dat, x = x, y = y, tr = tr),
      std.err = NA_real_)
  }
  boot_mic_pred <- function(split, x, y, tr){
    dat <- analysis(split)
    tibble(
      term = "mic_pred",
      estimate = mic_pred(data = dat, x = x, y = y, tr = tr),
      std.err = NA_real_)
  }

  boot_tr_rel <- function(split, model1 = model){
    dat <- analysis(split)
    tr_reliability(dat, model = model1, modification = FALSE)
  }

  boot_mic_adjust <- function(split, x, y, tr, reliability){
    dat <- analysis(split)
    tibble(
      term = "mic_adjust",
      estimate = mic_adjust(data = dat, x = x, y = y, tr = tr, reliability = reliability),
      std.err = NA_real_)
  }


 if("roc" %in% mic){
  boot_roc <-
    boots %>%
    mutate(mic_roc = map(splits, boot_mic_roc, x = x, y = y, tr = tr)) %>%
    int_pctl(. , mic_roc, alpha = 0.05)
  bootCI <- rbind(bootCI, boot_roc)
 }
  if("predict" %in% mic){
  boot_pred <-
    boots %>%
    mutate(mic_pred = map(splits, boot_mic_pred, x = x, y = y, tr = tr)) %>%
    int_pctl(. , mic_pred, alpha = 0.05)
  bootCI <- rbind(bootCI, boot_pred)

  }
  if("adjust" %in% mic){
  boot_adjust <-
    boots %>%
    mutate(bootrel = map_dbl(splits, boot_tr_rel, model1 = model),
           mic_adjust = map(splits, boot_mic_adjust, x = x, y = y, tr = tr, reliability = .data$bootrel)) %>%
    int_pctl(. , mic_adjust, alpha = 0.05)
  bootCI <- rbind(bootCI, boot_adjust)

  }

  bootCI



}
