#' Predicted Minimal Important Change (MIC)
#'
#' Minimal Important Change (MIC) obtained as predicted value from a logistic
#' regression model.
#'
#' @param data data.frame that holds the scores at two time points and the
#' transition rate in separate columns.
#' @param x vector with score at time point 1, or column name in the data if
#' `!is.null(data)`
#' @param y vector with score at time point 1, or column name in the data if
#' `!is.null(data)`
#' @param tr vector with transition rates (perceived change), or column name in
#' the data if `!is.null(data)`
#'
#' @return vector with the predicted MIC value
#' @export
#'
#' @examples
#' nitems <- 10
#' example$x <- rowSums(example[,1:nitems])                 # sumscore T1
#' example$y <- rowSums(example[,(nitems+1):(2*nitems)])    # sumscore T2
#' mic_pred(x = example$x, y = example$y, tr = example$trat)
#' mic_pred(data = example, x = example$x, y = example$y, tr = example$trat)
#' mic_pred(data = example, x = "x", y = "y", tr = "trat")
mic_pred <- function(data = NULL, x, y , tr){

  if(!is.null(data) & !is.character(x) & !is.character(y) & !is.character(tr)){
    warning("data object is not used; separate x, y and tr input is used to compute the MIC.")
  }

  if(!is.null(data) & is.character(x) & is.character(y) & is.character(tr)){
    if(!all(c(x, y, tr) %in% colnames(data))){stop("x, y and/or tr names were not found in the data.")}
    x <- data[,x]
    y <- data[,y]
    tr <- data[,tr]
  }
  q <- mean(tr)             # q = proportion improved (perceived)
  p <- log(q/(1-q))               # p = logodds(pre)

  xoc <- y - x                           # change score
  x <- data.frame(tr = tr, xoc = xoc)

  mylogit <- glm(tr ~ xoc, data = x, family = "binomial")

  C <- coef(mylogit)[1]                 # intercept coefficient C
  B <- coef(mylogit)[2]                 # regression coefficient B

   mic.pred <- (p-C)/B               # MIC(predicted)
  names(mic.pred) <- "predicted MIC"

  return(mic.pred)
}
