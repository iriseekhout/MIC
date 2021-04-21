#' ROC based Minimal Important Change (MIC)
#'
#' Minimal Important Change (MIC) obtained via the ROC.
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
#' @return
#' @export
#' @importFrom pROC roc coords
#' @examples
#' nitems <- 10
#' example$x <- rowSums(example[,1:nitems])                 # sumscore T1
#' example$y <- rowSums(example[,(nitems+1):(2*nitems)])    # sumscore T2
#' mic_roc(x = example$x, y = example$y, tr = example$trat)
#' mic_roc(data = example, x = example$x, y = example$y, tr = example$trat)
#' mic_roc(data = example, x = "x", y = "y", tr = "trat")
#' mic_roc(data = example, x = "x", y = "z", tr = "trat")
mic_roc <- function(data = NULL, x, y, tr){
  #roc
 if(!is.null(data) & !is.character(x) & !is.character(y) & !is.character(tr)){
    warning("data object is not used; separate x, y and tr input is used to compute the MIC.")
 }

  if(!is.null(data) & is.character(x) & is.character(y) & is.character(tr)){
    if(!all(c(x, y, tr) %in% colnames(data))){stop("x, y and/or tr names were not found in the data.")}
    x <- data[,x]
    y <- data[,y]
    tr <- data[,tr]
  }
    xoc <- y - x                           # change score

  ## Do ROC analysis
  rocobj <- roc(tr, xoc, quiet = TRUE)
  mic.roc <- coords(rocobj, x="best", input="threshold", ret="threshold",
                    best.method="youden", transpose = TRUE)
  names(mic.roc) <- "ROC-based MIC"

  return(mic.roc)
}
