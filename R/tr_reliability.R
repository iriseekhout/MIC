#' Reliability for the Transition Rates
#'
#' @param data data.frame with variables that are in `model`
#' @param model the model described using the `lavaan` model syntax. see model
#' for the examples or `?lavaan::model.syntax`
#' @param modification logical indicator if the modification index for the
#' latent variables should be returned. if `modification = FALSE` only the
#' reliability of the transition rating is returned.
#' @importFrom lavaan cfa fitMeasures modificationIndices parameterEstimates
#' @return `list` with reliability and modification index. if
#' `modification = FALSE` only reliabilty is returned.
#' @export
#'
#' @examples
#' model <- '
#' # Factors
#' F1 =~ v1_1+v1_2+v1_3+v1_4+v1_5+v1_6+v1_7+v1_8+
#'   v1_9+v1_10+trat
#' F2 =~ v2_1+v2_2+v2_3+v2_4+v2_5+v2_6+v2_7+v2_8+
#'   v2_9+v2_10+trat
#'
#' # Correlated errors over time
#' v1_1 ~~ v2_1
#' v1_2 ~~ v2_2
#' v1_3 ~~ v2_3
#' v1_4 ~~ v2_4
#' v1_5 ~~ v2_5
#' v1_6 ~~ v2_6
#' v1_7 ~~ v2_7
#' v1_8 ~~ v2_8
#' v1_9 ~~ v2_9
#' v1_10 ~~ v2_10
#' '
#' tr_reliability(data = example, model = model)
tr_reliability <- function(data, model, modification = TRUE){

  fit <- cfa(model, data=data, ordered=T,
             test="mean.var.adjusted")

  fitMeasures(fit, fit.measures = c("cfi.scaled","tli.scaled","rmsea.scaled",
                                    "rmsea.ci.lower.scaled","rmsea.ci.upper.scaled",
                                    "rmsea.pvalue.scaled","srmr") )

  MI <- modificationIndices(fit)
  MIst <- MI[MI$sepc.lv>0.3,] #standardized lv > 0.3


  pe <- parameterEstimates(fit, standardized=T, rsquare=T)
  rel.trat <- pe$est[pe$lhs=="trat" & pe$op=="r2"]

  if(modification){
  return(
    list("reliability" = rel.trat,
         "standardized modification index for lv" = MIst)
  )
  }
  if(!modification){
    return("reliability" = rel.trat)
  }
}
