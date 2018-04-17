#' Computes T
#'
#'@keywords internal
#'
#'@importFrom glmnet predict.glmnet
#'
compute_criteria.ehr2pheno <- function(x, goldstandard){

  predprobs <- predict(object = x$logisticEN_model, newx=x$training_data, type="response")
  goldstandard_ordered <- goldstandard[order(predprobs, decreasing=TRUE)]

  TPR = cumsum(goldstandard_ordered)/sum(goldstandard_ordered)
  FPR = cumsum(!goldstandard_ordered)/sum(!goldstandard_ordered)
  AUC = mean(TPR)
  return(list("criteria"=list("TPR" = TPR, "FPR" = FPR, "AUC" = AUC),
              "pred_proba"=predprobs)
  )
}
