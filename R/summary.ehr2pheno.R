#'Summarize a phenotype prediction
#'
#'Summarize a phenotype prediction from diagnosis codes and NLP processed notes
#'
#'@details Computes the True Positives Rate (TPR), False Positives Rate (FPR),
#'and Area Under the ROC Curve (AUC).
#'
#'@details WARNING: the criteria are computed on the training data itself and
#'are therefore over-optimistic !
#'
#'@param object An object of class \code{"ehr2pheno"}
#'
#'@param goldstandard a binary vector containing the gold-standard phenotypes
#'
#'@param \dots Arguments to be passed to or from other methods
#'
#'@return an object of class \code{"summary.ehr2pheno"} with the following elements:
#'\itemize{
#'  \item{\code{AUC}: }{a vector of length 3 containing the AUC on the training data for
#'  \code{codes} only, \code{nlp} only, and \code{both} codes and nlp.}
#'  \item{\code{TPR}: }{a matrix of 3 columns the TPR for each threshold
#'  on the training data for \code{codes} only, \code{nlp} only, and
#'  \code{both} codes and nlp.}
#'  \item{\code{FPR}: }{a matrix of 3 columns the FPR for each threshold
#'  on the training data for \code{codes} only, \code{nlp} only, and
#'  \code{both} codes and nlp.}
#'}
#'
#'@seealso \code{\link{ehr2pheno}}
#'
#'@export
summary.ehr2pheno <- function(object, goldstandard = NULL, ...){


  if(is.null(goldstandard)){
    stop("Please provide an 'goldstandard' argument")
  }

  message("Computing over-optimistic criteria on the training set")
  predEN <- lapply(object$feat_sel, FUN = compute_criteria.ehr2pheno, goldstandard = goldstandard)

  criteria_list <- predEN[["criteria"]]
  TPR <- sapply(criteria_list, "[[", "TPR")
  FPR <- sapply(criteria_list, "[[", "FPR")
  AUC <- sapply(criteria_list, "[[", "AUC")

  browser()
  beta_list <- lapply(lapply(object$feat_sel, "[[", "logisticEN_model"), "[[", "beta")
  beta_sel_index <- lapply(beta_list, FUN = function(x){which(abs(as.vector(x))>0)})
  beta_sel <- mapply(FUN="[", beta_list, beta_sel_index, j=1, drop=FALSE)

  res <- list("criteria"=list("AUC" = AUC, "TPR" = TPR, "FPR" = FPR),
              "pred_proba" = predEN[["pred_proba"]],
              "selected_OR" = list()
  )
  class(res) <- "summary.ehr2pheno"

  return(res)
}

