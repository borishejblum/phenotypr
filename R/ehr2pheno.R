#'Phenotyping from EHR data
#'
#'Using both diagnosis codes and CUIs extracted from NLP processed notes and selecting
#'predicting features
#'
#'@param codes a \code{n x q} matrix containg the occurences of the \code{q} diagnosis
#'codes related to the phenotype of interest for all \code{n} patients
#'
#'@param nlp a \code{n x p} matrix containg the occurences of the \code{q} diagnosis
#'codes related to the phenotype of interest for all \code{n} patients
#'
#'@param covariates an optional matrix of dimension \code{n x r} containing
#'additional potential predictors
#'
#'@param \dots further arguments to be passed to \code{\link[phenotypr]{get_thresholds}}
#'or \code{\link[phenotypr]{pheno_feat_sel}} subfunctions
#'
#'@return an object of type \code{ehr2pheno}, which is a list of the following elements:
#'\itemize{
#'  \item{\code{feat_sel}: }{itself a list with the three \code{glmnet} models,
#'    \code{"codes"}, \code{"nlp"} and  \code{"both"}}
#'  \item{\code{surrogates}: }{itself a list with the three constructed surrogates,
#'    \code{"codes"}, \code{"nlp"} and  \code{"both"}}
#'}
#'
#'@export
ehr2pheno <- function(codes, nlp, covariates=NULL, ...){

  #codes only
  codes_surrogate <- get_thresholds(x = rowSums(codes), ...)

  #nlp only
  nlp_surrogate <- get_thresholds(x = rowSums(nlp), ...)

  #both
  composite_surrogate <- (as.numeric(as.character(codes_surrogate$surrogate)) +
                            as.numeric(as.character(nlp_surrogate$surrogate)))/2

  composite_surrogate_char <- as.character(composite_surrogate)
  composite_surrogate_char[composite_surrogate>0.5] <- "1"
  composite_surrogate_char[composite_surrogate<0.5] <- "0"
  composite_surrogate_char[composite_surrogate == 0.5] <- "0.5"
  composite_surrogate <- factor(composite_surrogate_char, levels = c("0", "1", "0.5"), ordered=TRUE)

  codes_feat_sel <- pheno_feat_sel(surrogate = codes_surrogate$surrogate, features = codes, covariates = covariates)
  nlp_feat_sel <- pheno_feat_sel(surrogate = nlp_surrogate$surrogate, features = nlp, covariates = covariates)
  composite_feat_sel <- pheno_feat_sel(surrogate = composite_surrogate, features = cbind(codes, nlp), covariates = covariates)

  res <- list("feat_sel" = list("codes" = codes_feat_sel, "nlp" = nlp_feat_sel,
                                "both" = composite_feat_sel),
              "surrogates" = list("codes" = codes_surrogate, "nlp" = nlp_surrogate,
                                  "both" = composite_surrogate)
  )

  class(res) <- "ehr2pheno"

  return(res)

}