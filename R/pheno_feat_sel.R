#'Feature selection
#'
#'Elastic net logistic regression
#'
#'@param surrogate a vector of length \code{n} containing. Coerced to numeric.
#'
#'@param features a matrix of dimension \code{n} x \code{m} containing the features
#'from which to select predictors
#'
#'@param covariates an optional matrix of dimension \code{n} x \code{r} containing
#'additional potential predictors
#'
#'@param \dots further agruments not used here
#'
#'@return a list containing 2 components:
#'\itemize{
#' \item{logisticEN_model}{ a \code{\link[glmnet]{glmnet}} object
#' that can be used to predict external observation.}
#' \item{sel_feat}{a vector containing either the names of the selected features
#' via cross valiation on the "class" measure type (through \code{\link[glmnet]{cv.glmnet}})
#' using the \code{lambda.1se} parameter.}
#'}
#'
#'@importFrom glmnet glmnet cv.glmnet
#'
#'@export
pheno_feat_sel <- function(surrogate, features, covariates=NULL, ...){

  surrogate_num <- as.numeric(as.character(surrogate))
  index_extremes <- which(surrogate_num %in% c(0, 1))
  surrogate_extremes <- surrogate_num[index_extremes]

  if(!is.null(covariates)){
    features <- cbind(features, covariates)
  }

  model_cv <- glmnet::cv.glmnet(y=surrogate_extremes, x=features[index_extremes, , drop=FALSE],
                        family = "binomial", type.measure = "class")

  predict_model <- glmnet::glmnet(y=surrogate_extremes, x=features[index_extremes, , drop=FALSE],
                          family = "binomial", lambda = model_cv$lambda.1se)

  selected_feat <- which(abs(as.vector(predict_model$beta)) > 0)
  if(is.null(colnames(features))){
    selected_features <- selected_feat
  }else{
    selected_features <- colnames(features)[selected_feat]
  }

  return(list("logisticEN_model" = predict_model,
              "sel_feat" = selected_features,
              "training_data"= features))
}