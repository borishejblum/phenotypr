#' Split surrogate into 3 ordered categories based on the number of feature occurences
#'
#' Either fit 3 components Gaussian mixture to determine cutpoints for surrogate variable
#' of uses the most extremes quantiles
#'
#' @param x a feature occurence count vector to be split into 3 components
#' @param method a character string indicating which splitting method is
#' used : either \code{"GaussianMixture"} or \code{"quantiles"}. Default is
#' \code{"quantiles"}.
#' @param p the probability of extemes used in the quantile splitting method
#' (ignored if \code{method} is \code{"GaussianMixture"}). Default is \code{0.01}.
#'
#' @param \dots further agruments not used here
#'
#' @return a list with the following components:
#'  \itemize{
#'  \item{\code{thesholds}}{a list containing the lower threshold \code{lowthres}
#'  marking the difference between the two classes \code{0} (the fewest occurences) and \code{0.5};
#'  and the upper threshold \code{upthres} marking the difference between the
#'  two classes \code{0.5} and \code{1} (the most occurences)}
#'  \item{surrogate} a vector containing the clustering for each observation (as
#'  an ordered factor with the 3 following levels ordered by the number of occurences:
#'  \code{0 < 0.5 < 1}.)
#'  }
#'
#' @import mclust
#'
#' @export
get_thresholds <- function(x, method=c("quantiles", "GaussianMixture"), p=0.1, ...){

  if(length(method)>1){
    method <- method[1]
  }

  if(method == "GaussianMixture"){
    mixmod <- mclust::Mclust(data = x, G = 3, modelNames = "E")

    s <- factor(mixmod$classification, levels = order(mixmod$parameters$mean), ordered=TRUE)
    levels(s) <- c(0, 0.5, 1)

    maxes <- tapply(x, INDEX = s, FUN=max)
    mines <- tapply(x, INDEX = s, FUN=min)
    lowthres <- mean(c(maxes["0.5"], mines["1"]))
    upthresh <- mean(c(maxes["0"], mines["0.5"]))

  }else if(method == "quantiles"){
    lowthres <- quantile(x, probs = p) + 0.5
    upthresh <- quantile(x, probs = 1-p) + 0.5

    s <- cut(x, breaks=c(0, lowthres, upthresh, max(x)), labels=c("0", "0.5", "1"), ordered_result=TRUE, right=FALSE)
  }

  if(any(table(s)==0)){
    print(table(s))
    stop("At least of the surrogate level is empty...")
  }

  return(list("thesholds" = c("lowthres" = lowthres,
                              "upthresh" = upthresh),
              "surrogate" = s)
  )
}
