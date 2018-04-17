#'Plotting ROC curves from the summary of an ehr2pheno Object
#'
#'Plotting ROC curves from the summary of an ehr2pheno Object
#'
#'@details WARNING: the ROC are computed on the training data itself and
#'are therefore over-optimistic !
#'
#'@param x \code{summary.ehr2pheno} object
#'
#'@param \dots Arguments to be passed to or from other methods
#'
#'@seealso \code{\link{ehr2pheno}} \code{\link{summary.ehr2pheno}}
#'
#'@importFrom reshape2 melt
#'
#'@import ggplot2
#'@importFrom viridis scale_color_viridis
#'
#'@export
plot.summary.ehr2pheno <- function(x, ...){

  message("Plotting over-optimistic ROC curves on the training set...")

  xx <- x$criteria

  df2plot <- merge(reshape2::melt(xx$TPR, value.name = "TPR"), reshape2::melt(xx$FPR, value.name = "FPR"))
  colnames(df2plot)[c(1,2)] <- c("Patient", "Features")

  p <- ggplot(df2plot) +
    geom_abline(slope = 1, intercept = 0, color="grey50") +
    geom_line(aes(x = FPR, y = TPR, color = Features)) +
    theme_bw() +
    viridis::scale_color_viridis(discrete = TRUE) +
    ggtitle("AUC",
            subtitle = paste(paste(names(xx$AUC), formatC(xx$AUC, digits=3, format="f"), sep=": "), collapse=" ; ")
    )

  return(p)
}
