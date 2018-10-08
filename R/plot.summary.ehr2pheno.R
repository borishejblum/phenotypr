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
#'@import reshape2
#'@importFrom viridis scale_color_viridis
#'
#'@export
plot.summary.ehr2pheno <- function(x, ...){

  message("Plotting over-optimistic ROC curves on the training set...")

  xx <- x$criteria

  df2plot <- merge(reshape2::melt(xx$TPR, value.name = "TPR"), reshape2::melt(xx$FPR, value.name = "FPR"))
  colnames(df2plot)[c(1,2)] <- c("Patient", "Features")

  p <- ggplot(df2plot) +
    geom_ribbon(aes_string(x = "FPR", ymax = "TPR", ymin="FPR", fill = "Features"), alpha=0.15) +
    geom_line(aes_string(x = "FPR", y = "TPR", color = "Features")) +
    geom_abline(slope = 1, intercept = 0, color="grey50") +
    theme_bw() +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    viridis::scale_color_viridis("Predictive features", discrete = TRUE, labels = c("Diagnosis codes", "NLP", "Both")) +
    viridis::scale_fill_viridis("Predictive features", discrete = TRUE, labels = c("Diagnosis codes", "NLP", "Both")) +
    ggtitle("AUC",
            subtitle = paste(paste(c("Diagnosis codes", "NLP", "Both"), formatC(xx$AUC, digits=3, format="f"), sep=": "), collapse=" ; ")
    ) +
    labs(caption = "Receiver Operating Curves on training set")
  return(p)
}
