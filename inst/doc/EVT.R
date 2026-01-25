## ----setup, echo=FALSE, message=FALSE, warning=FALSE------
knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.align = 'center')
options(width = 60)
backup_options <- options()

## ----load-library, results='hide', echo=FALSE-------------
library(RecordLinkage)

## ----prepare-data-----------------------------------------
data(RLdata500)
bf <- list(1, 3, 5, 6, 7)
rpairs <- compare.dedup(RLdata500, identity = identity.RLdata500,
                        blockfld = bf, strcmp = 1:4)
rpairs <- emWeights(rpairs)

## ----interactive-example, results='hide', eval=FALSE------
# ## Not run: getParetoThreshold(rpairs)

## ----mrl-plot, echo=FALSE, fig.cap="**Figure 1:** Basic MRL plot", fig.width=6, fig.height=4----
plotMRL(rpairs)

## ----mrl-plot-interval, echo=FALSE, fig.cap="**Figure 2:** MRL plot with appropriate graph segment marked", fig.width=6, fig.height=4----
plotMRL(rpairs)
abline(v = c(1.2, 12.8), col = "red", lty = "dashed")
l <- mrl(rpairs$Wdata)
range <- l$x > 1.2 & l$x < 12.8
points(l$x[range], l$y[range], col = "red", type = "l")

## ----classification---------------------------------------
threshold <- getParetoThreshold(rpairs, interval = c(1.2, 12.8))
result <- emClassify(rpairs, threshold)
summary(result)

## ----cleanup, echo=FALSE, results='hide'------------------
options(backup_options)

