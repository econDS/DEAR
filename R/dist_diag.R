#' distribution diagnosis
#'
#' create histogram and boxplot of variable.
#'
#' @param x a numerical vector
#' @param title title of plot
#'
#' @return histogram and box plot
#' @export
#'
#' @examples
#' dist_diag(mtcars$mpg)
dist_diag <- function(x, title=deparse(substitute(x))) {
  par(mfrow=c(1,2), oma = c(0, 0, 1.7, 0))
  hist(x, main = "Histogram", xlab = "")
  boxplot(x, main="Boxplot", horizontal = T)
  mtext(title, outer = TRUE, cex = 1.5)
  par(mfrow=c(1,1), oma = c(0, 0, 0, 0))
}
