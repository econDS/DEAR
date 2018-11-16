#' barplot with label and sorted frequency
#'
#' @param x a categorical variable.
#' @param percent a logical for display label as percentage.
#' @param title a title of plot
#' @param unlimit a logical for set plot more than 10 categories
#'
#' @return
#' @export
#'
#' @examples
#' a <- c("A", "A", "B", "B", "B", "C")
#' barlab_order(a)
#' barlab_order(a, percent = TRUE)
barlab_order <- function(x, percent=FALSE, title=deparse(substitute(x)), unlimit=FALSE) {
  freq <- as.matrix(table(x))
  ind  <- order(freq, decreasing = TRUE)
  lbl <- attr(freq, "dimnames")[[1]][ind]

  num_cat <- length(freq)
  max_char_10 <- max(nchar(lbl[1:min(num_cat, 10)]))
  if(unlimit){
    barplot(freq[ind], names.arg = lbl[ind], col=rainbow(10),
            main=title, ylab='count')
  } else {
    if(num_cat >= 10){
      cat("There is", num_cat, "categories. Only first top 10th would plot")
      op <- par(mar=c(min(max_char_10,25)*0.5,4,4,2))
      p <- barplot(freq[ind][1:10], names.arg = "", col=rainbow(10), las=2,
                   ylim=c(0, max(freq)*1.11), main=title, ylab='count')
      text(p[,1], -3, srt = 65, adj= 1, xpd = TRUE, labels = lbl[1:10] , cex=1.2)
      text(p, freq[ind][1:10], label = freq[ind][1:10], pos = 3, cex = 0.8)
      rm(op)
    } else {
      if(percent){
        pct <- round(table(x)*100/sum(table(x)))
        pct_lb <- paste(pct,"%",sep="")
        p <- barplot(pct[ind], names.arg = lbl, main=title,
                     col=rainbow(10), ylim=c(0, max(pct)*1.2), ylab='percent')
        text(p, pct[ind], label = pct_lb[ind], pos = 3, cex = 0.8)
      } else {
        p <- barplot(freq[ind], names.arg = lbl, main=title,
                     col=rainbow(10), ylim=c(0, max(freq)*1.2), ylab='count')
        text(p, freq[ind], label = freq[ind], pos = 3, cex = 0.8)
      }
    }
  }
}


