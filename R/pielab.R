#' pielab
#'
#' create a pie chart with percentage label.
#'
#' @param x a vector of categories variable
#' @param lb a label of each category
#' @param cl a vector of colors
#' @param tt a title of plot
#' @param center start division line at center
#' @param fade make the pie chart fade
#'
#' @return piechar and freqeuncy_table
#' @export
#'
#' @examples
#' pielab(iris$Species)
pielab <- function(x, lb=c("Cat1","Cat2"), cl=c("cyan","pink"), tt="Title", center=FALSE ,fade=FALSE) {
  ft <- factor(x)
  pct <- round(table(ft)*100/sum(table(ft)))
  lb <- paste(lb, pct)
  lb <- paste(lb,"%",sep="")

  if (center==TRUE & fade==TRUE){
    pie(table(ft), col=cl, labels = lb, main=tt, init.angle = 270, density=50)
  } else if (center==TRUE & fade==FALSE) {
    pie(table(ft), col=cl, labels = lb, main=tt, init.angle = 270)
  } else if (center==FALSE & fade==TRUE) {
    pie(table(ft), col=cl, labels = lb, main=tt, density=50)
  } else {
    pie(table(ft), col=cl, labels = lb, main=tt)
  }
  freqeuncy_table <- ft
  return(table(freqeuncy_table))
}
