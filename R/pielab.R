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
#' a <- c("A","A","A","B","B","C")
#' pielab(a)
pielab <- function(x, lb="default", cl="default", tt="", center=FALSE ,fade=FALSE) {
  ft <- factor(x)
  pct <- round(table(ft)*100/sum(table(ft)), digits = 1)

  if(lb=="default"){
    lb <- unique(ft)
    lb <- paste(lb, pct)
    lb <- paste(lb,"%",sep="")
  } else {
    lb <- paste(lb, pct)
    lb <- paste(lb,"%",sep="")
  }

  if(cl=="default"){
    color_set <- c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C",
                   "#FB9A99","#E31A1C","#FDBF6F","#FF7F00")
    cl <- color_set[1:length(unique(ft))]
  }

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
