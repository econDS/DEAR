% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist_diag.R
\name{dist_diag}
\alias{dist_diag}
\title{distribution diagnosis}
\usage{
dist_diag(x, title = deparse(substitute(x)))
}
\arguments{
\item{x}{a numerical vector}

\item{title}{title of plot}
}
\value{
histogram and box plot
}
\description{
create histogram and boxplot of variable.
}
\examples{
dist_diag(mtcars$mpg)
}
