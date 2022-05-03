#' Cause-specific hazard test data set
#'
#' A dataset containing three covariates, a cause of interest,
#' and a competing cause.
#'
#' @format A data frame with 177 rows and 7 variables:
#' \describe{
#'     \item{Sex}{Patient sex, M = Male, F = Female}
#'     \item{D}{Disease}
#'     \item{Phase}{CR1, CR2, CR3}
#'     \item{Source}{Type of transplant, PB or BM+PB}
#'     \item{Age}{Patient age, years}
#'     \item{ftime}{Time of event in months}
#'     \item{Status}{Status indicator, 0 = censored, 1 = relapse, 2 = competing event}
#' }
#'
#'@source \url{http://www.stat.unipg.it/luca/R/bmtcrr.csv}
"bmtcrr"
