#' @title Calculate a vector mode value
#' @description Calculate a vector mode value.
#' @param x An atomic vector.
#' @return The mode value of x.
#' @export

mode_stat <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}