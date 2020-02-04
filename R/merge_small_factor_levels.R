#' @title Merge small factor levels.
#' @description Given an input factor/character vector,
#'   merges together all vector levels that account for
#'   less than \code{min.perc} of the total number of
#'   elements in the vector
#' @param x Either character of factor vector
#' @return Factor vector, with small levels merged
#' @export

merge_small_factor_levels <- function(x, min.perc = 0.01, case_weights = NULL){
  # min.perc = minimum percentage of non NA values
  if(!class(x) %in% c("character", "factor")) stop("input vector class should be either character or factor")
  if(class(x) == "character") x <- factor(x)
  if(is.null(case_weights)) case_weights <- rep(1, length(x))
  good_levels <- make.names(levels(x))
  if(!identical(good_levels, levels(x))) stop("Levels of input variable do not make good names. Consider running make.names on that variable prior to feeding it to this function")
  x_lev <- sort(tapply(case_weights, x, sum))
  x_lev <- x_lev/sum(x_lev)
  n_merge <- sum(x_lev < min.perc)
  if (n_merge == length(x)){
    stop("Factor has no levels with a fraction more than min.perc")
  } else if (n_merge > 0){
    small_lev <- names(x_lev)[1:n_merge]
    levels(x)[levels(x) %in% small_lev] <- ".merged"
  }
  x <- droplevels(x)
  return(x)
}
