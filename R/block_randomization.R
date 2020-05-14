#' @title Randomized block treatment assignment
#'
#' @description this function peforms block randomization
#' of 2 treatments.
#'
#' @param df input data.frame. should include the variables
#' in \code{blocking_vars}.
#' @param blocking_vars string vector denoting the names of
#' variables in \code{df} to perform block randomization over
#' @param frac a numeric between 0 and 1. the fraction of observations
#' assigned to the 2nd treatment group
#' @param treatment_names names of the treatment groups. default is "control"
#' for the first group and "treatment" for the second.
#'
#' @return a vector of treatment assignments
#' @example inst/block_randomization_example.R
#' @export

block_randomization <- function(df, blocking_vars, frac,
                                treatment_names = c("control", "treatment")){
  df <- df[blocking_vars]
  df$comb_vars <- apply(df, 1, function(row) paste0(row, collapse = ""))
  comb_vars <- unique(df$comb_vars)

  df$row <- NA
  df$num_rows <- NA
  for(comb in comb_vars){
    df$row[df$comb_vars==comb] <- sample(1:nrow(df[df$comb_vars==comb, ]))
    df$num_rows[df$comb_vars==comb] <- nrow(df[df$comb_vars==comb, ])
  }
  df$odd_and_last <- df$row == df$num_rows & df$num_rows %% 2 == 1
  df$odd_and_last_row <- NA
  df$odd_and_last_row[df$odd_and_last] <- sample(1:sum(df$odd_and_last))
  df$odd_and_last_sum <- NA
  df$odd_and_last_sum[df$odd_and_last] <- sum(df$odd_and_last)

  # assign control and treatment!

  treatment <- treatment_names[as.integer(
    ifelse(df$odd_and_last, df$odd_and_last_row <= df$odd_and_last_sum * frac,
           df$row <= df$num_rows * frac)) + 1]


  return(treatment)
}
