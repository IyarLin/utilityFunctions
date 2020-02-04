#' @title data.frame structure in data.frame format
#' @description This function produces output very similar to
#'   the one obtained by the \code{str} function, only it returns
#'   a data.frame object which can be used for example in
#'   rmarkdown reports
#' @param data A data.frame for which the structure is to be given
#' @param max_string_length maximum string length in the sample_values column
#' @return A data.frame with the following columns:
#'   1. variable: vairable name
#'   2. class: Variable class
#'   3. sample_values: Sample of the values that variable can take
#'     for factor variables and main stasitics (mean, median etc) for
#'     a numeric variable
#'   4. missing: Percent missing values
#' @export

structure_df <- function(data, max_string_length = 60) {
  tab <- lapply(data, function(column) {
    if (class(column)[1] == "numeric" | class(column) == "integer") {
      unique_values <- summary(column)[1:6]
      zeros <- str_extract(unique_values, "0\\.0+")
      zeros[is.na(zeros)] <- "00"
      zeros <- max(mean(nchar(zeros) - 2), 2)
      sample_values <- paste0(paste(names(unique_values), round(unname(unique_values), zeros), sep = " = "), collapse = ", ")
      ans <- data.frame(
        class = class(column),
        sample_values = sample_values,
        missing = paste0(round(mean(is.na(column)) * 100, 2), "%"),
        stringsAsFactors = F
      )
    } else if (class(column)[1] == "factor" | class(column)[1] == "ordered") {
      unique_values <- unique(na.omit(column))
      n_unique_values <- length(unique_values)
      sample_values <- paste0(sample(unique_values, size = min(5, n_unique_values), replace = F), collapse = ", ")
      if (nchar(sample_values) > max_string_length) {
        sample_values <- paste0(substr(sample_values, 1, max_string_length - 3), "...")
      }
      ans <- data.frame(
        class = paste0("factor with ", n_unique_values, " levels"),
        sample_values = sample_values,
        missing = paste0(round(mean(is.na(column)) * 100, 2), "%"),
        stringsAsFactors = F
      )
    } else if (class(column)[1] == "character") {
      unique_values <- unique(na.omit(column))
      n_unique_values <- length(unique_values)
      sample_values <- paste0(sample(unique_values, size = min(5, n_unique_values), replace = F), collapse = ", ")
      if (nchar(sample_values) > max_string_length) {
        sample_values <- paste0(substr(sample_values, 1, max_string_length - 3), "...")
      }
      ans <- data.frame(
        class = paste0("char with ", n_unique_values, " unique values"),
        sample_values = sample_values,
        missing = paste0(round(mean(is.na(column)), 2) * 100, "%"),
        stringsAsFactors = F
      )
    } else if (class(column)[1] == "logical") {
      ans <- data.frame(
        class = "logical",
        sample_values = paste0(
          "FALSE freq = ", round(sum(column == FALSE, na.rm = T) / length(column), 3), ", ",
          "TRUE freq = ", round(sum(column == TRUE, na.rm = T) / length(column), 3)
        ),
        missing = paste0(round(mean(is.na(column)) * 100, 2), "%"),
        stringsAsFactors = F
      )
    } else if (class(column)[1] == "Date") {
      sample_values <- as.character(as.Date(as.vector(summary.Date(column)[1:6]), origin = "1970-01-01"))
      ans <- data.frame(
        class = "Date",
        sample_values = paste0(sample_values, collapse = ", "),
        missing = paste0(round(mean(is.na(column)) * 100, 2), "%"),
        stringsAsFactors = F
      )
    }
    return(ans)
  })
  tab <- bind_rows(tab)
  tab <- tab %>%
    mutate(variable = names(data)) %>%
    select(variable, everything())
  return(tab)
}
