% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/block_randomization.R
\name{block_randomization}
\alias{block_randomization}
\title{Randomized block treatment assignment}
\usage{
block_randomization(
  df,
  blocking_vars,
  frac,
  treatment_names = c("control", "treatment")
)
}
\arguments{
\item{df}{input data.frame. should include the variables
in \code{blocking_vars}.}

\item{blocking_vars}{string vector denoting the names of
variables in \code{df} to perform block randomization over}

\item{frac}{a numeric between 0 and 1. the fraction of observations
assigned to the 2nd treatment group}

\item{treatment_names}{names of the treatment groups. default is "control"
for the first group and "treatment" for the second.}
}
\value{
a vector of treatment assignments
}
\description{
this function peforms block randomization
of 2 treatments.
}
\examples{
library(utilityFunctions)
df <- data.frame(var1 = c(rep("A", 99), rep("B", 201), rep("C", 44), rep("D", 56)),
                 var2 = c(rep("E", 25), rep("F", 100), rep("G", 100), rep("H", 100), rep("I", 75)),
                 var3 = rep("J", 400))
blocking_vars <- c("var1", "var2")

df$treatment <- block_randomization(df, blocking_vars, frac = 0.8,
                                    treatment_names = c("treat 1", "treat 2"))

# In the below example, not all blockings can attain an exact fraction of 0.8
tapply(df$treatment, paste0(df$var1, df$var2), function(x) mean(x == "treat 2"))
}
