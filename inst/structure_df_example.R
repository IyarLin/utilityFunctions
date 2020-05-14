library(utilityFunctions)
sample_df <- data.frame(numeric_var = rnorm(1000),
                        factor_var = factor(sample(LETTERS, 1000, replace = TRUE)),
                        character_var = sample(letters, 1000, replace = TRUE),
                        logical_var = rnorm(1000) > 0,
                        date_var = seq.Date(as.Date("2020-01-01"),
                                            as.Date("2020-01-01") + 999,
                                            by = "day"),
                        stringsAsFactors = F)
sample_df$numeric_var[sample.int(nrow(sample_df), 150)] <- NA


# in the console one can just do:
str(sample_df)

# if we want to have something close to put in a report we can use:
structure_df(sample_df)
