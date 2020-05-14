library(utilityFunctions)
df <- data.frame(var1 = c(rep("A", 99), rep("B", 201), rep("C", 44), rep("D", 56)),
                 var2 = c(rep("E", 25), rep("F", 100), rep("G", 100), rep("H", 100), rep("I", 75)),
                 var3 = rep("J", 400))
blocking_vars <- c("var1", "var2")

df$treatment <- block_randomization(df, blocking_vars, frac = 0.8,
                                    treatment_names = c("treat 1", "treat 2"))

# In the below example, not all blockings can attain an exact fraction of 0.8
tapply(df$treatment, paste0(df$var1, df$var2), function(x) mean(x == "treat 2"))
