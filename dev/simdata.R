# Simulate binary data
bin_randoms <- lapply(1:10, function(x) {
  x <- qnorm(runif(1,min=pnorm(0),max=pnorm(1)))[1]
  sample(0:1, 500, TRUE, prob = c(1-x, x))
})
bin_df <- data.frame(bin_randoms)
colnames(bin_df) <- letters[1:10]
write.csv(bin_df, "dev/sim_binary.csv", row.names = FALSE)
# Simulate log scaled data
lambdas <- sample(1:10, 10)
randoms <- lapply(lambdas, function(x) log1p(rpois(500, x)))
df <- data.frame(randoms)
colnames(df) <- letters[1:10]
write.csv(df, "dev/sim_logscaled.csv", row.names = FALSE)
