#Simulate normal data
normal_random <- lapply(1:10, function(x) {
  rnorm(200, runif(1, -5, 5), runif(1, 0, 10))
})
normal_df <- data.frame(normal_random)
colnames(normal_df) <- letters[1:10]
write.csv(normal_df, "dev/sim_normal.csv", row.names = FALSE)
file.copy("dev/sim_normal.csv", "inst/www/data/sim_normal.csv", overwrite = TRUE)
save(normal_df, file = "data/sim_normal.rda")

# Add annotation
normal_annotated <- cbind(normal_df, "annot" = rep(c("A", "B"), each = 100))
write.csv(normal_annotated, "dev/sim_normalannot.csv", row.names = FALSE)
file.copy("dev/sim_normalannot.csv", "inst/www/data/sim_normalannot.csv", overwrite = TRUE)
save(normal_annotated, file = "data/sim_normalannot.rda")
normal_missing <- normal_df
normal_missing[sample(200, 50), "j"] <- NA
write.csv(normal_missing, "dev/sim_normal_missing.csv", row.names = FALSE)
file.copy("dev/sim_normal_missing.csv", "inst/www/data/sim_normal_missing.csv", overwrite = TRUE)
save(normal_missing, file = "data/sim_normal_missing.rda")
# Simulate binary data
bin_randoms <- lapply(1:10, function(x) {
  x <- qnorm(runif(1,min=pnorm(0),max=pnorm(1)))[1]
  sample(0:1, 500, TRUE, prob = c(1-x, x))
})
bin_df <- data.frame(bin_randoms)
colnames(bin_df) <- letters[1:10]
write.csv(bin_df, "dev/sim_binary.csv", row.names = FALSE)
file.copy("dev/sim_binary.csv", "inst/www/data/sim_binary.csv", overwrite = TRUE)
save(bin_df, file = "data/sim_binary.rda")
# Simulate log scaled data
lambdas <- sample(1:10, 10)
randoms <- lapply(lambdas, function(x) log1p(rpois(500, x)))
logscaled_df <- data.frame(randoms)
colnames(logscaled_df) <- letters[1:10]
write.csv(logscaled_df, "dev/sim_logscaled.csv", row.names = FALSE)
file.copy("dev/sim_logscaled.csv", "inst/www/data/sim_logscaled.csv", overwrite = TRUE)
save(logscaled_df, file = "data/sim_logscaled.rda")
