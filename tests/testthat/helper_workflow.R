iris_numeric <- iris[, c("Petal.Length", "Sepal.Length")]
scaled_df <- scale_data(iris_numeric)
dmat <- compute_dmat(scaled_df)
clusters <- compute_clusters(dmat, "complete")
cluster_labels <- cut_clusters(clusters, 2)
dunn_res <- compute_metric(scaled_df, clusters, "Dunn")

iris_all <- iris[, c("Petal.Length", "Sepal.Length", "Petal.Width", "Sepal.Width")]
scaled_all <- scale_data(iris_all)
dmat_all <- compute_dmat(scaled_all)
clusters_all <- compute_clusters(dmat_all, "single")
ch_res <- compute_metric(scaled_all, clusters_all, "Calinski_Harabasz", 11)
