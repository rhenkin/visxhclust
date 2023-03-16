iris_numeric <- iris[c("Petal.Length", "Sepal.Length")]
scaled_df <- scale(iris_numeric)
scaled_unselected_df <- scale(iris[c("Petal.Width", "Sepal.Width")])
dmat <- compute_dmat(scaled_df)
clusters <- compute_clusters(dmat, "complete")
cluster_labels <- cut_clusters(clusters, 2)
longdata_with_labels <- annotate_clusters(iris_numeric, cluster_labels)
widedata_with_labels <- annotate_clusters(iris_numeric, cluster_labels, long = FALSE)
dunn_res <- compute_metric(dmat, clusters, "dunn")

iris_all <- iris[c("Petal.Length", "Sepal.Length", "Petal.Width", "Sepal.Width")]
scaled_all <- scale(iris_all)
dmat_all <- compute_dmat(scaled_all)
clusters_all <- compute_clusters(dmat_all, "single")
ch_res <- compute_metric(dmat_all, clusters_all, "silhouette", 11)
