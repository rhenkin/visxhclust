### Internal validation

This tab show the results of a selected internal validation measures for *k* clusters ranging from 2 to 14. The clusterCrit package is used to compute the measures/indices. For more information, please check the documentation of the package at [https://cran.r-project.org/web/packages/clusterCrit/index.html]. To facilitate interpretation, it is possible to highlight first or global minimum or maximum values.

The following is a short summary about which values to look for in each metric:  
**Maximum:** Calinski-Harabasz, Dunn, GDI, Gamma, PBM, Point biserial, Ratwosky-Lance, Silhouette, Tau, Wenmert-Gancarski  
**Minimum:** Banfeld-Raftery, C-index, Davies-Bouldin, G-plus, McClain-Rao, Ray-Turi, Scott-Symons, SD, Xie-Beni  

The remaining measures are the maximum or minimum difference between consecutive k's:  
**Max diff:** Ball-Hall, Ksq, Trace  
**Min diff:** Det ratio, log Det, log SS

