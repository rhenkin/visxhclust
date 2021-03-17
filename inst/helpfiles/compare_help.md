### Clusters comparison

This tab enables:

#### 1) Saving and exporting clusters

Write a name for the current clusters and click "Save results". The download button will export a CSV file with the subject identifiers and cluster labels. Click "Clear results" to remove all saved results.

#### 2) Comparing different solutions

When two or more cluster results are saved, this panel will show a Sankey diagram indicating the flow of objects across clusters. It's **essential** to note here that *cluster are re-labeled to facilitate comparison*. For that, a feature from the data is used to rank the clusters according to its mean value within each cluster. Consider the following example:

With two clusters (1 and 2), variable A is selected. The mean value of A in cluster 1 is *lower* than the mean value of A in cluster 2. In the plot, cluster 1 then becomes cluster 2, and cluster 2 becomes cluster 1.
  
This transformation enables comparing clusters computed from a different set of features, for example, to understand how individuals move across clusters after the removal or inclusion of a particular feature.
