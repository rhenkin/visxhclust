### Sidebar

This sidebar controls the main clustering workflow: 

1. Load an RDS, CSV, TSV or tab-delimited text file
1. Select a scaling method, distance metric and linkage method
1. Select the number of desired clusters
1. Select the variables/features to use in clustering

Most tabs will be updated with the results corresponding to the current settings. The sidebar contains two additional controls to help with the analysis:

*Heatmap annotations:* any categorical variable loaded with the data will be available for selection, as well as the numeric variables that contain missing values.

*Correlation-based feature selection:* a one-sided t-test based on the chosen threshold will be use to automatically remove highly correlated variables. You can inspect the relationship between the variables that were deselected in the *Data overview* tab, under *Highly correlated variables*.
