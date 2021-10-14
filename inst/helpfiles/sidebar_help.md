### Sidebar

This sidebar controls the main clustering workflow: 

1. Load an RDS, CSV, TSV or tab-delimited text file
1. Select a scaling method, distance metric and linkage method
1. Select the desired number of clusters
1. Select the variables/features to use in clustering

Most tabs will then be updated without any additional inputs. The sidebar contains two other controls to help with the analysis:

*Heatmap features:* variables selected from this list will be added to the top of the heatmaps annotations. This list will automatically include any non-numeric variable loaded with the data (e.g. text) or numeric variables that contain missing values, which cannot be used for clustering.

*Correlation-based feature selection:* a one-sided t-test based on the chosen threshold will be use to automatically remove highly correlated variables. You can inspect the relationship between the variables that were deselected in the *Data overview* tab, under *Highly correlated variables*.

### Example data

The following datasets can be downloaded, inspected and loaded into the tool to understand the required format:  

1. [Normally-distributed data](data/sim_normal.csv)  
1. [Normally-distributed data with missing values](data/sim_normal_missing.csv)  
1. [Normally-distributed data with annotation](data/sim_normalannot.csv)  
1. [Binary data](data/sim_binary.csv)  
1. [Log-scaled data](data/logscaled.csv)  
