---
title: 'visxchlust: An R Shiny package for visual exploration of hierarchical clustering'
tags:
  - R
  - clustering
  - interactive visualization
  - visual analytics
  - Shiny
authors:
  - name: Rafael Henkin
    orcid: 0000-0002-5511-5230
    affiliation: 1
  - name: Michael R. Barnes
    orcid: 0000-0001-9097-7381
    affiliation: 1
affiliations:
 - name: Centre for Translational Bioinformatics, Queen Mary University of London
   index: 1
date: 24 November 2021
bibliography: paper.bib

---

# Summary

**visxchlust** is an R Shiny[@Shiny] web app that facilitates iterative exploration of hierarchical clustering. The package assembles together the outputs of different steps of a clustering workflow in a fluid visual analytics[@Keim] interface, enabling analysts to revise selected features and hyperparameters and quickly re-evaluate results. It focuses on hierarchical clustering, a widely used method often chosen due to the familiarity of users with the tree-like structure of results presented as dendrograms and its frequent combination with visualization techniques such as heatmaps, which are included in popular visualization packages. Package reference and an illustrated tutorial are available [online](https://rhenkin.github.io/visxhclust/articles/visxhclust.html).

# Statement of need

Clustering methods are supported by dozens of packages in popular data analysis languages such as R and Python. In R, basic clustering functions are included with standard R installations [@Rbase] and can be extended by various kinds of packages. In Python, state-of-the-art packages such as scikit-learn[@scikit-learn] also include considerable support for clustering. The visxchlust package addresses the overheads and difficulties that emerge when analysts have to combine multiple packages to create a flexible clustering workflow and iterate over combinations of hyperparameters and features. It integrates computation, plotting and evaluation of hierarchical clustering in a visual analytics interface by building on existing packages from the R ecosystem. The interface enables fast iterative workflows without requiring users to learn how to use specific packages or manipulate inputs and outputs of different packages. While the web app does not require any programming knowledge at all to be used, the package also exports internal functions to help with reproducibility and preparation of figures for publication.

# Overview

Users can install the visxhclust package in their local R installation and run the Shiny app in their browser. The app layout is split into a sidebar, through which the user can load data and change the clustering settings, and a main view where the user can navigate through tabs with various outputs (see figure \autoref{fig:interface}). Once the app is running, users can begin the clustering step of their analytical workflow by loading the data. By default, the tool will transform all features into standard scores by using the `scale()` function from R, though users can load standardized datasets and disable the scaling function. The app will also compute Spearman's rank correlation across features and deselect strongly correlated features (default threshold of 0.9) -- as this does not hide features, users can easily re-select features if so desired. The initial setup will compute two clusters using Euclidean distance and Ward's linkage method; with these settings, when the user loads a dataset, almost every clustering-related output will be updated across the app as soon as the user opens the corresponding. The tool supports the distance measures included in standard R distributions (Euclidean, maximum, Manhattan, Canberra, binary and Minkowski), plus cosine and Mahalanobis distance [@Mahalanobis1936]. 

![Interface of visxchlust after loading the app.\label{fig:interface}](interface.png)

The main clustering results are displayed in the heatmap with dendrogram tab, which is based on the `ComplexHeatmap` [@Gu2016] package. By default, all numeric features will be displayed, with the features not used in the clustering shown directly below the main heatmap. In the sidebar, users can also select categorical features to be added as annotations in the top rows of the heatmap, as well as numeric features that contain missing values and that were identified when the dataset was loaded. As the heatmap shows the standardized data, users can investigate the characteristics of the clusters through a tab that contains plots showing the distribution of the original values of features across the clusters. The same tab also contains textual summaries of median, lower and upper quartiles for the features, and plots showing the distribution of data points for categorical features across clusters. Finally, as part of the main features of the clustering workflow, various internal validation metrics can be computed through the `clusterCrit` [@clusterCrit] package. After these visualization and evaluation steps, users can quickly change the settings in the sidebar to compute another set of clusters and continue with the loop until satisfied with a solution. The data annotated with the cluster labels can be downloaded at any point from the sidebar.

Besides these views, the tool also contains a tab for the computation of the Gap statistic [@Tibshirani2001] as an additional evaluation metric and a tab that enables comparing differences across clusters for the features that were not used in the clustering [@Dinno2017]. Finally, basic support for data diagnostics is provided through tabs showing correlation heatmaps, PCA plots and projection of distance matrix, as well as a tabular view of clusters for inspection and removal of data points. The interface contains multiple help points for each tab and also includes downloadable data examples (e.g. if the app is deployed in an internal server for multiple users), and the package documentation includes a visual tutorial and two examples of workflows using the exported functions.

# Requirements and limitations

The version of the web app described here has the following requirements about data and limitations:

- **Handling missing data:** visxhclust was not designed to be used as a tool for a complete cluster-based workflow, thus the expectation is that any dataset loaded into the tool will have already been cleaned and pre-processed. However, the app handles deviations, such as mixed numeric and text values or missing rows, by enabling users to include features with such cases as annotations, but not allowing them to be selected for clustering.
- **Dataset size:** the web app also has, by design, limitations on the size of datasets.The aim was to provide interpretable outputs and support fast iterations, meaning that datasets starting with more than a few thousands of rows will likely result in larger computation times and messy outputs; the same applies to features -- the design expectation is that users will do the appropriate reduction of dimensionality for such cases.
- **Guidance:** despite the inclusion of multiple help boxes, with external references to some of the packages mentioned above, the app is not an educational tool or include a step-by-step guidance on clustering; 

# Acknowledgements

This work was funded by the Health Data Research UK grant #####. We thank David Watson for helpful suggestions for the app and participants of the WHRI CompBio Code Review club for feedback on the package.
