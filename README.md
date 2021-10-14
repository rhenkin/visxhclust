
<!-- README.md is generated from README.Rmd. Please edit that file -->

# visxhclust: visual exploration of hierarchical clustering

<!-- badges: start -->

[![R-CMD-check](https://github.com/rhenkin/visxhclust/workflows/R-CMD-check/badge.svg)](https://github.com/rhenkin/visxhclust/actions)
<!-- badges: end -->

visxhclust is a package that includes a Shiny application for **vis**ual
e**x**ploration of **h**ierarchical **clust**ering. It is aimed at
facilitating iterative workflows of hierarchical clustering on numeric
data. For that, the app allows users to quickly change parameters and
analyse and evaluate results with typical heatmaps with dendrograms and
other charts. Additionally, it includes lightweight data overview plots
such as correlation heatmaps, annotated MDS and PCA plots. On the
evaluation side, it builds on existing packages to compute internal
validation scores and Gap statistic, as well as Dunn’s test to evaluate
significant differences between clusters.

The app includes multiple help points in the interface and a vignette
documenting how to load data and the basic clustering loop – see
`vignette("visxhclust")`. The package exports various functions to help
with documenting and reproducing a clustering workflow with R or R
Markdown – see `vignette("clusterworkflow")` and
`vignette("clusterevaluation")` for examples.

## Installation

The latest release can be installed from CRAN:

``` r
install.packages("visxhclust")
```

The latest development version can be installed from GitHub:

``` r
remotes::install_github("rhenkin/visxhclust")
```

Most dependencies are found in CRAN. However, the heatmap drawing
package is part of [Bioconductor](http://www.bioconductor.org/) and may
require a separate installation:

``` r
install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
```

## Usage and data requirements

To use your data with the tool, you can save a data frame or tibble in
an RDS file, or use comma or tab-delimited files, with .csv, .tsv or
.txt extensions. The clustering method supported by the tool works only
on numeric values; columns containing text will be set aside to annotate
the heatmap if so desired. If a column named `ID` exists, it will be
used as an internal identifier for rows.

Clustering requires complete datasets with no missing values, NULLs or
NAs. If any column contains missing values, it will be set aside to be
used as a heatmap annotation. Badly formatted data will also lead to
unexpected results in the tool. As an alternative, imputation packages
can be used to fill missing data and faulty rows (e.g. text in numeric
columns) should be removed before loading the file into the tool. The
tool provides limited abilities to help with diagnosing issues and
preprocessing data.

To run the app once the package is installed:

``` r
library(visxhclust)
# Increases max file size to 30 MB
options(shiny.maxRequestSize = 30*1024^2)
run_app()
```
