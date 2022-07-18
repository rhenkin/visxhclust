
<!-- README.md is generated from README.Rmd. Please edit that file -->

# visxhclust: visual exploration of hierarchical clustering

<!-- badges: start -->

[![R-CMD-check](https://github.com/rhenkin/visxhclust/workflows/R-CMD-check/badge.svg)](https://github.com/rhenkin/visxhclust/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/visxhclust)](https://CRAN.R-project.org/package=visxhclust)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.04074/status.svg)](https://doi.org/10.21105/joss.04074)
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
significant differences between clusters. Many of the functions are also
exported to facilitate documenting a complete analysis cycle.

**NEW!** A live demo of the app is running
[here](https://rhenkin.shinyapps.io/visxhclust).

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

## Getting started

To run the app once the package is installed, use the following
commands:

``` r
library(visxhclust)
# Increases max file size to 30 MB
options(shiny.maxRequestSize = 30*1024^2)
run_app()
```

The app includes multiple help points in the interface (look for the
question marks), and there are also three guides on how to use tool:

-   An [animated
    guide](https://rhenkin.github.io/visxhclust/articles/visxhclust.html)
    on loading data and the basic clustering loop. It’s also accessible
    in R by using the command `vignette("visxhclust")`.
-   An example of how to [reproduce an
    analysis](https://rhenkin.github.io/visxhclust/articles/clusterworkflow.html)
    an analysis using the functions exported by the package. See with
    `vignette("clusterworkflow")` in R.
-   An example of how to [reproduce the evaluation
    workflow](https://rhenkin.github.io/visxhclust/articles/clusterevaluation.html)
    using the functions exported by the package. See with
    `vignette("clusterevaluation")` in R.

## Usage tips and data requirements

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

# Contributing

Please see the
[guide](https://github.com/rhenkin/visxhclust/blob/master/CONTRIBUTING.md)
for code contribution and suggestions.
