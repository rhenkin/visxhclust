
<!-- README.md is generated from README.Rmd. Please edit that file -->

# visxhclust: visual exploration of hierarchical clustering

<!-- badges: start -->
<!-- badges: end -->

visxhclust is a package that consists mainly of a Shiny application for
**vis**ual e**x**ploration of **h**ierarchical **clust**ering. It
implements hierarchical clustering for numeric data and includes various
plots and interactions to facilitate iterative workflows. Features
include:

-   Selection of variables supported by a correlation-driven t-test
-   Visual inspection of MDS and PCA projections
-   Visualisation of results with dendrogram, heatmap and boxplots
-   Validation by significance testing, internal validation scores and
    indices and computation of Gap statistic
-   Comparison of multiple clustering results with Sankey plots
-   Saving and loading of settings and results

The app includes multiple help points in the interface, and the package
additionally exports various functions to help with documenting and
reproducing a clustering workflow in R Markdown – check
`vignette("clusterworkflow")` for an example.

## Installation

The package can be installed from github using:

``` r
remotes::install_github("rhenkin/visxchlust")
```

Most packages are from CRAN. However, the heatmap drawing package is
part of [Bioconductor](http://www.bioconductor.org/) and may require a
separate installation:

``` r
install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
```

## Requirements and usage

To use your data with the tool, you can save a data frame or tibble in
an RDS file, or use comma or tab-delimited files, with .csv, .tsv or
.txt extensions. The clustering in the tool is done on numeric values;
columns containing text will be listed under possible heatmap
annotations. If an ID column exists, it will be used as an internal
identifier for rows, as the app support removing rows through the table
view.

Clustering requires complete datasets with no empty values, NULLs or
NAs. If any column contains missing values, it will be set aside to be
used as a heatmap annotation. Badly formatted data will also lead to
unexpected results in the tool. Imputation packages can be used to fill
missing data and faulty rows should be removed before loading the file
into the tool. The tool is not intended to help with this stage of data
processing.

To run the app once the package is installed:

``` r
library(visxhclust)
run_app()
```