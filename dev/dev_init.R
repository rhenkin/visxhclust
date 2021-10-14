# Basics
# Commands that are run once

# usethis::use_gpl3_license(name = "Rafael Henkin")
# usethis::use_readme_rmd( open = FALSE )
# usethis::use_news_md( open = FALSE )
#
# usethis::use_pkgdown()
#
# usethis::use_package("shiny")
# usethis::use_package("bsplus")
# usethis::use_package("shinycssloaders")
# usethis::use_package("DT")
#
# usethis::use_package("dplyr")
# usethis::use_package("tidyr")
# usethis::use_package("tidyselect")
# #usethis::use_package("readr")
#
# usethis::use_package("cluster")
# usethis::use_package("fastcluster")
# usethis::use_package("clusterCrit")
#
# usethis::use_package("RColorBrewer")
# usethis::use_package("ggplot2")
#
# usethis::use_package("dunn.test")
#
# usethis::use_package("shinyhelper")

# usethis::use_package("dendextend")
# usethis::use_package("circlize")
# usethis::use_package("patchwork")
# Bioconductor packages:
# usethis::use_package("ComplexHeatmap")


# Vignettes
#usethis::use_vignette("clusterworkflow")
#usethis::use_vignette("clusterevaluation")
#usethis::use_vignette("visxhclust")

#usethis::use_testthat()
#usethis::use_github_actions()

devtools::document()
pkgdown::build_site()
devtools::load_all(".")
run_app()
