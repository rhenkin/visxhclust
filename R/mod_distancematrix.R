ui_distancematrix <- function() {
  ns <- NS("distancematrix")
  tabPanel("Distance matrix",
           value = "distm",
           bs_accordion("distmap") %>%
             bs_append(
               "2D Projection",
               plotOutput(ns("dmat_projection"),
                          width = "400px",
                          height = "400px"
               ) %>% withSpinner()
             ) %>%
             bs_append(
               "Heatmap",
               verticalLayout(
                 plotOutput(ns("dmat_heatmap"),
                            width = "15cm",
                            height = "15cm"
                 ) %>% withSpinner()
               )
             ) %>% shinyhelper::helper(type = "markdown", content = "dmat_help")
  )
}

server_distancematrix <- function(id, dmat, cluster_labels, cluster_colors) {
  moduleServer(id, function(input, output, session) {
    output$dmat_projection <- renderCachedPlot({
      dmat_projection(dmat(), cluster_labels(), cluster_colors)
    }, cacheKeyExpr = list(dmat(), cluster_labels()))

    output$dmat_heatmap <- renderCachedPlot({
      dmat_heatmap(dmat())
    }, cacheKeyExpr = list(dmat()))
  })
}
