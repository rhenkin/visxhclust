ui_pca <- function() {
  ns <- NS("pca")
  tabPanel("PCA",
    value = "pca",
    splitLayout(
      verticalLayout(
        fluidRow(
          column(6,selectizeInput(ns("pca_xaxis"),
                                  label = "X axis:", choices = NULL)),
          column(6,selectizeInput(ns("pca_yaxis"),
                                  label = "Y axis:", choices = NULL))),
        plotOutput(ns("pca_plot"), width = "100%", height = "400px")
      ),
      plotOutput(ns("drivers_plot"), width = "100%", height = "auto"),
      cellWidths = c("40%", "60%")
    ) %>% shinyhelper::helper(type = "markdown", content = "pca_help")
  )
}

server_pca <- function(id, selected_data, cluster_labels, cluster_colors) {
  moduleServer(id, function(input, output, session) {

    pca_data <- reactive({
      df <- selected_data()
      stats::prcomp(df)
    })

    observeEvent(pca_data(), {
      pca_res <- pca_data()
      pca_dims <- colnames(pca_res$x)
      updateSelectizeInput(session, "pca_xaxis",
                           choices = pca_dims, selected = "PC1")
      updateSelectizeInput(session, "pca_yaxis",
                           choices = pca_dims, selected = "PC2")
    })

    output$pca_plot <- renderPlot({
      req(input$pca_xaxis)
      req(input$pca_yaxis)
      pca_scatterplot(pca_data(), cluster_labels(),
                      cluster_colors, input$pca_xaxis, input$pca_yaxis)
    })

    output$drivers_plot <- renderPlot({
      drivers_df <- pca_drivers_df(selected_data(), pca_data())
      pca_driversplot(drivers_df)
    }, height = function() max(300, ncol(selected_data()) * 25))

  })
}
