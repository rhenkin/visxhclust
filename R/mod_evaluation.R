ui_evaluation <- function() {
  ns <- NS("evaluation")
  tabPanel(
    "Evaluation",
    value = "measures",
    verticalLayout(
      fluidRow(
        column(width = 3,
        selectizeInput(
          ns("evaluation_selected"),
          label = "Select measure",
          choices = c("silhouette", "dunn")
        )),
        column(width = 4,
        selectizeInput(
          ns("evaluation_highlight"),
          "Highlight number of clusters (k):",
          choices = c(
            "First minimum" = "firstmin",
            "Global minimum" = "globalmin",
            "First maximum" = "firstmax",
            "Global maximum" = "globalmax"
          )
        ))) %>%
        shinyhelper::helper(type = "markdown", content = "measures_help"),
      plotOutput(ns("measure_over_k"))
    )
  )
}

server_evaluation <- function(id, distance_matrix, clusters) {
  moduleServer(id, function(input, output, session) {
    output$measure_over_k <- renderPlot({
      req(input$evaluation_selected)

      metric_results <- compute_metric(distance_matrix(),
                                       clusters(),
                                       input$evaluation_selected)
      validate(need(all(!is.na(metric_results)),
                    "Metric not evaluated for current data"))
      optimal_k <- optimal_score(metric_results$score,
                                 method = input$evaluation_highlight)

      line_plot(metric_results, "k", "score", optimal_k)
    })
  })
}

