# Gap statistic computation module

ui_gapstat <- function() {
  ns <- NS("gapstat")
  tabPanel("Gap stat",
           value = "gapstat",
           verticalLayout(fluidRow(
             column(
               3,
               numericInput(
                 ns("gap_B"),
                 "Bootstrap samples:",
                 value = 500,
                 min = 50,
                 step = 50
               )
             ),
             column(
               2,
               actionButton(
                 ns("compute_gap"),
                 "Compute",
                 style = "margin-top: 25px")
             ),
             column(
               4,
               selectizeInput(
                 ns("gap_method"),
                 "Optimal k method:",
                 choices = c(
                   "firstSEmax",
                   "Tibs2001SEmax",
                   "globalSEmax",
                   "firstmax",
                   "globalmax"
                 )
               )
             )
           ) %>% shinyhelper::helper(type = "markdown", content = "gap_help"),
           plotOutput(ns("gap_plot")) %>% withSpinner()
           )
  )
}
server_gapstat <- function(id, selected_data, clusters) {
  moduleServer(id, function(input, output, session) {
    # Wait for user input instead of computing automatically
    gap_result <- eventReactive(input$compute_gap, {
      isolate({
        df <- selected_data()
        clustered_data <- clusters()
      })
      validate(need(input$gap_B > 0,
                    "Bootstrap samples must be greater than 0."))
      compute_gapstat(df, clustered_data, input$gap_B)
    })

    output$gap_plot <- renderPlot({
      req(gap_result())

      gap_table <- gap_result()
      if (isTruthy(input$gap_method)) {
        optimal_k <- cluster::maxSE(gap_table$gap,
                                    gap_table$SE.sim,
                                    method = input$gap_method)
      } else {
        optimal_k <- NULL
      }

      line_plot(gap_table, "k", "gap", xintercept = optimal_k) +
        labs(y = "Gap statistic")
    })
  })
}

