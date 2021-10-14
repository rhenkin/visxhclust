ui_overview <- function() {
  ns <- NS("overview")
  tabPanel("Data overview", value = "overview",
           bs_accordion("overview") %>%
             bs_append(
               "Correlation heatmap for selected variables",
               plotOutput(
                 ns("corr_heatmap"),
                 width = "650px", height = "600px")
             ) %>%
             bs_append(
               "Highly correlated variables",
               DT::DTOutput(ns("dropped_vars_table"))) %>%
             bs_append(
               "Raw data histogram",
               verticalLayout(
                 selectInput(
                   ns("overview_var"),
                   "Choose variable",
                   choices = NULL),
                 checkboxInput(
                   ns("overview_scale"), "Scale?"),
                 plotOutput(
                   ns("overview_hist"),
                   width = "400px", height = "400px")
               )
             ) %>% shinyhelper::helper(type = "markdown",
                                       content = "overview_help")
  )
}
server_overview <- function(id, selected_data, selected_numeric, dropped_variables,
                         apply_scaling) {
  moduleServer(id, function(input, output, session) {
    # Correlation heatmap of the selected numeric variables
    output$corr_heatmap <- renderCachedPlot({
      df <- selected_data()
      correlation_heatmap(df)
    }, cacheKeyExpr = list(selected_data()))

    # Table with correlation information of dropped variables
    output$dropped_vars_table <- DT::renderDT({
      validate(need(!is.null(dropped_variables()),
                    "No variables were removed due to strong correlation."))
      dropped_variables()
    })

    # Update overview variable list
    observeEvent(selected_numeric(), {
      updateSelectInput(session,
                        "overview_var",
                        choices = selected_numeric())
    })

    # Histograms for selected variables
    output$overview_hist <- renderPlot({
      df <- selected_data()
      validate(need(input$overview_var %in% colnames(df),
                    "Variable does not exist"))
      df <- df[, input$overview_var]
      if (input$overview_scale == TRUE) {
        df <- as.data.frame(scale_data(df, apply_scaling()))
      }
      ggplot(df, aes_string(input$overview_var)) +
        geom_histogram(bins = 15)
    })

  })
}
