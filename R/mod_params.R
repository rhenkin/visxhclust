ui_params <- function() {
  ns <- NS("params")
  tabPanel(
    "Parameters",
    value = "parameters",
    verticalLayout(
      fluidRow(
        column(2, style = "margin-top: 25px; margin-bottom: 25px",
               actionButton(ns("configs_save"), "Save parameters")),
        column(2, style = "margin-top: 25px; margin-bottom: 25px",
               actionButton(ns("configs_load"), "Load selected"))
      ) %>% shinyhelper::helper(type = "markdown", content = "params_help"),
      fluidRow(DT::DTOutput(ns("configs_table")))
    )
  )
}

server_params <- function(id, distance_method, linkage_method, nclusters) {
  moduleServer(id, function(input, output, session) {

    saved_configurations <- reactiveVal(list())

    ## Render the configuration table
    output$configs_table <- DT::renderDT({
      validate(need(length(saved_configurations()) > 0, "No states saved"))
      do.call(rbind, saved_configurations())
    },
    rownames = TRUE,
    caption = "Click on a row to select parameters to load them again",
    selection = "single",
    options = list(ordering = FALSE))

    # State saving
    # Save current key parameters to table.
    # Currently doesn't save the correlation threshold nor selected variables
    observeEvent(input$configs_save, {
      idx <- length(saved_configurations()) + 1
      current_config <- list(
        distance_method = distance_method(),
        linkage_method = linkage_method(),
        nclusters = nclusters()
      )
      config_list <- saved_configurations()
      config_list[[idx]] <- current_config
      saved_configurations(config_list)
    })

    # State loading
    # Reload parameters from the selected row on the table
    selected_config <- eventReactive(input$configs_load, {
      req(input$configs_table_rows_selected)
      saved_configurations()[[input$configs_table_rows_selected]]
    })
    selected_config
  })
}
