#' @name shiny_modules
#' @export
mod_krill_length_frequency_ui <- function(id) {
  ns <- NS(id)
  
  # assemble UI elements
  tagList(
    fluidRow(
      box(
        title = "Filters", status = "warning", width = 12, collapsible = TRUE,
        fluidRow(
          column(4, uiOutput(ns("cruise_uiOut_selectize"))),
          column(
            width = 4, 
            uiOutput(ns("leg_uiOut_selectize")), 
            uiOutput(ns("area_uiOut_selectize"))
          )
        )
      )
      # box(
      #   title = "Next steps...", status = "warning", width = 7, collapsible = TRUE,
      #   fluidRow(
      #     h5("todo")
      #   )
      # )
    ),
    mod_output_ui(ns("output"))
  )
}



#' @name shiny_modules
#' @export
mod_krill_length_frequency_server <- function(id, pool, amlr.station.header) {
  stopifnot(
    is.reactive(pool)
  )
  
  moduleServer(
    id,
    function(input, output, session) {
      #########################################################################
      ### Reactive inputs - for filters
      header.cruise <- reactive(sort(unique(amlr.station.header()$cruise)))
      header.leg <- reactive(sort(unique(amlr.station.header()$leg)))
      header.area <- reactive(sort(unique(amlr.station.header()$area)))
      
      output$cruise_uiOut_selectize <- renderUI({
        selectInput(
          session$ns("cruise"), tags$h5("Cruise(s) to include"),
          choices = header.cruise(), selected = header.cruise(), 
          multiple = TRUE, selectize = TRUE
        )
      })
      
      output$leg_uiOut_selectize <- renderUI({
        selectInput(
          session$ns("leg"), tags$h5("Cruise leg(s) to include"),
          choices = header.leg(), selected = header.leg(), 
          multiple = TRUE, selectize = TRUE
        )
      })
      
      output$area_uiOut_selectize <- renderUI({
        selectInput(
          session$ns("area"), tags$h5("AMLR area(s) to include"),
          choices = header.area(), selected = header.area(), 
          multiple = TRUE, selectize = TRUE
        )
      })
      
      #########################################################################
      ### Output plot
      plot_output <- reactive({
        req(NULL)
      })
      
      
      ### Output table
      tbl_output <- reactive({
        data.frame(1:10)
      })
      
      
      ### Send to output module
      observe(mod_output_server("output", id, tbl_output, plot_output))
    }
  )
}