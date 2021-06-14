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
          column(
            width = 5, 
            uiOutput(ns("cruise_uiOut_selectize")), 
            uiOutput(ns("leg_uiOut_selectize")), 
            uiOutput(ns("area_uiOut_selectize"))
          ), 
          column(
            width = 5, 
            uiOutput(ns("ice_uiOut_selectize")), 
            uiOutput(ns("net_type_uiOut_selectize")), 
            uiOutput(ns("time_uiOut_selectize"))
          ),
          column(2, uiOutput(ns("cruise_station_exclude_uiOut_selectize")))
        ), 
        fluidRow(
          column(
            width = 6, 
            sliderInput(ns("depth_fished"), tags$h5("Depths fished, to include"), 
                        min = 0, max = 750, value = c(0, 180), step = 1), 
            actionButton(ns("depth_shallow"), "Set depth fished to 0-180"), 
            actionButton(ns("depth_deep"), "Set depth fished to 181-bottom")
          ), 
          column(6, sliderInput(ns("krill_count"), tags$h5("Krill counts, to include"), 
                                min = 0, max = 100000000, value = c(0, 100000000), step = 1))
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
mod_krill_length_frequency_server <- function(id, pool, headers.list, net) {
  stopifnot(
    is.reactive(pool),
    is.list(headers.list),
    is.reactive(net)
  )
  
  moduleServer(
    id,
    function(input, output, session) {
      amlr.station.header <- reactive(headers.list$amlr_station_header())
      net.station.header <- reactive(headers.list$net_station_header())
      
      
      #########################################################################
      ### Reactive inputs - for filters
      output$cruise_uiOut_selectize <- renderUI({
        header.cruise <- sort(unique(amlr.station.header()$cruise))
        selectInput(
          session$ns("cruise"), tags$h5("Cruise(s) to include"),
          choices = header.cruise, selected = header.cruise, 
          multiple = TRUE, selectize = TRUE
        )
      })
      
      output$leg_uiOut_selectize <- renderUI({
        header.leg <- sort(unique(amlr.station.header()$leg))
        selectInput(
          session$ns("leg"), tags$h5("Cruise leg(s) to include"),
          choices = header.leg, selected = header.leg, 
          multiple = TRUE, selectize = TRUE
        )
      })
      
      output$area_uiOut_selectize <- renderUI({
        header.area <- sort(unique(amlr.station.header()$area))
        selectInput(
          session$ns("area"), tags$h5("AMLR area(s) to include"),
          choices = header.area, selected = header.area, 
          multiple = TRUE, selectize = TRUE
        )
      })
      
      output$ice_uiOut_selectize <- renderUI({
        header.ice <- sort(unique(amlr.station.header()$ice))
        selectInput(
          session$ns("ice"), tags$h5("Ice type(s) to include"),
          choices = header.ice, selected = header.ice, 
          multiple = TRUE, selectize = TRUE
        )
      })
      
      output$net_type_uiOut_selectize <- renderUI({
        header.net.type <- sort(unique(net()$net_type))
        selectInput(
          session$ns("net_type"), tags$h5("Net type(s) to include"),
          choices = header.net.type, selected = header.net.type, 
          multiple = TRUE, selectize = TRUE
        )
      })
      
      output$time_uiOut_selectize <- renderUI({
        header.time <- sort(unique(net.station.header()$time_local))
        selectInput(
          session$ns("time"), tags$h5("Time(s) of day to include"),
          choices = header.time, selected = header.time, 
          multiple = TRUE, selectize = TRUE
        )
      })
      
      output$cruise_station_exclude_uiOut_selectize <- renderUI({
        header.cruise.station <- sort(unique(amlr.station.header()$cruise_station))
        selectInput(
          session$ns("cruise_station_exclude"), tags$h5("Cruise + stations to EXCLUDE"),
          choices = header.cruise.station, selected = NULL, 
          multiple = TRUE, selectize = TRUE
        )
      })
      
      #########################################################################
      ### Processing
      krill_lf <- reactive({
        req(input$cruise, input$leg, input$area, input$ice, input$net_type, input$time)
        
        
        x.orig <- amlr.station.header() %>% 
          filter(cruise %in% input$cruise, 
                 leg %in% input$leg, 
                 area %in% input$area, 
                 ice %in% input$ice) %>% 
          left_join(net.station.header(), by = "amlr_station_header_id") %>% 
          filter(time_local %in% input$time) %>% 
          left_join(net(), by = "net_station_header_id") %>% 
          filter(net_type %in% input$net_type, 
                 between(depth_fished, input$depth_fished[1], input$depth_fished[2])) 
        
        browser()
        x <- x.orig %>% 
          select(amlr_station_header_id, cruise_station, cruise, station, 
                 leg, ship, area, bottom_depth, 
                 net_station_header_id, time_local, tow_valid, 
                 net_id, net_type, final_reading, initial_reading, 
                 tucker_header_data_id, is_tucker)
      })
      
      
      #########################################################################
      ### Output plot
      plot_output <- reactive({
        req(NULL)
      })
      
      
      ### Output table
      tbl_output <- reactive({
        krill_lf()
      })
      
      
      ### Send to output module
      observe(mod_output_server("output", id, tbl_output, plot_output))
    }
  )
}