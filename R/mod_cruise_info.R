#' Shiny module for OCEO cruise information
#'
#' Shiny module for OCEO cruise information
#'
#' @name mod_cruise_info
#'
#' @param id character used to specify namespace, see \code{\link[shiny]{NS}}
#' @param col.width integer; column width of column of UI widgets
#'
#' @export
mod_cruise_info_ui <- function(id, col.width = 7) {
  ns <- NS(id)
  
  # assemble UI elements
  tagList(
    box(
      title = "Cruise information", status = "warning", width = col.width, collapsible = TRUE,
      DTOutput(ns("tbl_info")),
      tags$br(), 
      downloadButton(ns("tbl_download"), "Download table as CSV")
    )
  )
}


#' @name mod_cruise_info
#'
#' @param pool reactive; a DBI database connection pool
#' Intended to be the output of \code{\link[amlrDatabases]{mod_database_server}}
#'
#' @returns 
#' A data frame with the contents of the AMLR_STATION_HEADER table, 
#' with some columns renamed
#'
#' @export
mod_cruise_info_server <- function(id, pool) {
  stopifnot(is.reactive(pool))
  
  moduleServer(
    id,
    function(input, output, session) {
      pasouq <- function(x) paste(sort(unique(x)), collapse = ", ")
      
      # Get data from table
      amlr_station_header <- reactive({
        x <- tbl(req(pool()), "AMLR_STATION_HEADER") %>% 
          collect() %>% 
          # mutate(tmp = paste(amlr_cruise, Station, sep = "_"))
          # waldo::compare(x$amlr_station, y$tmp) #ok
          rename(amlr_station_header_id = ID, cruise_station = amlr_station, 
                 cruise = amlr_cruise, station = Station, ship = Ship, 
                 area = amlr_area)
        
        x.names <- c(
          "amlr_station_header_id", "cruise_station", "cruise", "station", 
          "leg", "ship", "area", "bottom_depth", 
          "created_dt", "ice_status", "restrictedUse", "gssmu"
        )
        validate(
          need(identical(names(x), x.names), 
               "amlr_station_header names have changed - please contact Sam")
        )
        
        x
      })
      
      # Summarize for display table
      cruise_info <- reactive({
        amlr_station_header() %>% 
          group_by(Cruise = .data$cruise) %>% 
          summarise(`Ship(s)` = pasouq(.data$ship), 
                    `Area(s)` = pasouq(.data$area), 
                    `Leg(s)` = pasouq(.data$leg), 
                    `GSSMU(s)` = pasouq(.data$gssmu), 
                    `Number of stations` = length(.data$station))
      })
      
      # Season info display table
      output$tbl_info <- renderDT(cruise_info(), options = list(scrollX = TRUE))
      
      # Download table
      output$tbl_download <- downloadHandler(
        filename = function() {
          "cruise_info_table.csv"
        },
        content = function(file) {
          write.csv(cruise_info(), file = file, row.names = FALSE, na = "")
        }
      )
      
      ### Return reactive for amlr station header data frame
      return(amlr_station_header)
    }
  )
}