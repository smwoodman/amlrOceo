#' Shiny module for OCEO header information
#'
#' Shiny module for OCEO header information
#'
#' @name mod_headers
#'
#' @param id character used to specify namespace, see \code{\link[shiny]{NS}}
#' @param col.width integer; column width of column of UI widgets
#'
#' @export
mod_headers_ui <- function(id, col.width = 7) {
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


#' @name mod_headers
#'
#' @param pool reactive; a DBI database connection pool
#' Intended to be the output of \code{\link[amlrDatabases]{mod_database_server}}
#'
#' @returns 
#' A list of reactive data frames from HEADER tables: 
#' 1) \code{amlr_station_header}:
#'  a reactive data frame with the contents of the AMLR_STATION_HEADER table, 
#'  with some columns renamed. The column names of this output data frame are:
#'  amlr_station_header_id, cruise_station, cruise, station, leg, ship, area, 
#'  bottom_depth, created_dt, ice, restrictedUse, gssmu
#' 2) \code{net_station_header}:
#'  a reactive data frame with the contents of the NET_STATION_HEADER table, 
#'  with some columns renamed. The column names of this output data frame are:
#'  tbd
#'
#' @export
mod_headers_server <- function(id, pool) {
  stopifnot(is.reactive(pool))
  
  moduleServer(
    id,
    function(input, output, session) {
      pasouq <- function(x) paste(sort(unique(x)), collapse = ", ")
      
      ### Get data from AMLR_STATION_HEADER table
      amlr_station_header <- reactive({
        amlr.station.header <- tbl(req(pool()), "AMLR_STATION_HEADER") %>% 
          collect() %>% 
          amlrOceo::oceo_amlr_station_header()
        
        amlr.station.header.names <- c(
          "amlr_station_header_id", "cruise_station", "cruise", "station", 
          "leg", "ship", "area", "bottom_depth", 
          "created_dt", "ice", "restrictedUse", "gssmu"
        )
        validate(
          need(identical(names(amlr.station.header), amlr.station.header.names), 
               "amlr_station_header names have changed - please report this as an issue")
        )
        
        amlr.station.header
      })
      
      ### Get data from NET_STATION_HEADER table
      net_station_header <- reactive({
        net.station.header <- tbl(req(pool()), "NET_STATION_HEADER") %>% 
          collect()  %>% 
          oceo_net_station_header()
        
        net.station.header.names <- c(
          "net_station_header_id", "amlr_station_header_id", 
          "time_local", "start_time_utc", "end_time_utc", 
          "acoustic_obs", "deck_obs", "sampling_comments", "date_tow", "water_depth", 
          "start_lat_deg", "start_lat_min", "start_lon_deg", "start_lon_min", 
          "end_lat_deg", "end_lat_min", "end_lon_deg", "end_lon_min", 
          "tow_valid", "net_comments", "created_dt", "tow_comments", 
          "old_start_time_utc", "old_end_time_utc"
        )
        validate(
          need(identical(names(net.station.header), net.station.header.names), 
               "net_station_header names have changed - please report this as an issue")
        )
        
        net.station.header
      })
      
      
      ### Summarize for display table
      output$tbl_info <- renderDT({
        amlr_station_header() %>% 
          group_by(Cruise = .data$cruise) %>% 
          summarise(`Ship(s)` = pasouq(.data$ship), 
                    `Area(s)` = pasouq(.data$area), 
                    `Leg(s)` = pasouq(.data$leg), 
                    `GSSMU(s)` = pasouq(.data$gssmu), 
                    `Number of stations` = length(.data$station))
      }, rownames = FALSE, options = list(scrollX = TRUE))
      
      # Download display table
      output$tbl_download <- downloadHandler(
        filename = function() {
          "cruise_info_table.csv"
        },
        content = function(file) {
          write.csv(cruise_info(), file = file, row.names = FALSE, na = "")
        }
      )
      
      ### Return list of reactives of header data frames
      list(
        amlr_station_header = amlr_station_header, 
        net_station_header = net_station_header
      )
    }
  )
}