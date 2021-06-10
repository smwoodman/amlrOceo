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
      title = "Cruise information", status = "warning", solidHeader = FALSE, width = col.width, collapsible = TRUE,
      tableOutput(ns("tbl_info")),
      downloadButton(ns("tbl_download"), "Download table as CSV")
    )
  )
}


#' @name mod_cruise_info
#'
#' @param pool reactive; a DBI database connection pool, intended to be the output of \code{\link{mod_database_server}}
#'
#' @returns
#' \code{mod_season_server} returns a list of ...:
#' 1) \code{season.df}, the season information data frame and
#' 2) \code{season.id.list}, a list of the ID values from the season information table,
#' with the 'season_name' values as names
#'
#' @export
mod_cruise_info_server <- function(id, pool) {
  stopifnot(is.reactive(pool))
  
  moduleServer(
    id,
    function(input, output, session) {
      pasouq <- function(x) paste(sort(unique(x)), collapse = ", ")
      
      # Get data from table
      cruise_info <- reactive({
        tbl(req(pool()), "AMLR_STATION_HEADER") %>% 
          collect() %>% 
          group_by(`AMLR Cruise` = .data$amlr_cruise) %>% 
          summarise(`Ship(s)` = pasouq(.data$Ship), 
                    `Area(s)` = pasouq(.data$amlr_area), 
                    `Leg(s)` = pasouq(.data$leg), 
                    `GSSMU(s)` = pasouq(.data$gssmu), 
                    `Number of stations` = length(.data$Station))
      })
      
      # Season info display table
      output$tbl_info <- renderTable(cruise_info())
      
      # Download table
      output$tbl_download <- downloadHandler(
        filename = function() {
          "cruise_info_table.csv"
        },
        content = function(file) {
          write.csv(cruise_info(), file = file, row.names = FALSE, na = "")
        }
      )
      
      ### Return values
      list()
      # list(
      #   season.df = season_info,
      #   season.id.list = reactive(set_names(as.list(season_info()$ID), season_info()$season_name))
      # )
    }
  )
}


##############################################################################


##############################################################################
