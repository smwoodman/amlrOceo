#' Modules for the different tabs of the amlrOceo Shiny app
#'
#' Modules for the different tabs of the amlrOceo Shiny app
#'
#' @name shiny_modules
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param pool reactive; a DBI database connection pool. 
#' Intended to be the output of \code{\link[amlrDatabases]{mod_database_server}}
#' @param amlr.station.header reactive; the output of \code{\link{mod_cruise_info_server}}
#'
NULL
