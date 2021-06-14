#' Modules for the different tabs of the amlrOceo Shiny app
#'
#' Modules for the different tabs of the amlrOceo Shiny app
#'
#' @name shiny_modules
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param pool reactive; a DBI database connection pool. 
#'   Intended to be the output of \code{\link[amlrDatabases]{mod_database_server}}
#' @param headers.list list of reactive data frames from OCEO header tables; 
#'   the output of \code{\link{mod_headers_server}}
#' @param net reactive data frame of the NET table from AMLR_OCEO
#'
NULL
