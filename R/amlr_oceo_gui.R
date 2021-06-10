#' Open amlrOceo Shiny app
#'
#' Open the amlrOceo R Shiny application
#'
#' @param launch.browser Logical with default of \code{TRUE};
#'   passed to \code{launch.browser} argument of \code{\link[shiny]{runApp}}
#'
#' @examples
#' if (interactive()) amlr_oceo_gui(launch.browser = TRUE)
#'
#' @seealso \url{https://www.fisheries.noaa.gov/about/antarctic-ecosystem-research-division-southwest-fisheries-science-center}
#'
#' @export
amlr_oceo_gui <- function(launch.browser = TRUE) {
  appDir <- system.file("shiny", package = "amlrOceo")
  if (appDir == "") {
    stop("There was an error opening the Shiny app; try re-installing the 'amlrOceo' package",
         call. = FALSE)
  }
  shiny::runApp(appDir, launch.browser = launch.browser, display.mode = "normal")
}
