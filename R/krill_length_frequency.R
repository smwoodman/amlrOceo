#' Calculate krill length frequencies
#' 
#' Calculate krill length frequencies from provided data
#' 
#' @param net data frame; 
#' @param net.station.header data frame; 
#' @param amlr.station.header data frame; 
#' @param tucker.header.data data frame; 
#' 
#' @details stuff
#' 
#' @return A data frame...
#' 
#' @examples 
#' \dontrun{
#'   krill_length_frequency(
#'     net, net.station.header, amlr.station.header, tucker.header.data
#'   )
#' }
#' 
#' @export
krill_length_frequency <- function(net, net.station.header, amlr.station.header, 
                                   tucker.header.data = NULL) {
  stopifnot(
    all(vapply(list(net, net.station.header, amlr.station.header, tucker.header.data), 
               inherits, FUN.VALUE = as.logical(1), "data.frame"))
    # TODO: check for net types after fixing blank one
  )
  
  # Check that if tucker nets, tucker.header.data is not null
  
}
