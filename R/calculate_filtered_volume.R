#' Calculate filtered volumes for nets
#' 
#' Recreates vCalculate_Filtered_Volume
#' 
#' @name calculate_volume_filtered
#' 
#' @param x a data frame the necessary columns; see Details
#' 
#' @details 
#' calculate_filtered_volume takes a joined data frame as the input
#' calculate_filtered_volume_tbl takes the table data frames as inputs
#' 
#' @return A data frame...
#' 
#' @examples 
#' calculate_filtered_volume(data.frame(
#'   net_id = 1:7, net_station_header_id = 1, amlr_station_header_id = 1,
#'   cruise = "AMLR_test", station = "St1", 
#'   net_type = c("I", "I", "B", "B", "T", "T", "VRS"),
#'   net_num = 1:7, 
#'   final_reading = rep(1000, 7), 
#'   initial_reading = rep(1, 7),
#'   depth_fished = as.integer(rep(170, 7)), 
#'   swept_volume = c(NA, NA, NA, NA, 50, 50, NA), 
#'   tot_dist = c(NA, NA, NA, NA, 1000, 1000, NA), 
#'   max_depth = as.integer(c(NA, NA, NA, NA, 179, 179, NA))
#' ))
#' 
#' @export
calculate_filtered_volume <- function(x) {
  names.x <- c("net_id", "net_station_header_id", "amlr_station_header_id",
               "cruise", "station", "net_num", 
               "net_type", "final_reading", "initial_reading", "depth_fished")
  names.x.tucker <- c("swept_volume", "tot_dist", "max_depth")
  stopifnot(
    inherits(x, "data.frame"),
    all(names.x %in% names(x)), 
    # TODO: check for net types after fixing blank one
    if_else("T" %in% x$net_type, all(names.x.tucker %in% names(x)), TRUE)
  )
  
  # x.orig <- x
  if (any(x$net_type == "T" & is.na(x$tucker_header_data_id))) {
    x.tucker.issue <- x %>% filter(net_type == "T" & is.na(tucker_header_data_id))
    warning("NET records with the following net IDs were removed because ", 
            "they had a net_type value of 'T', but a NULL tucker_header_data_id value:\n", 
            paste(sort(x.tucker.issue$net_id), collapse = ", "))
    x <- x %>% filter(!(net_type == "T" & is.na(tucker_header_data_id)))
  }
  
  x %>% 
    filter(net_type %in% c("I", "T", "B", "VRD", "VRS")) %>%  #TODO: remove
    mutate(reading_na = final_reading == 0 | is.na(final_reading) | is.na(initial_reading), 
           volume_isnull = replace_na(
             if_else(net_type == "T", FALSE, 
                     replace_na(final_reading, 0) - replace_na(initial_reading, 0) == 0), 
             TRUE
           ), 
           volume = case_when(
             net_type == "I" ~ 
               if_else(reading_na, 3850, (final_reading - initial_reading) * 0.0752),
             net_type == "B" ~ 
               if_else(reading_na, 389, (final_reading - initial_reading) * 0.00758), 
             net_type %in% c("VRD", "VRS") ~ 
               if_else(reading_na, 133.5, (final_reading - initial_reading) * 0.00664), 
             net_type == "T" ~ 
               if_else(cruise == "AMLR2011", tot_dist * 4, swept_volume), 
             TRUE ~ NA_real_
           ), 
           depth_fished_isnull = if_else(
             net_type == "T", FALSE, replace_na(depth_fished, 0) == 0
           ), 
           depth_fished = if_else( # must happen after depth_fished_isnull
             net_type == "T", max_depth, 
             if_else(replace_na(depth_fished, 0) == 0, 
                     as.integer(170), depth_fished))) %>% 
    select(net_id, net_station_header_id, amlr_station_header_id,
           cruise, station, net_type, net_num, 
           volume, volume_isnull, depth_fished, depth_fished_isnull) %>%
    arrange(net_id)
}



#' @name calculate_volume_filtered
#' 
#' @param net data frame; 
#' @param net.station.header data frame; 
#' @param amlr.station.header data frame; 
#' @param tucker.header.data data frame; 
#' 
#' @examples 
#' \dontrun{
#'   calculate_filtered_volume_tbl(
#'     oceo_net(net), 
#'     oceo_net_station_header(net.station.header), 
#'     oceo_amlr_station_header(amlr.station.header), 
#'     oceo_tucker_header_data(tucker.header.data)
#'   )
#' }
#' 
#' @export
calculate_filtered_volume_tbl <- function(net, net.station.header, amlr.station.header, 
                                          tucker.header.data) {
  stopifnot(
    all(vapply(list(net, net.station.header, amlr.station.header, tucker.header.data), 
               inherits, FUN.VALUE = as.logical(1), "data.frame"))
  )
  
  joined.df <- net %>% 
    left_join(net.station.header %>% 
                select(net_station_header_id, amlr_station_header_id), 
              by = "net_station_header_id") %>% 
    left_join(amlr.station.header %>% 
                select(amlr_station_header_id, cruise, station), 
              by = "amlr_station_header_id") %>% 
    left_join(tucker.header.data %>% 
                select(tucker_header_data_id, tot_dist, max_depth, swept_volume), 
              by = "tucker_header_data_id")
  
  
  calculate_filtered_volume(joined.df)
}



