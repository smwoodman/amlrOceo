#' Calculate filtered volumes for nets
#' 
#' Recreates vCalculate_Filtered_Volume
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
#'   calculate_filtered_volume(
#'     net, net.station.header, amlr.station.header, tucker.header.data
#'   )
#' }
#' 
#' @export
calculate_filtered_volume <- function(net, net.station.header, amlr.station.header, 
                                      tucker.header.data) {
  stopifnot(
    all(vapply(list(net, net.station.header, amlr.station.header, tucker.header.data), 
               inherits, FUN.VALUE = as.logical(1), "data.frame"))
    # TODO: check for net types after fixing blank one
  )
  
  net %>% 
    rename(net_id = NET_ID) %>% 
    left_join(net.station.header %>% 
                select(net_station_header_id = ID, amlr_station_header_id), 
              by = "net_station_header_id") %>% 
    left_join(amlr.station.header %>% 
                select(amlr_station_header_id = ID, amlr_cruise, amlr_station), 
              by = "amlr_station_header_id") %>% 
    left_join(tucker.header.data %>% 
                select(tucker_header_data_id = ID, tot_dist, Maximum_Depth, Swept_volume), 
              #, tucker_rec_num2 = tucker_rec_num), 
              by = "tucker_header_data_id") %>% 
    # waldo::compare(net.proc$tucker_rec_num, net.proc$tucker_rec_num2)
    filter(net_type != "T" | !is.na(tucker_header_data_id), 
           net_type %in% c("I", "T", "B", "VRD", "VRS")) %>% 
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
               if_else(amlr_cruise == "AMLR2011", tot_dist * 4, Swept_volume), 
             TRUE ~ NA_real_
           ), 
           depth_fished_isnull = if_else(
             net_type == "T", FALSE, replace_na(depth_fished, 0) == 0
             ), 
           depth_fished = if_else( # must happen after depth_fished_isnull
             net_type == "T", Maximum_Depth, 
             if_else(replace_na(depth_fished, 0) == 0, 
                     as.integer(170), depth_fished))) %>% 
    select(net_id, net_station_header_id, 
           amlr_cruise, amlr_station, amlr_station_header_id,
           net_type, net, volume, volume_isnull, depth_fished, depth_fished_isnull) %>%
    arrange(net_id)
}
