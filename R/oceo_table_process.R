#' Renaming functions
#' 
#' Functions that rename columns in data frames extracted from data frames int eh AMLR_OCEO database
#' 
#' @name oceo_table_process
#' 
#' @param x data frame, a table from the AMLR_OCEO database
#' 
#' @details 
#' The AMLR_OCEO database has a number of views, stored procedures, and other items 
#' that make it difficult to change the names of fields in the table.
#' Thus, the purpose of these functions is to have a single place where the 
#' columns are renamed to something more consistent and R-friendly.
#' These functions also make it so that only one place needs to be changed
#' if the table column names do change.
#' 
#' @returns The data frame \code{x}, with columns processed as described below:
#' 1) \code{oceo_amlr_station_header}. Output column names:
#' amlr_station_header_id, cruise_station, cruise, station, leg, ship, area, 
#' bottom_depth, created_dt, ice, restrictedUse, gssmu
#' 2) \code{oce_net_station_header}. 
#' time_local is converted to a factor, and date_tow is converted to a Date object. 
#' Output column names: tbd
#' 
#' @examples 
#' \dontrun{
#'   tbl(con, "table_name") %>% 
#'     collect() %>%
#'     oceo_process_amlr_station_header()
#' }
NULL


#' @name oceo_table_process
#' @export
oceo_amlr_station_header <- function(x) {
  x %>% 
    rename(amlr_station_header_id = ID, cruise_station = amlr_station, 
         cruise = amlr_cruise, station = Station, ship = Ship, 
         area = amlr_area, ice = ice_status)
}


#' @name oceo_table_process
#' @export
oceo_net_station_header <- function(x) {
  x %>% 
    rename(net_station_header_id = ID, sampling_comments = sampling_Comments,
           start_lat_deg = Start_Latitude_deg, start_lat_min = Start_Latitude_min, 
           start_lon_deg = Start_Longitude_deg, start_lon_min = Start_Longitude_min,
           end_lat_deg = End_latitude_deg, end_lat_min = End_Latitude_min, 
           end_lon_deg = End_Longitude_deg, end_lon_min = End_Longitude_min, 
           tow_valid = Tow_Valid, tow_comments = Invalid_Tow_Comments) %>% 
    mutate(time_local = as.factor(time_local), 
           # deal with hms if/when necessary
           date_tow = lubridate::ymd(date_tow))
}


#' @name oceo_table_process
#' @export
oceo_net <- function(x) {
  x %>% 
    rename(net_id = NET_ID, net_num = net, amlr_haul = AMLR_HAUL, 
           tow_valid = Tow_Valid, invalid_tow_comments = Invalid_Tow_Comments)
}


#' @name oceo_table_process
#' @export
oceo_tucker_header_data <- function(x) {
  x %>% 
    rename(tucker_header_data_id = ID, haul = Haul, station = Station, 
           swept_volume = Swept_volume, max_depth = Maximum_Depth, 
           depth_range = Depth_range, sampling_comments = Sampling_comments, 
           amlr_station_header_id = AMLR_Station_Header_ID)
}
