# Oceo database exploration

###############################################################################
library(tidyverse)
library(lubridate)
library(DBI)
library(odbc)

tableNA <- function(...) table(..., useNA = 'ifany')

con <- dbConnect(odbc(),
                 Driver = "SQL Server", #"SQL Server Native Client 11.0",
                 Server = "swc-estrella-s",
                 Database = "AMLR_OCEO",
                 Trusted_Connection = "True")


amlr.station <- tbl(con, "AMLR_STATION_HEADER") %>% collect()
ctd.station <- tbl(con, "CTD_STATION_HEADER") %>% collect()
net.station <- tbl(con, "NET_STATION_HEADER") %>% collect()

tucker <- tbl(con, "TUCKER_HEADER_DATA") %>% collect()

ctd.bottle <- tbl(con, "CTD_BOTTLE_DATA") %>% collect()
# ctd.cast <- tbl(con, "CTD_CAST_DATA") %>% collect()
ctd.chla <- tbl(con, "CHLA_SIZEFRACTION_RAW") %>% collect()
ctd.nutrient <- tbl(con, "BOTTLE_NUTRIENT") %>% collect()

net <- tbl(con, "NET") %>% collect()
net.zoo <- tbl(con, "NET_ZOOPLANKTON_DATA") %>% collect()
# krill <- tbl(con, "krill") %>% collect()
krill.maturity.length <- tbl(con, "KRILL_MATURITY_LENGTH") %>% collect()
elmd <- tbl(con, "EUPHAUSIID_LENGTH_MATURITY_DATA") %>% collect()

x1 <- tbl(con, "vCalculate_VOLUME_FILTERED") %>% collect() %>% 
  arrange(net_id)




###############################################################################
# Station header tables

tableNA(amlr.station$amlr_cruise)
# tableNA(amlr.station$Station)
n_distinct(amlr.station$Station)
tableNA(amlr.station$leg)
tableNA(amlr.station$Ship)
tableNA(amlr.station$amlr_area)
summary(amlr.station$bottom_depth)
tableNA(amlr.station$ice_status)
tableNA(amlr.station$restrictedUse) # Peru ones are the only ones marked as 'restricted'
tableNA(amlr.station$gssmu)

tableNA(amlr.station$amlr_cruise, amlr.station$restrictedUse)


amlr.station.restricted <- amlr.station %>% filter(restrictedUse)


sum(is.na(ctd.station$amlr_station_header_id)) #0
sum(is.na(ctd.station$UTC_date))
tableNA(ctd.station$bottles_fired)
tableNA(ctd.station$standard_depths)
tableNA(ctd.station$water_zones)



sum(is.na(net.station$amlr_station_header_id)) #0
summary(ymd(net.station$date_tow)) #287 NA
tableNA(net.station$time_local)
tableNA(net.station$Tow_Valid)

waldo::compare(net.station$start_time_utc, net.station$old_start_time_utc) #not the same


###############################################################################
# CTD data tables

sum(is.na(ctd.bottle$ctd_station_header_id)) #0
tableNA(ctd.bottle$bottle)
tableNA(tableNA(ctd.bottle$ctd_station_header_id)) # How many bottle casts were there per station

sum(is.na(ctd.cast$ctd_station_header_Id)) #0
summary(ctd.cast$Depth)
summary(ctd.cast$Temperature)

sum(is.na(ctd.chla$AMLR_Station_Header_ID)) #97
sum(is.na(ctd.nutrient$AMLR_Station_Header_ID)) #76
sum(is.na(ctd.nutrient$ctd_station_header_id)) #127


###############################################################################
# Net tow data tables
sum(is.na(net$net_station_header_id)) #0
sum(!is.na(net$start_date_utc)) #16
sum(!is.na(net$Start_Latitude_deg)) #141
sum(!is.na(net$Ice_type)) #600
tableNA(net$net_type)


tableNA(krill.maturity.length$maturity)
tableNA(krill.maturity.length$time_local)
tableNA(krill.maturity.length$amount)
summary(krill.maturity.length$abund)

d <- krill.maturity.length %>% filter(net_id == 7374)
d2 <- krill.maturity.length %>% filter(net_id == 3)
unique(d$Krill_total)
unique(d$Krill_measured)
unique(d$Species_ID)
unique(d$length)
sum(d2$amount)


krill.maturity.length.summ <- krill.maturity.length %>% 
  group_by(net_id) %>% 
  summarise(total_n_distinct = n_distinct(Krill_total), 
            total = unique(Krill_total), 
            measured_n_distinct = n_distinct(Krill_measured), 
            species_n_distinct = n_distinct(Species_ID), 
            amt_sum = sum(amount), 
            diff = total - amt_sum)

tableNA(krill.maturity.length.summ$total_n_distinct)
tableNA(krill.maturity.length.summ$measured_n_distinct)
tableNA(krill.maturity.length.summ$species_n_distinct)
tableNA(krill.maturity.length.summ$diff)
sum(krill.maturity.length.summ$diff != 0) #1291
sum(krill.maturity.length.summ$diff == 0) #2077


# Compare klm and elmd
klm.net3 <- krill.maturity.length %>% filter(net_id == 3)
elmd.net3 <- elmd %>% filter(NET_ID == 3)

net.id <- 3
klm.net.curr <- krill.maturity.length %>% filter(net_id == net.id)
elmd.net.curr <- elmd %>% filter(NET_ID == net.id)
all.equal(
  elmd.net.curr %>% 
    select(net_id = NET_ID, Species_ID, length, maturity, amount) %>% 
    arrange(Species_ID, length, maturity), 
  klm.net.curr %>% 
    select(net_id, Species_ID, length, maturity, amount) %>% 
    arrange(Species_ID, length, maturity)
)


###############################################################################
# Why are some tucker nets missing tucker headers?
net.tucker <- net %>% filter(net_type == "T")


###############################################################################
# Compare vCalculate_Volume_Filtered to output of amlrOceo::calculate_volume_filtered()

waldo::compare(x1$net_id, net.proc$net_id)
waldo::compare(x1$AMLR_Cruise, net.proc$amlr_cruise)
waldo::compare(x1$Volume,net.proc$volume, tolerance = 10)
waldo::compare(x1$NZ_vol, if_else(net.proc$volume_isnull, "Y", "N"))
waldo::compare(x1$depth_fished, net.proc$depth_fished, tolerance = 10)
waldo::compare(x1$NZ_depth, if_else(net.proc$depth_fished_isnull, "Y", "N"))


d.diff <- x1 %>% filter(round(x1$Volume, 2) != round(net.proc$volume, 2))
d.diff <- x1 %>% filter(NZ_vol != if_else(net.proc$volume_isnull, "Y", "N"))

# Wowww so cool
x2 <- x1 %>% 
  select(net_id, net_station_header_id, amlr_cruise = AMLR_Cruise, amlr_station = AMLR_Station, 
         amlr_station_header_id = AMLR_Station_Header_ID,
         net_type, net, volume = Volume, volume_isnull = NZ_vol, 
         depth_fished, depth_fished_isnull = NZ_depth)

waldo::compare(
  x2, 
  amlrOceo::calculate_filtered_volume(net, net.station, amlr.station, tucker) %>% 
    mutate(volume_isnull = if_else(volume_isnull, "Y", "N"), 
           depth_fished_isnull = if_else(depth_fished_isnull, "Y", "N")), 
  tolerance = 10
)
