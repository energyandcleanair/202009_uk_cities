source('./config.R')
source('./plots.R')
source('./utils.R')
source('./tables.R')

# Update measurements
# utils.upload.measurements()

date_from <- "2020-03-23"
date_to <- "2020-05-12"
source <- "defra"
polls <- c("no2","pm25","o3","pm10")



# Get stations
l <- rcrea::locations(country=country, city=city, source=source, with_meta = T)
l <- l %>% mutate(type=recode(type,
                              "Background Urban"="Background",
                              "Background Suburban"="Background",
                              "Industrial Urban"="Industrial",
                              "Industrial Suburban"="Industrial",
                              "Traffic Urban"="Traffic",
                              .default = "Unknown"),
                  type=tidyr::replace_na(type, "Unknown"))

write.csv(l %>% dplyr::select(id, name, city, type, source),
          file.path(dir_results_data, "stations.csv"), row.names = F)


l.all <- rcrea::locations(country=country, source=source, with_meta = T)
l.all <- l.all %>% mutate(type=recode(type,
                                      "Background Urban"="Background",
                                      "Background Suburban"="Background",
                                      "Industrial Urban"="Industrial",
                                      "Traffic Urban"="Traffic",
                                      .default = "Unknown"),
                          type=tidyr::replace_na(type, "Unknown"))

write.csv(l %>% dplyr::select(id, name, city, type, source),
          file.path(dir_results_data, "stations_all.csv"), row.names = F)

# Get measurements
mc <- utils.get.city.measurements(city, polls, use_local=T)
ms <- utils.get.station.measurements(city, polls, l, use_local=T)

# Export csvs
export_measurements(mc)

# Plots
for(poll in polls){
  plot_poll(poll, ms=ms, mc=mc, date_from=date_from)
}

# Transportation ----------------------------------------------------------
tc.tomtom <- utils.transport.tomtom(city)
tc.apple <- utils.transport.apple(city)
tc.mapbox <- utils.transport.mapbox(date0=date_from, city=city)

plot_traffic_poll_tomtom(mc, tc.tomtom, n_day=14)
plot_traffic_poll_tomtom(mc, tc.tomtom, n_day=30)

plot_traffic_poll_apple(mc, tc.apple, n_day=14)
plot_traffic_poll_apple(mc, tc.apple, n_day=30)

plot_traffic_poll_mapbox(mc, tc.mapbox, n_day=14)
plot_traffic_poll_mapbox(mc, tc.mapbox, n_day=30)

plot_traffic_poll_tomtom_apple(mc, tc.tomtom, tc.apple, n_day=30)

# plot_traffic_poll_apple_tomtom(mc, tc.apple, tc.tomtom, n_day=30)


# plot_corr_traffic_poll_tomtom(mc, tc.tomtom, tc.apple, tc.mapbox, date_from=date_from, date_to=date_to)
# plot_corr_traffic_poll_apple(mc, tc.tomtom, tc.apple, tc.mapbox, date_from=date_from, date_to=date_to)
# plot_corr_traffic_poll_mapbox(mc, tc.tomtom, tc.apple, tc.mapbox, date_from=date_from, date_to=date_to)

# Other charts ------------------------------------------------------------
for(poll in polls){
  plot_predicted_vs_observed(mc, poll=poll, date_from=date_from, date_to=date_to)
  plot_predicted_vs_observed_ts(mc, poll=poll, date_from=date_from, date_to=date_to)
}


plot_anomaly_average(mc %>% filter(tolower(region_id)!="others"), process_anomaly = "anomaly_gbm_lag1_city_mad", date_from=date_from, date_to=date_to, filename=paste0("plot_anomaly_lockdown.jpg"))

 # Other tables ------------------------------------------------------------
table_impact(mc, tc.tomtom, tc.apple, tc.mapbox, date_from = date_from, date_to=date_to)
# table_impact(mc, tc.tomtom, tc.apple, tc.mapbox, n_day=7, date_from = date_from, date_to=date_to)
# table_impact(mc, tc.tomtom, tc.apple, tc.mapbox, n_day=14, date_from = date_from, date_to=date_to)
# table_impact(mc, tc.tomtom, tc.apple, tc.mapbox, n_day=30, date_from = date_from, date_to=date_to)




# Questions ---------------------------------------------------------------

# - How many cities recovered their pre-covid levels
mc %>%
  filter(process_id=="anomaly_gbm_lag1_city_mad",
         lubridate::date(date)>=lubridate::date("2020-03-23") - lubridate::days(30),
         lubridate::date(date)<=lubridate::date("2020-09-30")) %>%
  rcrea::utils.running_average(30) %>%
  filter(lubridate::date(date)>=lubridate::date("2020-03-23")) %>%
  group_by(poll, unit, process_id, region_name) %>%
  arrange(date) %>%
  summarise(
    anomaly_lockdown=first(value),
    anomaly_last=last(value)) %>%
  mutate(delta_rebound=anomaly_last-anomaly_lockdown) %>%
  arrange(poll, delta_rebound) %>%
  write.csv(file.path("results","data","rebound_end_september.csv"), row.names = F)

mc %>%
  filter(process_id %in% c("anomaly_offsetted_gbm_lag1_city_mad","anomaly_gbm_lag1_city_mad"),
         lubridate::date(date)>=lubridate::date("2020-03-23") - lubridate::days(30),
         lubridate::date(date)<=lubridate::date("2020-09-30")) %>%
  select(-c(unit)) %>%
  rcrea::utils.running_average(30) %>%
  filter(lubridate::date(date)>=lubridate::date("2020-03-23")) %>%
  tidyr::pivot_wider(names_from="process_id", values_from="value") %>%
  rename(anomaly=anomaly_gbm_lag1_city_mad, offsetted=anomaly_offsetted_gbm_lag1_city_mad) %>%
  group_by(poll, region_name) %>%
  arrange(date) %>%
  summarise(
    anomaly_lockdown=first(anomaly),
    offsetted_lockdown=first(offsetted),
    anomaly_last=last(anomaly),
    offsetted_last=last(offsetted)) %>%
  mutate(delta_rebound=anomaly_last-anomaly_lockdown,
         delta_rebound_relative=(anomaly_last-anomaly_lockdown)/offsetted_lockdown) %>%
  arrange(poll, desc(delta_rebound)) %>%
  write.csv(file.path("results","data","rebound_end_september_with_relative.csv"), row.names = F)


# - How many cities went below 10 for PM2.5


