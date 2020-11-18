
source('./config.R')
source('./plots.R')
source('./utils.R')
source('./tables.R')

# Update measurements
# utils.upload.measurements()

date_from <- "2020-03-16"
date_to <- "2020-05-12"
source <- "defra"

mc <- rcrea::measurements(country=country, source=source, poll=c("no2","pm25","pm10","o3"), with_metadata = T, deweathered=NULL, aggregate_level="city")
ms <- rcrea::measurements(country=country, source=source, poll=c("no2","pm25","pm10","o3"), with_metadata = T, deweathered=NULL, aggregate_level="station")

# Combining others into "Other"
renaming_list = setNames(as.list(tolower(city)), tolower(city))
mc <- mc %>%
  mutate(region_id=recode(region_id, !!!renaming_list, .default="others"),
         region_name=tools::toTitleCase(region_id)) %>%
  group_by(date, poll, unit, region_id, process_id, source, timezone, region_name, country) %>%
  summarise(value=mean(value, na.rm=T))


l <- rcrea::locations(country=country, city=city, source=source, with_meta = T)
l <- l %>% mutate(type=recode(type,
                              "Background Urban"="Background",
                              "Background Suburban"="Background",
                              "Industrial Urban"="Industrial",
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

ms.city <- ms %>%
  left_join(l %>%
              mutate(id=tolower(id)) %>%
              dplyr::select(id, city, type),
            by=c("region_id"="id")) %>%
  mutate(city=recode(tolower(city), !!!renaming_list, .default="other"),
         type=tidyr::replace_na(type, "Unknown"),
         level=paste0("station-", type))


mc.city <- mc %>%
  mutate(city=region_id,
         level="city")

# Export csvs
export_measurements(mc.city)

# Plots
plot_poll("no2", ms.city, mc.city, date_from=date_from)
plot_poll("pm25", ms.city, mc.city, date_from=date_from)
plot_poll("pm10", ms.city, mc.city, date_from=date_from)
plot_poll("o3", ms.city, mc.city, date_from=date_from)

# Transportation ----------------------------------------------------------
tc.tomtom <- rcrea::transport.tomtom_congestion(cities=tibble(city=city, country="GB"))
tc.apple <- utils.transport.apple() %>% filter(region_id %in% tolower(city))
tc.mapbox <- utils.transport.mapbox(date_from)

plot_traffic_poll_tomtom(mc.city, tc.tomtom, n_day=14)
plot_traffic_poll_tomtom(mc.city, tc.tomtom, n_day=30)

plot_traffic_poll_apple(mc.city, tc.apple, n_day=14)
plot_traffic_poll_apple(mc.city, tc.apple, n_day=30)

plot_traffic_poll_mapbox(mc.city, tc.mapbox, n_day=14)
plot_traffic_poll_mapbox(mc.city, tc.mapbox, n_day=30)


plot_corr_traffic_poll_tomtom(mc.city, tc.tomtom, tc.apple, tc.mapbox, date_from=date_from, date_to=date_to)
plot_corr_traffic_poll_apple(mc.city, tc.tomtom, tc.apple, tc.mapbox, date_from=date_from, date_to=date_to)
plot_corr_traffic_poll_mapbox(mc.city, tc.tomtom, tc.apple, tc.mapbox, date_from=date_from, date_to=date_to)

# Other charts ------------------------------------------------------------
plot_predicted_vs_observed(mc.city, poll="no2", date_from=date_from, date_to=date_to)
plot_predicted_vs_observed(mc.city, poll="pm25", date_from=date_from, date_to=date_to)
plot_predicted_vs_observed(mc.city, poll="pm10", date_from=date_from, date_to=date_to)
plot_predicted_vs_observed(mc.city, poll="o3", date_from=date_from, date_to=date_to)
plot_anomaly_average(mc.city, process_anomaly = "anomaly_gbm_lag1_city_mad", date_from=date_from, date_to=date_to, filename=paste0("plot_anomaly_lockdown.jpg"))

# Other tables ------------------------------------------------------------
table_impact(mc.city, tc.tomtom, tc.apple, tc.mapbox, n_day=7, date_from = date_from, date_to=date_to)
table_impact(mc.city, tc.tomtom, tc.apple, tc.mapbox, n_day=14, date_from = date_from, date_to=date_to)
table_impact(mc.city, tc.tomtom, tc.apple, tc.mapbox, n_day=30, date_from = date_from, date_to=date_to)
t.impact <- table_impact(mc.city, tc.tomtom, tc.apple, tc.mapbox, n_day=NULL, date_from = date_from, date_to=date_to)





