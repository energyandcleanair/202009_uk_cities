
source('./config.R')
source('./plots.R')
source('./utils.R')

# Update measurements
# utils.upload.measurements()


date_from <- "2020-03-23"
date_to <- "2020-05-12"
source <- "defra"

mc <- rcrea::measurements(country=country, city=city, source=source, poll=c("no2","pm25","pm10","o3"), with_metadata = T, deweathered=NULL, aggregate_level="city")
ms <- rcrea::measurements(country=country, city=city, source=source, poll=c("no2","pm25","pm10","o3"), with_metadata = T, deweathered=NULL, aggregate_level="station")

l <- rcrea::locations(country=country, city=city, source=source, with_meta = T)
l <- l %>% mutate(type=recode(type,
                              "Background Urban"="Background",
                              "Background Suburban"="Background",
                              "Industrial Urban"="Industrial",
                              "Traffic Urban"="Traffic",
                         .default = "Unknown"))

write.csv(l %>% dplyr::select(id, name, city, type, source),
          file.path(dir_results_data, "stations.csv"), row.names = F)

ms.city <- ms %>%
  left_join(l %>%
              mutate(id=tolower(id)) %>%
              dplyr::select(id, city, type),
            by=c("region_id"="id")) %>%
  mutate(city=tolower(city),
         level=paste0("station-", type))
  # Adding anomaly vs lockdown starting date
  # rbind(utils.anomaly_lockdown(.,process_anomaly="anomaly_gbm_lag1_station"))


mc.city <- mc %>%
  mutate(city=tolower(region_id),
         level="city")
  # Adding anomaly vs lockdown starting date
  # rbind(utils.anomaly_lockdown(.))


plot_poll("no2", ms.city, mc.city)
plot_poll("pm25", ms.city, mc.city)
plot_poll("pm10", ms.city, mc.city)
plot_poll("o3", ms.city, mc.city)

# Transportation ----------------------------------------------------------
tc.tomtom <- rcrea::transport.tomtom_congestion(cities=tibble(city=city, country="GB"))
tc.apple <- utils.transport.apple() %>% filter(region_id %in% tolower(city))

plot_traffic_poll_tomtom(mc.city, tc.tomtom, n_day=14)
plot_traffic_poll_tomtom(mc.city, tc.tomtom, n_day=30)

plot_traffic_poll_apple(mc.city, tc.apple, n_day=14)
plot_traffic_poll_apple(mc.city, tc.apple, n_day=30)

plot_corr_traffic_poll_tomtom(mc.city, tc.tomtom, tc.apple, date_from=date_from, date_to=date_to)
plot_corr_traffic_poll_apple(mc.city, tc.tomtom, tc.apple, date_from=date_from, date_to=date_to)

# Other charts ------------------------------------------------------------
plot_predicted_vs_observed(mc.city, poll="no2", date_from=date_from, date_to=date_to)
plot_predicted_vs_observed(mc.city, poll="pm25", date_from=date_from, date_to=date_to)
plot_predicted_vs_observed(mc.city, poll="pm10", date_from=date_from, date_to=date_to)
plot_predicted_vs_observed(mc.city, poll="o3", date_from=date_from, date_to=date_to)
plot_anomaly_average(mc.city, process_anomaly = "anomaly_gbm_lag1_city_mad", date_from=date_from, date_to=date_to, filename=paste0("plot_anomaly_lockdown.jpg"))

# Other tables ------------------------------------------------------------
table_impact(mc.city, tc.tomtom, tc.apple, n_day=7, date_from = date_from, date_to=date_to)
table_impact(mc.city, tc.tomtom, tc.apple, n_day=14, date_from = date_from, date_to=date_to)
table_impact(mc.city, tc.tomtom, tc.apple, n_day=30, date_from = date_from, date_to=date_to)
t.impact <- table_impact(mc.city, tc.tomtom, tc.apple, n_day=NULL, date_from = date_from, date_to=date_to)





