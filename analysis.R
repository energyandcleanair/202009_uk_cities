require(rcrea)
require(dplyr)
require(ggplot2)

source('./config.R')
source('./plots.R')

mc <- rcrea::measurements(country=country, city=city, source="eea",poll="no2", with_metadata = T, deweathered=T, aggregate_level="city")
ms <- rcrea::measurements(country=country, city=city, source="eea",poll="no2", with_metadata = T, deweathered=T, aggregate_level="station")

l <- rcrea::locations(country=country, city=city, source="eea", with_meta = T)
write.csv(l %>% dplyr::select(id, name, city, name_2, name_1, type, source), file.path(dir_results, "stations.csv"), row.names = F)

ms.city <- ms %>%
  left_join(l %>%
              mutate(id=tolower(id)) %>%
              dplyr::select(id, city, type),
            by=c("region_id"="id")) %>%
  mutate(city=tolower(city),
         level=paste0("station-", type))

mc.city <- mc %>%
  mutate(city=tolower(region_id),
         level="city")

(plot_city_vs_stations(ms.city=ms.city %>% filter(process_id=="anomaly_gbm_lag1_station"),
                      mc.city=mc.city %>% filter(process_id=="anomaly_gbm_lag1_city_mad"),
                      running_days=30,
                      unit="Anomaly [µg/m3]",
                      file=file.path(dir_results, "plot_city_vs_stations.png")))

(plot_city_vs_stations(ms.city=ms.city %>% filter(process_id=="anomaly_offsetted_gbm_lag1_station"),
                       mc.city=mc.city %>% filter(process_id=="anomaly_offsetted_gbm_lag1_city_mad"),
                       running_days=30,
                       unit="Weather-corrected concentration [µg/m3]",
                       file=file.path(dir_results, "plot_city_vs_stations_offsetted.png")))

(plot_city_vs_stations(ms.city=ms.city %>% filter(process_id=="anomaly_percent_gbm_lag1_station"),
                       mc.city=mc.city %>% filter(process_id=="anomaly_percent_gbm_lag1_city_mad"),
                       running_days=30,
                       unit="Anomaly [%]",
                       file=file.path(dir_results, "plot_city_vs_stations_percent.png")))
