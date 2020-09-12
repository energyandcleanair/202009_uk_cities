require(rcrea)
require(dplyr)
require(ggplot2)

source('./config.R')
source('./plots.R')

mc <- rcrea::measurements(country=country, city=city, source="eea",poll="no2", with_metadata = T, deweathered=T, aggregate_level="city")
ms <- rcrea::measurements(country=country, city=city, source="eea",poll="no2", with_metadata = T, deweathered=T, aggregate_level="station")

l <- rcrea::locations(country=country, city=city, source="eea", with_meta = T)
write.csv(l, file.path(dir_results, "stations.csv"), row.names = F)

ms.city <- ms %>%
  left_join(l %>%
              mutate(id=tolower(id)) %>%
              dplyr::select(id, city, type),
            by=c("region_id"="id")) %>%
  mutate(city=tolower(city),
         level=paste0("station-", type)) %>%
  filter(process_id=="anomaly_gbm_lag1_station")

mc.city <- mc %>%
  mutate(city=tolower(region_id),
         level="city") %>%
  filter(process_id=="anomaly_gbm_lag1_city_mad")

(plot_city_vs_stations(ms.city=ms.city,
                      mc.city=mc.city,
                      running_days=30,
                      file=file.path(dir_results, "plot_city_vs_stations.png")))
