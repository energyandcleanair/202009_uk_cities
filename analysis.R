require(rcrea)
require(dplyr)
require(ggplot2)
require(rdefra)
require(zoo)
require(revgeo)
require(openxlsx)
require(RColorBrewer)

source('./config.R')
source('./plots.R')

source <- "defra"

mc <- rcrea::measurements(country=country, city=city, source=source, poll=c("no2","pm25","pm10","o3"), with_metadata = T, deweathered=NULL, aggregate_level="city")
ms <- rcrea::measurements(country=country, city=city, source=source, poll=c("no2","pm25","pm10","o3"), with_metadata = T, deweathered=NULL, aggregate_level="station")

l <- rcrea::locations(country=country, city=city, source=source, with_meta = T)
l <- l %>% mutate(type=recode(type, "Background Urban"="Background",
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
         level=paste0("station-", type)) %>%
  # Adding relative vs counterfactual
  rbind(utils.anomaly_rel_counterfactual(.,
                                         process_observation = "station_day_mad",
                                         process_anomaly="anomaly_gbm_lag1_station")

  )




mc.city <- mc %>%
  mutate(city=tolower(region_id),
         level="city") %>%
  # Adding relative vs counterfactual
  rbind(utils.anomaly_rel_counterfactual(.))


plot_poll("no2", ms.city, mc.city)
plot_poll("pm25", ms.city, mc.city)
plot_poll("pm10", ms.city, mc.city)
plot_poll("o3", ms.city, mc.city)



# Transportation ----------------------------------------------------------
tc <- rcrea::transport.tomtom_congestion(cities=tibble(city=city, country="GB"))
plot_traffic_poll(mc.city, tc)


# Other charts ------------------------------------------------------------
plot_corr_traffic_poll(mc.city, tc)


# Other tables ------------------------------------------------------------

mc.city %>%
  filter(process_id=="anomaly_rel_counterfactual") %>%
  utils.add_lockdown() %>%
  filter(date>=lubridate::date(movement),
         date<=lubridate::date(movement)+lubridate::days(61)) %>%
  # rcrea::utils.rolling_average("day", 30, "value") %>%
  group_by(region_id, poll) %>%
  summarise(min=min(value/100, na.rm=T),
            avg_1month=mean(value/100, na.rm=T)) %>%
  tidyr::pivot_wider(names_from=poll, values_from=c(min, avg_1month)) %>%
  write_csv(file.path("results","data","lockdown_impact.csv"), na = "")


