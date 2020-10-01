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
         level=paste0("station-", type))

mc.city <- mc %>%
  mutate(city=tolower(region_id),
         level="city")




plot_poll("no2", ms.city, mc.city)
plot_poll("pm25", ms.city, mc.city)
plot_poll("pm10", ms.city, mc.city)
plot_poll("o3", ms.city, mc.city)



# Transportation ----------------------------------------------------------
tc <- rcrea::transport.tomtom_congestion(cities=tibble(city=city, country="GB"))

d.plot <- mc.city %>%
  mutate(date=lubridate::date(date),
         value=value/100) %>%
  filter(process_id=="anomaly_percent_gbm_lag1_city_mad",
         poll=="no2") %>%
  utils.add_lockdown() %>%
  right_join(tc %>%
               mutate(date=lubridate::date(date),
                      region_id=tolower(city)) %>%
               select(-c(value, weekday, week, city)),
             by=c("region_id","country","date")) %>%
  rename(no2=value, traffic=diffRatio) %>%
  mutate(movement=lubridate::date(movement),
         first_measures=lubridate::date(first_measures))

d.plot <- d.plot %>%
  tidyr::pivot_longer(c(no2, traffic), names_to="indicator") %>%
  rcrea::utils.rolling_average("day",14,"value")

ggplot(d.plot) +
  geom_line(aes(date,value,col=indicator)) +
  facet_wrap(~region_id) +
  scale_y_continuous(labels=scales::percent) +
  theme_light() +
  geom_vline(data=d.plot, aes(xintercept=movement, linetype="National lockdown"),
             color=rcrea::CREAtheme.pal_crea['Turquoise']) +
  geom_vline(data=d.plot, aes(xintercept=partial_restriction, linetype="Partial restrictions"),
                 color=rcrea::CREAtheme.pal_crea['Turquoise']) +
  scale_linetype_manual(values=c("dashed","dotted"), name=NULL) +
  labs(
    subtitle="2020 vs 2019 NO2 weather-corrected concentration and traffic congestion levels",
    caption="Source: CREA based on DEFRA and TomTom",
       y="Year-on-year variation",
    x=NULL)

ggsave(file.path(dir_results_plots, "no2.traffic.png"), width=10, height=8)

