# What's happening in London

m.london.city.eea <- rcrea::measurements(city="London",
                                         source="eea",
                                         poll=c("pm25","no2"),
                                         aggregate_level = "city",
                                         date_from="2020-01-01",
                                         deweathered=NULL)

m.london.station.eea <- rcrea::measurements(city="London",
                                         source="eea",
                                         poll=c("pm25","no2"),
                                         aggregate_level = "station",
                                         date_from="2020-01-01",
                                         deweathered=NULL)

m.london.city.defra <- rcrea::measurements(city="London",
                                         source="defra",
                                         poll=c("pm25","no2"),
                                         aggregate_level = "city",
                                         date_from="2020-01-01",
                                         deweathered=NULL)

m.london.station.defra <- rcrea::measurements(city="London",
                                            source="defra",
                                            poll=c("pm25","no2"),
                                            aggregate_level = "station",
                                            date_from="2020-01-01",
                                            deweathered=NULL)


m <- rbind(m.london.city.eea %>% mutate(level="city"),
           m.london.station.eea %>% mutate(level="station"),
           m.london.city.defra %>% mutate(level="city"),
           m.london.station.defra %>% mutate(level="station"))

m.plot1 <- m %>%
  filter(process_id %in% c("station_day_mad", "city_day_mad")) %>%
  rcrea::utils.rolling_average("day", 30, "value")

ggplot(m.plot1) +
  geom_line(aes(date, value, color=level, group=region_id)) +
  facet_wrap(~source + poll)
