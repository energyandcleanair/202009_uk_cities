require(plyr)
require(raster)
require(tidyverse)
require(lubridate)
require(dplyr)
require(rcrea)
require(sf)
require(forcats)
source('./config.R')
source('./plots.health.R')

m <- rcrea::measurements(city=city,
                         deweathered = NULL,
                         date_from="2020-03-01",
                         source="defra",
                         with_metadata = T,
                         with_geometry = T)
#
# m.lockdowns <- rbind(m,
#              utils.anomaly_lockdown(m, 0) %>% mutate(process_id="anomaly_lockdown_0"),
#              utils.anomaly_lockdown(m, 7) %>% mutate(process_id="anomaly_lockdown_7"),
#              utils.anomaly_lockdown(m, 14) %>% mutate(process_id="anomaly_lockdown_14"),
#              utils.anomaly_lockdown(m, 30) %>% mutate(process_id="anomaly_lockdown_30")
#              )

# devtools::reload('/Users/ht/Library/R/3.6/library/rcrea', quiet = FALSE)


health.run <- function(m, n_day){

  m.lockdowns <- rbind(m, utils.anomaly_lockdown(m, n_day))

  m.scenarios <- m.lockdowns %>%
    mutate(country="GB") %>%
    rcrea::utils.add_city_pop() %>%
    rcrea::health.build.scenarios(
      process_observation ="city_day_mad",
      process_anomaly = "anomaly_lockdown")

  movement <- "2020-03-23"
  hi.detailed <- health.impact(m.scenarios, date_from=movement, date_to="2020-05-23")
  write.csv(hi.detailed, file.path(dir_results_data,"health.details.csv"), row.names = F)

  hi.simplified <- health.simplify(hi.detailed)
  write.csv(hi.simplified, file.path("results","data","health.summary.csv"))

  plot_anomaly_average(m.lockdowns, date_from=movement, filename=paste0("plot_anomaly_lockdown_avg_",n_day,"day.jpg"))
  plot_cost_causes(hi.detailed, filename=paste0("plot_cost_causes_",n_day,"day.jpg"))
  plot_death(hi.detailed, filename=paste0("plot_death_",n_day,"day.jpg"))
  plot_causes(hi.detailed, filename=paste0("plot_causes_",n_day,"day.jpg"))
}

health.run(m,0)
health.run(m,7)
health.run(m,14)
health.run(m,30)


