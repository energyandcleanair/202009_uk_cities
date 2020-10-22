require(plyr)
require(raster)
require(tidyverse)
require(magrittr)
require(lubridate)
require(dplyr)
require(rcrea)
require(sf)
require(forcats)
source('./config.R')

m <- rcrea::measurements(city=city,
                         deweathered = NULL,
                         date_from="2020-03-01",
                         source="defra",
                         with_metadata = T,
                         with_geometry = T)

devtools::reload('/Users/ht/Library/R/3.6/library/rcrea', quiet = FALSE)


m.scenarios <- m %>%
  mutate(country="GB") %>%
  rcrea::utils.add_city_pop() %>%
  rcrea::health.build.scenarios()


hi.detailed <- health.impact(m.scenarios, date_from="2020-03-01", date_to="2020-12-31")
write.csv(hi.detailed, file.path(dir_results_data,"health.details.csv"), row.names = F)

hi.simplified <- health.simplify(hi.detailed)
write.csv(hi.simplified, file.path("results","data","health.summary.csv"))


plot_causes(hi.detailed)
plot_cost_causes(hi.detailed)
plot_death(hi.detailed)
