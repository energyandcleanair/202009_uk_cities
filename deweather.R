source('./config.R')

if(F){
  require(creadeweather)
  configs <- tibble(
    filename = c("m.dew.endq3.RDS","m.dew.endq2.RDS","m.dew.endq3.pbl.RDS","m.dew.endq2.pbl.RDS"),
    training_end = c("2019-09-30","2019-06-30","2019-09-30","2019-06-30"),
    add_pbl = c(F,F,T,T)
  )

  for (row in 1:nrow(configs)) {
    m.dew.city.raw <- creadeweather::deweather(source="mee",
                                               city=cities$CityEN,
                                               poll="pm25",
                                               output="anomaly",
                                               upload_results = F,
                                               years_force_refresh = NULL,
                                               add_pbl=configs[[row,"add_pbl"]],
                                               training_start_anomaly = "2017-04-01",
                                               training_end_anomaly = configs[[row,"training_end"]])


  m.dew.20200316 <- creadeweather::deweather(city=city, source="defra",
                                             poll=c("no2","pm25"),
                                             training_start_anomaly ="2017-03-16",
                                             training_end_anomaly = "2020-03-16",
                                             upload_results = F)
  saveRDS(m.dew.20200316, "resuts/m.dew.20200316.RDS")

  m.dew.20200316.pbl <- creadeweather::deweather(city=city, source="defra",
                                                 poll=c("no2","pm25"),
                                                 training_start_anomaly ="2017-03-16",
                                                 training_end_anomaly = "2020-03-16",
                                                 years_force_refresh = NULL,
                                                 add_pbl=T,
                                                 upload_results = F)
  saveRDS(m.dew.20200316.pbl, "resuts/m.dew.20200316.pbl.RDS")

  m.obs <- rcrea::measurements(source="defra", city=city, poll=c("no2","pm25"), date_from="2017-03-16", date_to=date_to)

}else{
  m.dew <- readRDS("~/development/crea/studies/202009_ukcities/results/data/m.dew.20200316.RDS")
  m.dew.pbl <- readRDS("~/development/crea/studies/202009_ukcities/results/data/m.dew.20200316.pbl.RDS")
  m.obs <- readRDS("~/development/crea/studies/202009_ukcities/results/data/m.obs.RDS") %>%
    mutate(date=lubridate::date(date))
}

date_from <- "2020-03-16"
date_to <- "2020-05-12"

m.dew <- m.dew %>%
  mutate(process_id=output) %>%
  tidyr::unnest(normalised) %>%
  select(-c(predicted)) %>%
  mutate(date=lubridate::date(date))

m.dew.pbl <- m.dew.pbl %>%
  mutate(process_id=paste0(output,"_pbl")) %>%
  tidyr::unnest(normalised) %>%
  select(-c(predicted)) %>%
  mutate(date=lubridate::date(date))

m <- bind_rows(m.obs, m.dew, m.dew.pbl)

ggplot(m %>% filter(process_id %in% c("city_day_mad", "anomaly_counterfactual","anomaly_counterfactual_pbl"), poll=="no2") %>% rcrea::utils.running_average(30)) +
  geom_line(aes(date, value, col=process_id)) +
  facet_wrap(~region_id, scales="free_y")


plot_predicted_vs_observed(m.dew, poll="no2", date_from=date_from, date_to=date_to)

process_id %in% c("city_day_mad",
                  "anomaly_counterfactual_gbm_lag1_city_mad")) %>%

