source('./config.R')

if(T){
  require(creadeweather)
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
                                    upload_results = F)
  saveRDS(m.dew.20200316.pbl, "resuts/m.dew.20200316.pbl.RDS")

}
