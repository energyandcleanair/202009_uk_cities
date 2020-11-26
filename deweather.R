source('./config.R')

if(T){
  require(creadeweather)
  m.dew <- creadeweather::deweather(city=city, source="defra", poll="no2",
                                    training_start_anomaly ="2017-03-16",
                                    training_end_anomaly = "2020-03-16",
                                    upload_results = F)

}
