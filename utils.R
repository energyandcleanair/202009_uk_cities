utils.read.stations <- function(){
  ukair_catalogue(
    site_name = "",
    pollutant = 9999,
    group_id = 9999,
    closed = "true",
    country_id = 9999,
    region_id = 9999
  )
}

utils.upload.stations <- function(){
  s = utils.read.stations()

  s.filtered <- s %>%
    filter(!is.na(EU.Site.ID)) %>%
    mutate(
      source="defra",
      timezone="Europe/London"
    ) %>%
    rename(id=UK.AIR.ID,
           name=Site.Name,
           type=Environment.Type)

  s.filtered.geocoded <- s.filtered %>%
    filter(!is.na(Latitude))

  # cities.google <- revgeo(s.filtered.geocoded$Longitude, s.filtered.geocoded$Latitude, provider='google', API="AIzaSyAM2hj3VbXCSjAXIjLjLH_DfPPSiV8Bhg0", output = 'frame')

  cities.photon <- revgeo(s.filtered.geocoded$Longitude, s.filtered.geocoded$Latitude, provider='photon', output = 'frame')

  s.filtered.geocoded$city <- cities.photon$city

  s.result <- s.filtered.geocoded %>%
    mutate(country="GB")

  s.sf <- sf::st_as_sf(s.result, coords=c("Longitude","Latitude"), crs=4326) %>%
    tibble() %>%
    mutate(geometry=sf::st_as_text(geometry, EWKT=T))

  required_cols <- c("id","name","city","country","timezone","source","geometry","type")
  s.upload <- s.sf %>% select_at(required_cols)

  rcrea::upsert_locations(s.upload)

}

utils.read.measurements.file <- function(file){

  stations <- read.csv(file, skip = 3, nrow = 1, header=F, stringsAsFactors = F) %>%
    tidyr::pivot_longer(cols=names(.), values_to="station") %>%
    pull(station) %>%
    na.locf(na.rm=F) %>%
    tail(-2)

  data <- read.csv(file, skip = 4, header = T, stringsAsFactors = F) %>%
    filter(Time != "")

  data.long <- data %>% tidyr::pivot_longer(cols=-c(Date,Time), names_to="indicator", values_to="value")
  data.long$station <- rep(stations, nrow(data))

  data.long$indicator[stringr::str_starts(data.long$indicator, "Status")] <- "status"
  data.long$indicator[stringr::str_starts(data.long$indicator, "PM10")] <- "pm10"
  data.long$indicator[stringr::str_starts(data.long$indicator, "PM2.5")] <- "pm25"
  data.long$indicator[stringr::str_starts(data.long$indicator, "Ozone")] <- "o3"
  data.long$indicator[stringr::str_starts(data.long$indicator, "Nitrogen.dioxide")] <- "no2"
  data.long$indicator[stringr::str_starts(data.long$indicator, "Nitrogen.dioxide")] <- "no2"
  polls <- c("pm10","pm25","no2","o3")
  data.long <- data.long %>%
    left_join(tibble(indicator=polls,pollutant=polls)) %>%
    mutate(pollutant=na.locf(pollutant,na.rm=F))

  data.long$indicator[data.long$indicator %in% polls] <- "value"
  data.long <- data.long %>% tidyr::pivot_wider(names_from=indicator, values_from=value)
  data.long$value <- as.numeric(data.long$value)
  data.long$date = as.POSIXct(paste(data.long$Date,data.long$Time))

  data.long$unit <- data.lon$status
  data.long$unit[stringr::str_detect(data.long$unit$unit,"ugm-3")] <- "µg/m3"
  data.long$source <- "defra"

  return(data.long %>% dplyr::select(-c(Date,Time)))
}

utils.read.measurements <- function(){


  files <- list.files(file.path("data","defra"), pattern="*.csv", full.names = T)
  m <- do.call("rbind", lapply(files, utils.read.measurements.file))

  # m doesn't have station id
  stations <- utils.read.stations()

  m.full <- m %>%
    inner_join(stations %>% select(region_id=UK.AIR.ID, station="Site.Name"), by=c("station"))

  return(m.full)

}


utils.upload.measurements <- function(){

  m <- utils.read.measurements()

  # Sources must have been uploaded before
  l <- rcrea::locations(source="defra")

  m <- m %>%
    filter(!is.na(value)) %>%
    left_join(l %>% select(region_id=id, city))



  # Only upload daily values

  # 1.stations
  m.station.day <- m %>%
    group_by(region_id, poll=pollutant, unit, date=lubridate::date(date), source) %>%
    summarise(value=mean(value, na.rm=T))

  m.station.day$process_id = "station_day_mad" #This is actually wrong, there is no filter applied yet. Makes things simpler for deweathering though

  required_cols <- c("date","poll","unit","region_id","process_id","source","value")
  m.station.day <- m.station.day %>% select_at(required_cols)
  rcrea::upsert_meas(m.station.day[1:100000,])
  rcrea::upsert_meas(m.station.day[100001:200000,])
  rcrea::upsert_meas(m.station.day[200001:300000,])
  rcrea::upsert_meas(m.station.day[300001:400000,])
  rcrea::upsert_meas(m.station.day[400001:500000,])
  rcrea::upsert_meas(m.station.day[500001:600000,])
  rcrea::upsert_meas(m.station.day[600001:nrow(m.station.day),])

  # 2.cities
  m.city.day <- m %>%
    group_by(region_id=tolower(city), poll=pollutant, unit, date=lubridate::date(date), source) %>%
    summarise(value=mean(value, na.rm=T))

  m.city.day$process_id = "city_day_mad" #This is actually wrong, there is no filter applied yet. Makes things simpler for deweathering though
  m.city.day <- m.city.day %>% select_at(required_cols)
  rcrea::upsert_meas(m.city.day)


}

utils.export.meas <- function( ms.city, mc.city, filename, running_days=30){


  ms.running <- ms.city %>% rcrea::utils.rolling_average("day", running_days, "value") %>%
    select(station=region_id, city, poll, unit, date, value, type, source, process_id) %>%
    arrange(station, poll, date)

  mc.running <- mc.city %>% rcrea::utils.rolling_average("day", running_days, "value") %>%
    select(city=region_id, poll, unit, date, value, source, process_id) %>%
    arrange(city, poll, date)

  format <- function(x){ x %>% mutate(poll=paste0(poll," [",unit,"]"), date=lubridate::date(date)) %>%
    select(-c(process_id, unit)) %>%
    pivot_wider(names_from=poll)}

  list_of_datasets <- list(
    mc.running %>% filter(process_id=="city_day_mad") %>% format(),
    mc.running %>% filter(process_id=="anomaly_offsetted_gbm_lag1_city_mad") %>% format()
    # ms.running %>% filter(process_id=="station_day_mad") %>% format(),
    # ms.running %>% filter(process_id=="anomaly_offsetted_gbm_lag1_station") %>% format()
  )

  names(list_of_datasets) <- c(
    paste0("city.observations.",running_days,"days"),
    paste0("city.deweathered.",running_days,"days")
    # paste0("station.observations.",running_days,"days"),
    # paste0("station.deweathered.",running_days,"days")
  )


  write.xlsx(list_of_datasets, file = file.path(dir_results_data, "measurements.xlsx"))


}


utils.anomaly_rel_counterfactual <- function(m, process_observation="city_day_mad", process_anomaly="anomaly_gbm_lag1_city_mad"){

  recoder <- c("observation","anomaly")
  names(recoder) <- c(process_observation, process_anomaly)

  m %>%
    filter(process_id %in% c(process_observation, process_anomaly)) %>%
    mutate(process_id=recode(process_id, !!!recoder)) %>%
    mutate(unit=recode(unit, "Δ µg/m3"="µg/m3")) %>%
    tidyr::pivot_wider(names_from=process_id, values_from=value) %>%
    mutate(value=anomaly/(observation-anomaly),
           process_id="anomaly_rel_counterfactual") %>%
    filter(!is.na(value)) %>%
    select(-c(observation, anomaly))

}
