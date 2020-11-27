utils.get.city.measurements <- function(city, polls, use_local=T){
  file <- file.path("data","m.city.RDS")
  if(!use_local || !file.exists(file)){

    mc <- rcrea::measurements(country=country, source=source, poll=polls, with_metadata = T, deweathered=NULL, aggregate_level="city")

    # Combining others into "Other"
    renaming_list = setNames(as.list(tolower(city)), tolower(city))
    mc <- mc %>%
      mutate(region_id=recode(region_id, !!!renaming_list, .default="others"),
             region_name=tools::toTitleCase(region_id)) %>%
      group_by(date, poll, unit, region_id, process_id, source, timezone, region_name, country) %>%
      summarise(value=mean(value, na.rm=T))

    mc <- mc %>%
      mutate(city=region_id,
             level="city") %>%
      ungroup()

    saveRDS(mc, file)

  }else{
    mc <- readRDS(file)
  }

  return(mc)
}

utils.get.station.measurements <- function(city, polls, use_local=T){
  file <- file.path("data","m.station.RDS")
  if(!use_local || !file.exists(file)){

    ms <- rcrea::measurements(country=country, source=source, poll=polls,
                              date_from="2020-01-01", # Too time consuming otherwise
                              with_metadata = T, deweathered=NULL, aggregate_level="station")
    l <- rcrea::locations(country=country, city=city, source=source, with_meta = T)

    ms <- ms %>%
      left_join(l %>%
                  mutate(id=tolower(id)) %>%
                  dplyr::select(id, city, type),
                by=c("region_id"="id")) %>%
      mutate(city=recode(tolower(city), !!!renaming_list, .default="other"),
             type=tidyr::replace_na(type, "Unknown"),
             level=paste0("station-", type))
    saveRDS(ms, file)
  }else{
    ms <- readRDS(file)
  }

  return(ms)
}

utils.read.stations <- function(){
  require(rdefra)
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

  require(devtools)
  devtools::install_github("hubert-thieriot/revgeo")
  library(revgeo)

  s = utils.read.stations()
  s.filtered <- s %>%
    filter(!is.na(EU.Site.ID)) %>%
    mutate(
      source="defra",
      timezone="Europe/London"
    ) %>%
    dplyr::rename(id=UK.AIR.ID,
                  name=Site.Name,
                  type=Environment.Type)

  s.filtered.geocoded <- s.filtered %>%
    filter(!is.na(Latitude))

  # cities.google <- revgeo(s.filtered.geocoded$Longitude, s.filtered.geocoded$Latitude, provider='google', API="AIzaSyAM2hj3VbXCSjAXIjLjLH_DfPPSiV8Bhg0", output = 'frame')
  cities.photon <- revgeo(s.filtered.geocoded$Longitude, s.filtered.geocoded$Latitude, provider='photon', output = 'frame')

  s.filtered.geocoded$city <- cities.photon$city

  s.result <- s.filtered.geocoded %>%
    mutate(country="GB")


  s.result <- s.result %>%
    mutate(city=recode(city,
                       "Aberdeen City"="Aberdeen",
                       "City of Edinburgh"="Edinburgh",
                       "GLASGOW"="Glasgow",
                       "Royal Borough of Greenwich"="Greenwich",
                       "City of Nottingham"="Nottingham"
    ))

  s.result$city[stringr::str_detect(s.result$city,"London Borough of *")] <- "London"
  s.result$city[stringr::str_detect(s.result$name, "^London")] <- "London"

  cities <- read.csv(file.path("data","stations_validated.csv"))
  s.result <- s.result %>%
    select(-c(city)) %>%
    left_join(cities %>% distinct(id, city))


  s.sf <- sf::st_as_sf(s.result, coords=c("Longitude","Latitude"), crs=4326)

  required_cols <- c("id","name","city","country","timezone","source","geometry","type")

  s.upload <- s.sf %>% tibble %>%
    mutate(geometry=sf::st_as_text(geometry, EWKT=T)) %>%
    select_at(required_cols)

  s.sf %>%
    cbind(., st_coordinates(.)) %>%
    st_set_geometry(NULL) %>%
    write.csv(file.path("results","data","stations.csv"), row.names = F)

  s.sf %>% sf::st_write(file.path("results","data","stations.kml"), append=F)


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

  data.long$unit <- data.long$status
  data.long$unit[stringr::str_detect(data.long$unit,"ugm-3")] <- "µg/m3"
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
    dplyr::select(station=region_id, city, poll, unit, date, value, type, source, process_id) %>%
    arrange(station, poll, date)

  mc.running <- mc.city %>% rcrea::utils.rolling_average("day", running_days, "value") %>%
    dplyr::select(city=region_id, poll, unit, date, value, source, process_id) %>%
    arrange(city, poll, date)

  format <- function(x){ x %>% mutate(poll=paste0(poll," [",unit,"]"), date=lubridate::date(date)) %>%
      dplyr::select(-c(process_id, unit)) %>%
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
    dplyr::select(-c(observation, anomaly))

}


utils.anomaly_lockdown <- function(m, lockdown_running_day=7,
                                   process_anomaly="anomaly_gbm_lag1_city_mad",
                                   process_observation="city_day_mad"){

  if(!is.null(lockdown_running_day)){
    m.offset <- m %>%
      filter(process_id==process_anomaly) %>%
      dplyr::select_at(setdiff(names(.), "geometry")) %>% #running average doesn't work with geometry fields
      rcrea::utils.add_lockdown() %>%
      filter(date>=lubridate::date(movement)-lockdown_running_day,
             date<=lubridate::date(movement)) %>%
      group_by(region_id, country, poll) %>%
      dplyr::summarise(offset=mean(value, na.rm=T)) %>%
      ungroup()
  }else{
    m.offset <- tibble(region_id=unique(m$region_id), offset=0)
  }



  ren <- function(p){
    if(p==process_anomaly){return("anomaly")}
    if(p==process_observation){return("observation")}
    return(p)
  }

  m %>%
    filter(process_id %in% c(process_anomaly, process_observation)) %>%
    mutate(process_id=purrr::map_chr(process_id, ren),
           unit=gsub("Δ ","",unit)) %>%
    tidyr::pivot_wider(names_from=process_id, values_from=value) %>%
    left_join(m.offset) %>%
    mutate(anomaly_lockdown=anomaly-offset,
           anomaly_lockdown_relative=anomaly_lockdown/(observation-anomaly)) %>%
    dplyr::select(-c(anomaly,observation,offset)) %>%
    tidyr::pivot_longer(cols=c(anomaly_lockdown, anomaly_lockdown_relative), names_to="process_id")


}

utils.transport.apple <- function(){
  read.csv("data/applemobilitytrends.csv") %>%
    filter(geo_type=="city",
           country=="United Kingdom") %>%
    tidyr::pivot_longer(cols=!c(geo_type, region, transportation_type, alternative_name, sub.region, country),
                        names_to="date", names_pattern = "X(.*)") %>%
    mutate(date=lubridate::date(gsub("\\.","-",date)),
           region_id=tolower(region),
           country="GB")
}

utils.transport.mapbox <- function(date0="2020-03-23"){

  fs <- Sys.glob("data/mapbox/GB/adm2/total/2020/*/*/data/adm2.csv")
  t <- do.call("rbind",lapply(fs, read.csv)) %>%
    rename(region_code=geography, date=agg_day_period, value=activity_index_total) %>%
    mutate(date=lubridate::date(date))

  # Scale
  t.scaled <- t %>%
    # rcrea::utils.running_average(n_day_for_scaling) %>%
    filter(lubridate::date(date)<=lubridate::date(date0)) %>%
    group_by(region_code) %>%
    dplyr::summarise(mean_pre_lockdown=mean(value)) %>%
    right_join(t) %>%
    mutate(value=(value-mean_pre_lockdown)/mean_pre_lockdown,
           date=lubridate::date(date)) %>%
    select(region_code, date, value )

  # Add region_id
  c <- read.csv("data/mapbox_city.csv")

  c %>% filter(PUA != "#N/A") %>%
    rename(region_code=GSS_CD,
           region_id=PUA) %>%
    mutate(region_id=tolower(region_id),
           country="GB") %>%
    select(region_code, country, region_id) %>%
    inner_join(t.scaled)
}

