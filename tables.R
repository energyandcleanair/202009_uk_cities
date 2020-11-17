table_impact <- function(mc.city, tc.tomtom, tc.apple, tc.mapbox, n_day, date_from, date_to, save=T){

  width <- ifelse(is.null(n_day), 1, n_day)

  tc.equivalent.tomtom <- tc.tomtom %>%
    mutate(process_id="anomaly_lockdown_relative",
           region_id=tolower(city),
           poll="traffic_tomtom",
           value=diffRatio) %>%
    dplyr::select(region_id, process_id, date, poll, value)

  tc.equivalent.apple <- tc.apple %>%
    mutate(process_id="anomaly_lockdown_relative",
           poll="traffic_apple",
           value=value/100-1) %>%
    dplyr::select(region_id, process_id, date, poll, value)

  tc.equivalent.mapbox <- tc.mapbox %>%
    mutate(process_id="anomaly_lockdown_relative",
           poll="traffic_mapbox",
           value=value) %>%
    dplyr::select(region_id, process_id, date, poll, value)


  t.impact <- mc.city %>%
    filter(date>=lubridate::date(date_from)-ifelse(!is.null(n_day), n_day, 0),
           date<=lubridate::date(date_to)) %>%
    rbind(utils.anomaly_lockdown(., n_day)) %>%

    bind_rows(tc.equivalent.tomtom) %>%
    bind_rows(tc.equivalent.apple) %>%
    bind_rows(tc.equivalent.mapbox) %>%
    filter(process_id %in% c("anomaly_lockdown",
                             "anomaly_lockdown_relative")) %>%
    mutate(process_id=recode(process_id,
                             "anomaly_lockdown_relative"="relative",
                             "anomaly_lockdown"="absolute")) %>%
    filter(date>=lubridate::date(date_from),
           date<=lubridate::date(date_to)) %>%
    group_by(region_id, poll, process_id) %>%
    arrange(date) %>%
    mutate(value_rolled=value, zoo::rollapply(value, width, mean,  na.rm=T, align='right',fill=NA)) %>%
    dplyr::summarise(min=min(value_rolled, na.rm=T),
                     avg=mean(value, na.rm=T)) %>%
    tidyr::pivot_wider(names_from=c(process_id,poll), values_from=c(min, avg)) %>%
    arrange(avg_relative_no2)

  if(save){
    filename <- paste0("lockdown_impact_",n_day,"day_",
                       gsub("-","",date_from),"_",
                       gsub("-","",date_to),".csv")
    write.csv(t.impact, file.path("results","data", filename),
              na = "", row.names = F)
  }

  return(t.impact)
}


export_measurements <- function(mc.city){
  write.csv(mc.city %>%
              dplyr::select(process_id, date, poll, unit, city, source, value) %>%
              filter(process_id=="city_day_mad"),
            file.path(dir_results_data, "measurements_observations.csv"), row.names = F)

  write.csv(mc.city %>%
              dplyr::select(process_id, date, poll, unit, city, source, value) %>%
              filter(process_id=="anomaly_offsetted_gbm_lag1_city_mad"),
            file.path(dir_results_data, "measurements_deweathered.csv"), row.names = F)

  write.csv(mc.city %>%
              dplyr::select(process_id, date, poll, unit, city, source, value) %>%
              filter(process_id=="anomaly_gbm_lag1_city_mad"),
            file.path(dir_results_data, "measurements_anomaly.csv"), row.names = F)

  write.csv(mc.city %>%
              dplyr::select(process_id, date, poll, unit, city, source, value) %>%
              filter(process_id=="anomaly_percent_gbm_lag1_city_mad"),
            file.path(dir_results_data, "measurements_anomaly_percent.csv"), row.names = F)

  write.csv(mc.city %>%
              dplyr::select(process_id, date, poll, unit, city, source, value) %>%
              filter(process_id=="anomaly_counterfactual_gbm_lag1_city_mad"),
            file.path(dir_results_data, "measurements_counterfactual.csv"), row.names = F)
}
