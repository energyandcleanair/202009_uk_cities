table_impact <- function(mc, tc.tomtom, tc.apple, tc.mapbox, date_from, date_to, save=T){

  # width <- ifelse(is.null(n_day), 1, n_day)
  width = 30

  tc.equivalent.tomtom <- tc.tomtom %>%
    mutate(process_id="relative_anomaly",
           poll="traffic_tomtom",
           value=traffic) %>%
    dplyr::select(region_id, process_id, date, poll, value)

  tc.equivalent.apple <- tc.apple %>%
    mutate(process_id="relative_anomaly",
           poll="traffic_apple",
           value=traffic) %>%
    dplyr::select(region_id, process_id, date, poll, value)

  tc.equivalent.mapbox <- tc.mapbox %>%
    mutate(process_id="relative_anomaly",
           poll="traffic_mapbox",
           value=value) %>%
    dplyr::select(region_id, process_id, date, poll, value)

  t.impact <- mc %>%
    filter(date>=lubridate::date(date_from), #-ifelse(!is.null(n_day), n_day, 0),
           date<=lubridate::date(date_to)) %>%

    # Old version: take n_day before lockdown as reference for anomaly
    # rbind(utils.anomaly_lockdown(., n_day)) %>%

    # New version: simply consider anomaly
    mutate(unit="µg/m3",
           process_id=recode(process_id,
                             "city_day_mad"="observed",
                             "anomaly_gbm_lag1_city_mad"="absolute_anomaly",
                             "anomaly_counterfactual_gbm_lag1_city_mad"="predicted"))  %>%
    filter(process_id %in% c("absolute_anomaly","relative_anomaly","predicted","observed")) %>%
    # tidyr::spread(process_id, value) %>%
    # mutate(relative=absolute/counterfactual) %>%
    # select(-c(counterfactual)) %>%
    # tidyr::gather("process_id", "value", absolute, relative) %>%

    # Version below should give similar results,
    # But tiny differences... Opting for the one above
    # to be compatible with charts
    # mutate(process_id=recode(process_id,
    #                   "anomaly_vs_counterfactual_gbm_lag1_city"="anomaly_lockdown_relative",
    #                   "anomaly_gbm_lag1_city_mad"="anomaly_lockdown")) %>%


    bind_rows(tc.equivalent.tomtom) %>%
    bind_rows(tc.equivalent.apple) %>%
    bind_rows(tc.equivalent.mapbox) %>%
    filter(process_id %in% c("absolute_anomaly",
                             "relative_anomaly",
                             "predicted",
                             "observed")) %>%
    filter(date>=lubridate::date(date_from),
           date<=lubridate::date(date_to)) %>%
    group_by(region_id, poll, process_id) %>%
    arrange(date) %>%
    # Min is not anymore computed using the right approach. Removing
    # mutate(value_rolled=zoo::rollapply(value, width, mean,  na.rm=T, align='right',fill=NA)) %>%
    dplyr::summarise(avg=mean(value, na.rm=T)) %>%
    tidyr::pivot_wider(names_from=c(process_id), values_from=c(avg), names_prefix="avg_") %>%
    mutate(
      avg_relative_anomaly=ifelse(is.na(avg_relative_anomaly), avg_absolute_anomaly / avg_predicted, avg_relative_anomaly)
    ) %>%
    tidyr::pivot_wider(names_from=c(poll), values_from=c(avg_relative_anomaly, avg_absolute_anomaly, avg_predicted, avg_predicted)) %>%
    arrange(avg_relative_anomaly_no2)

  if(save){
    filename <- paste0("lockdown_impact_",
                       # n_day,"day_",
                       gsub("-","",date_from),"_",
                       gsub("-","",date_to),".csv")
    write.csv(t.impact, file.path("results","data", filename),
              na = "", row.names = F)
  }

  return(t.impact)
}


export_measurements <- function(mc){
  write.csv(mc %>%
              dplyr::select(process_id, date, poll, unit, city, source, value) %>%
              filter(process_id=="city_day_mad"),
            file.path(dir_results_data, "measurements_observations.csv"), row.names = F)

  write.csv(mc %>%
              dplyr::select(process_id, date, poll, unit, city, source, value) %>%
              filter(process_id=="anomaly_offsetted_gbm_lag1_city_mad"),
            file.path(dir_results_data, "measurements_deweathered.csv"), row.names = F)

  write.csv(mc %>%
              dplyr::select(process_id, date, poll, unit, city, source, value) %>%
              filter(process_id=="anomaly_gbm_lag1_city_mad"),
            file.path(dir_results_data, "measurements_anomaly.csv"), row.names = F)

  write.csv(mc %>%
              dplyr::select(process_id, date, poll, unit, city, source, value) %>%
              filter(process_id=="anomaly_percent_gbm_lag1_city_mad"),
            file.path(dir_results_data, "measurements_anomaly_percent.csv"), row.names = F)

  write.csv(mc %>%
              dplyr::select(process_id, date, poll, unit, city, source, value) %>%
              filter(process_id=="anomaly_counterfactual_gbm_lag1_city_mad"),
            file.path(dir_results_data, "measurements_counterfactual.csv"), row.names = F)
}
