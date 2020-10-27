table_impact <- function(mc.city, tc, n_day, date_from, date_to){


  tc.equivalent <- tc %>%
    mutate(process_id="anomaly_lockdown_relative",
           region_id=tolower(city),
           poll="traffic",
           value=diffRatio) %>%
    select(region_id, process_id, date, poll, value)


  t.impact <- mc.city %>%
    filter(date>=lubridate::date(date_from)-n_day,
           date<=lubridate::date(date_to)) %>%
    rbind(utils.anomaly_lockdown(., n_day)) %>%
    bind_rows(tc.equivalent) %>%
    filter(process_id %in% c("anomaly_lockdown",
                             "anomaly_lockdown_relative")) %>%
    mutate(process_id=recode(process_id,
                             "anomaly_lockdown_relative"="relative",
                             "anomaly_lockdown"="absolute")) %>%
    filter(date>=lubridate::date(date_from),
           date<=lubridate::date(date_to)) %>%
    group_by(region_id, poll, process_id) %>%
    arrange(date) %>%
    mutate(value_rolled=zoo::rollapply(value, n_day, mean, align='right', fill=NA)) %>%
    dplyr::summarise(min=min(value_rolled, na.rm=T),
                     avg=mean(value, na.rm=T)) %>%
    tidyr::pivot_wider(names_from=c(process_id,poll), values_from=c(min, avg)) %>%
    arrange(avg_relative_no2)

  filename <- paste0("lockdown_impact_",n_day,"day_",
                     gsub("-","",date_from),"_",
                     gsub("-","",date_to),".csv")
  write.csv(t.impact, file.path("results","data", filename),
            na = "", row.names = F)

  return(t.impact)
}
