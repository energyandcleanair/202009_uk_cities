plot_city_vs_stations <- function(ms.city, mc.city, running_days, unit=NULL, file=NULL, plot_add=NULL, subtitle=NULL){

  source <- unique(c(ms.city$source, mc.city$source))

  if(is.null(ms.city) || !nrow(ms.city)){
    # No stations
    d.raw <- mc.city %>% mutate(alpha=1)
  }else{
    d.raw <- bind_rows(ms.city %>% mutate(alpha=0),
                       ms.city %>%
                         distinct(city) %>%
                         left_join(mc.city) %>%
                         mutate(alpha=1))
  }

  d.plot <-  d.raw %>%
    filter(!is.na(date)) %>%
    rcrea::utils.rolling_average("day",running_days,"value") %>%
    rcrea::utils.add_lockdown() %>%
    mutate(city=tools::toTitleCase(city))


  # Value at lockdown
  d.lockdown.level <- d.plot %>% filter(lubridate::date(date)==lubridate::date(movement), level=="city")

  unit <- d.plot$unit %>% unique()
  poll <- toupper(d.plot$poll %>% unique())
  if(is.null(subtitle)){
    subtitle <- paste0(poll," weather-corrected values. ", running_days,"-day running average")
  }

  p <- ggplot(d.plot) +
    geom_line(aes(date, value, color=level, group=region_id, size=level, alpha=alpha)) +
    scale_color_manual(values=c(brewer.pal(8, "Spectral")[c(1,2,6,8)],"grey40")) +
    scale_size_manual(values=c(0.6,0.3,0.3,0.3,0.3)) +
    facet_wrap(~city, scales="free_y") +
    scale_alpha(range = c(0.8, 1), guide='none') +
    geom_hline(data=d.lockdown.level, aes(yintercept = value, linetype="Lockdown value"),
               color=rcrea::CREAtheme.pal_crea['Turquoise']) +
    rcrea::theme_crea() +
    labs(x=NULL,
         y=paste0(unit),
         subtitle=subtitle,
         caption=paste("Source: CREA based on", toupper(source)))

  p <- p +
    geom_vline(data=d.plot, aes(xintercept=movement, linetype="Lockdown date"),
               color=rcrea::CREAtheme.pal_crea['Turquoise']) +
    # geom_vline(data=d.plot, aes(xintercept=partial_restriction, linetype="Partial restrictions"),
    #            color=rcrea::CREAtheme.pal_crea['Turquoise']) +
    scale_linetype_manual(values=c("dotted","dashed"), name=NULL)

  if(!is.null(plot_add)){
    p <- p + plot_add
  }

  if(!is.null(file)){
    ggsave(file, plot=p, width=12, height=10)
  }

  return(p)
}


plot_poll <- function(poll, ms.city, mc.city){
  # (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
  #                                                   process_id=="anomaly_gbm_lag1_station"),
  #                        mc.city=mc.city %>% filter(poll==!!poll,
  #                                                   process_id=="anomaly_gbm_lag1_city_mad"),
  #                        running_days=30,
  #                        unit="Anomaly [µg/m3]",
  #                        file=file.path(dir_results_plots,
  #                                       paste0("plot_",poll,"_anomaly.png"))))

  poll_name <- recode(poll,
                      "no2"="NO2",
                      "pm25"="PM2.5",
                      "pm10"="PM10",
                      "o3"="O3",
                      .default = poll)

  (plot_city_vs_stations(ms.city=NULL,
                         mc.city=mc.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_offsetted_gbm_lag1_city_mad"),
                         running_days=30,
                         unit="Weather-corrected concentration [µg/m3]",
                         file=file.path(dir_results_plots,
                                        paste0("plot_",poll,"_city_deweathered.png")),
                         plot_add=ylim(0, NA)))

  (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_offsetted_gbm_lag1_station"),
                         mc.city=mc.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_offsetted_gbm_lag1_city_mad"),
                         running_days=30,
                         unit="Weather-corrected concentration [µg/m3]",
                         file=file.path(dir_results_plots,
                                        paste0("plot_",poll,"_deweathered.png")),
                         plot_add=ylim(0, NA)))

  (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
                                                    process_id=="station_day_mad") %>%
                           filter(date>="2018-01-01"),
                         mc.city=mc.city %>% filter(poll==!!poll,
                                                    process_id=="city_day_mad") %>%
                           filter(date>="2018-01-01"),
                         running_days=30,
                         unit="Observed concentration [µg/m3]",
                         subtitle=paste0(poll_name, " concentration levels. 30-day running average"),
                         file=file.path(dir_results_plots,
                                        paste0("plot_",poll,"_observations.png")),
                         plot_add=ylim(0, NA)))

  # Version representing anomaly
  (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_gbm_lag1_station"),
                         mc.city=mc.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_gbm_lag1_city_mad"),
                         running_days=30,
                         unit=paste0("Anomaly [Δ µg/m3]"),
                         file=file.path(dir_results_plots,
                                        paste0("plot_",poll,"_anomaly_vs_average.png"))))

  # Version representing anomaly with lockdown at 0
  (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_lockdown"),
                         mc.city=mc.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_lockdown"),
                         running_days=30,
                         unit=paste0("Anomaly [Δ µg/m3]"),
                         file=file.path(dir_results_plots,
                                        paste0("plot_",poll,"_anomaly_from_lockdown.png"))))

  (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_lockdown"),
                         mc.city=mc.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_lockdown"),
                         running_days=0,
                         unit=paste0("Anomaly [Δ µg/m3]"),
                         file=file.path(dir_results_plots,
                                        paste0("plot_",poll,"_anomaly_from_lockdown_0day.png"))))


  (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_lockdown"),
                         mc.city=mc.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_lockdown"),
                         running_days=0,
                         unit=paste0("Anomaly [Δ µg/m3]"),
                         file=file.path(dir_results_plots,
                                        paste0("plot_",poll,"_anomaly_from_lockdown.png"))))


  # Version representing anomaly vs counterfactual
  (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_percent_gbm_lag1_station"),
                         mc.city=mc.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_percent_gbm_lag1_city_mad"),
                         running_days=30,
                         unit="Anomaly [%]",
                         file=file.path(dir_results_plots,
                                        paste0("plot_",poll,"_anomaly_vs_average.png"))))



  # Version representing anomaly vs counterfactual
  (plot_city_vs_stations(ms.city=ms.city %>%
                           filter(poll==!!poll,
                                  process_id=="anomaly_vs_counterfactual_gbm_lag1_city") %>%
                           mutate(value=value*100),
                         mc.city=mc.city %>%
                           filter(poll==!!poll,
                                  process_id=="anomaly_vs_counterfactual_gbm_lag1_city") %>%
                           mutate(value=value*100),
                         running_days=30,
                         unit="Anomaly [%]",
                         file=file.path(dir_results_plots,
                                        paste0("plot_",poll,"_anomaly_vs_counterfactual.png"))))
}


plot_traffic_poll <- function(mc.city, tc, n_day){

  d.plot <- mc.city %>%
    mutate(date=lubridate::date(date),
           value=value/100) %>%
    filter(process_id=="anomaly_percent_gbm_lag1_city_mad",
           poll=="no2") %>%
    utils.add_lockdown() %>%
    right_join(tc %>%
                 mutate(date=lubridate::date(date),
                        region_id=tolower(city)) %>%
                 dplyr::select(-c(value, weekday, week, city)),
               by=c("region_id","country","date")) %>%
    dplyr::rename(no2=value, traffic=diffRatio) %>%
    mutate(movement=lubridate::date(movement),
           first_measures=lubridate::date(first_measures))

  d.plot <- d.plot %>%
    tidyr::pivot_longer(c(no2, traffic), names_to="indicator") %>%
    rcrea::utils.rolling_average("day", n_day, "value")

  ggplot(d.plot) +
    geom_line(aes(date,value,col=indicator)) +
    facet_wrap(~region_id) +
    scale_y_continuous(labels=scales::percent) +
    theme_light() +
    geom_vline(data=d.plot, aes(xintercept=movement, linetype="National lockdown"),
               color=rcrea::CREAtheme.pal_crea['Turquoise']) +
    geom_vline(data=d.plot, aes(xintercept=partial_restriction, linetype="Partial restrictions"),
               color=rcrea::CREAtheme.pal_crea['Turquoise']) +
    scale_linetype_manual(values=c("dashed","dotted"), name=NULL) +
    labs(
      subtitle=paste0("2020 vs 2019 NO2concentration and traffic congestion levels\n",
                      n_day,"-day running average"),
      caption="Source: CREA based on DEFRA and TomTom, using weather-corrected values",
      y="Year-on-year variation",
      x=NULL)

  ggsave(file.path(dir_results_plots, paste0("no2.traffic.",n_day,"day.png")), width=10, height=8)

}

plot_corr_traffic_poll <- function(mc.city, tc){

  d.plot <- mc.city %>%
    mutate(date=lubridate::date(date)) %>%
    filter(process_id=="anomaly_rel_counterfactual",
           poll=="no2") %>%
    utils.add_lockdown() %>%
    right_join(tc %>%
                 mutate(date=lubridate::date(date),
                        region_id=tolower(city)) %>%
                 dplyr::select(-c(value, weekday, week, city)),
               by=c("region_id","country","date")) %>%
    dplyr::rename(no2=value, traffic=diffRatio) %>%
    mutate(movement=lubridate::date(movement),
           first_measures=lubridate::date(first_measures)) %>%
    filter(date>=movement) %>%
    rcrea::utils.rolling_average("day", 30, c("no2","traffic")) %>%
    group_by(region_id) %>%
    summarise_at(c("no2","traffic"), min, na.rm=T)

  ggplot(d.plot) + geom_point(aes(traffic, no2))
}

plot_anomaly_average <- function(m, process_anomaly="anomaly_lockdown", date_from="2020-03-23", date_to="2020-12-31", filename=paste("anomaly_average.jpg")){
  (plt <- ggplot(m %>%
           filter(process_id==process_anomaly,
                  date>=date_from,
                  date<=date_to) %>%
           dplyr::group_by(region_id, poll) %>%
           dplyr::summarise(value=mean(value, na.rm=T))) +
    geom_bar(stat = "identity", aes(x=poll, y=value, fill=poll)) +
    facet_wrap(~region_id) +
    theme_light() +
    geom_hline(yintercept=0))

  ggsave(file.path("results", "plots", filename), plot = plt, width=10, height=6)
  plt

}

plot_anomaly_min <- function(m, process_anomaly="anomaly_lockdown", date_from="2020-03-23", date_to="2020-12-31", filename=paste("anomaly_average.jpg")){
  (plt <- ggplot(m %>%
                   filter(process_id==process_anomaly,
                          date>=date_from,
                          date<=date_to) %>%
                   dplyr::group_by(region_id, poll) %>%
                   dplyr::summarise(value=mean(value, na.rm=T))) +
     geom_bar(stat = "identity", aes(x=poll, y=value, fill=poll)) +
     facet_wrap(~region_id) +
     theme_light() +
     geom_hline(yintercept=0))

  ggsave(file.path("results", "plots", filename), plot = plt, width=10, height=6)
  plt

}
