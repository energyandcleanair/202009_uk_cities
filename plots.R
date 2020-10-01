plot_city_vs_stations <- function(ms.city, mc.city, running_days, unit=NULL, file=NULL, plot_add=NULL, subtitle=NULL){

  source <- unique(c(ms.city$source, mc.city$source))


  if(!nrow(ms.city)){
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
                                                    process_id=="anomaly_percent_gbm_lag1_station"),
                         mc.city=mc.city %>% filter(poll==!!poll,
                                                    process_id=="anomaly_percent_gbm_lag1_city_mad"),
                         running_days=30,
                         unit="Anomaly [%]",
                         file=file.path(dir_results_plots,
                                        paste0("plot_",poll,"_anomaly_percent.png"))))

  (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
                                                    process_id=="station_day_mad") %>%
                           filter(date>="2018-01-01"),
                         mc.city=mc.city %>% filter(poll==!!poll,
                                                    process_id=="city_day_mad") %>%
                           filter(date>="2018-01-01"),
                         running_days=30,
                         unit="Observed concentration [µg/m3]",
                         subtitle="NO2 concentration levels. 30-day running average",
                         file=file.path(dir_results_plots,
                                        paste0("plot_",poll,"_observations.png")),
                         plot_add=ylim(0, NA)))
}
