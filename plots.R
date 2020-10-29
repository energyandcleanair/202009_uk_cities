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

plot_predicted_vs_observed <- function(mc.city, poll, date_from, date_to){

  poll_name <- recode(poll,
                      "no2"="NO2",
                      "pm25"="PM2.5",
                      "pm10"="PM10",
                      "o3"="O3",
                      .default = poll)

  d.plot <- mc.city %>%
    filter(poll==!!poll,
           process_id %in% c("city_day_mad",
                                       "anomaly_counterfactual_gbm_lag1_city_mad")) %>%
    mutate(process_id=recode(process_id,
                             "city_day_mad"="Observed",
                             "anomaly_counterfactual_gbm_lag1_city_mad"="Predicted"),
           region_id=tools::toTitleCase(region_id)) %>%
    filter(date>=date_from,
           date<=date_to) %>%
    group_by(region_id, process_id,  unit, poll) %>%
    dplyr::summarise(value=mean(value, na.rm=T))

  predicted <- d.plot[d.plot$process_id=="Predicted",]

  d.plot$region_id <- factor(as.character(d.plot$region_id), levels=levels(reorder(predicted$region_id, predicted$value)))

  (plt <- ggplot(d.plot %>% filter(!is.na(region_id))) +
    geom_bar(stat = "identity",
             aes(y=region_id, x=value, fill=process_id),
             position = "dodge") +
    theme_light() +
    rcrea::CREAtheme.scale_fill_crea_d(name=NULL)+
    labs(y=NULL,
         x=paste0(poll_name," concentration levels [",unique(d.plot$unit),"]"),
         title=paste("Air pollution during lockdown vs. predicted"),
         caption=paste0("Source: CREA based on DEFRA. Lockdown period considered: ",
                        date_from, " to ", date_to)))

  ggsave(file.path(dir_results_plots_poll, paste0("poll_",poll,"_bar_predicted_vs_observed.png")),
         plt,
         width=8, height=8)
  return(plt)

}

plot_poll <- function(poll, ms.city, mc.city, days=c(0, 14, 30)){
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

  for(day in days){

    (plot_city_vs_stations(ms.city=NULL,
                           mc.city=mc.city %>% filter(poll==!!poll,
                                                      process_id=="anomaly_offsetted_gbm_lag1_city_mad"),
                           running_days=day,
                           unit="Weather-corrected concentration [µg/m3]",
                           file=file.path(dir_results_plots_poll,
                                          paste0("plot_",poll,"_city_deweathered_",day,"day.png")),
                           plot_add=ylim(0, NA)))

    (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
                                                      process_id=="anomaly_offsetted_gbm_lag1_station"),
                           mc.city=mc.city %>% filter(poll==!!poll,
                                                      process_id=="anomaly_offsetted_gbm_lag1_city_mad"),
                           running_days=day,
                           unit="Weather-corrected concentration [µg/m3]",
                           file=file.path(dir_results_plots_poll,
                                          paste0("plot_",poll,"_deweathered_",day,"day.png")),
                           plot_add=ylim(0, NA)))

    (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
                                                      process_id=="station_day_mad") %>%
                             filter(date>="2018-01-01"),
                           mc.city=mc.city %>% filter(poll==!!poll,
                                                      process_id=="city_day_mad") %>%
                             filter(date>="2018-01-01"),
                           running_days=day,
                           unit="Observed concentration [µg/m3]",
                           subtitle=paste0(poll_name, " concentration levels. 30-day running average"),
                           file=file.path(dir_results_plots_poll,
                                          paste0("plot_",poll,"_observations_",day,"day.png")),
                           plot_add=ylim(0, NA)))

    # Version representing anomaly
    (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
                                                      process_id=="anomaly_gbm_lag1_station"),
                           mc.city=mc.city %>% filter(poll==!!poll,
                                                      process_id=="anomaly_gbm_lag1_city_mad"),
                           running_days=day,
                           unit=paste0("Anomaly [Δ µg/m3]"),
                           file=file.path(dir_results_plots_poll,
                                          paste0("plot_",poll,"_anomaly_vs_average_",day,"day.png"))))

    # Version representing anomaly with lockdown at 0
    # (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
    #                                                   process_id=="anomaly_lockdown"),
    #                        mc.city=mc.city %>% filter(poll==!!poll,
    #                                                   process_id=="anomaly_lockdown"),
    #                        running_days=30,
    #                        unit=paste0("Anomaly [Δ µg/m3]"),
    #                        file=file.path(dir_results_plots,
    #                                       paste0("plot_",poll,"_anomaly_from_lockdown.png"))))
    #
    # (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
    #                                                   process_id=="anomaly_lockdown"),
    #                        mc.city=mc.city %>% filter(poll==!!poll,
    #                                                   process_id=="anomaly_lockdown"),
    #                        running_days=0,
    #                        unit=paste0("Anomaly [Δ µg/m3]"),
    #                        file=file.path(dir_results_plots,
    #                                       paste0("plot_",poll,"_anomaly_from_lockdown_0day.png"))))
    #
    #
    # (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
    #                                                   process_id=="anomaly_lockdown"),
    #                        mc.city=mc.city %>% filter(poll==!!poll,
    #                                                   process_id=="anomaly_lockdown"),
    #                        running_days=0,
    #                        unit=paste0("Anomaly [Δ µg/m3]"),
    #                        file=file.path(dir_results_plots,
    #                                       paste0("plot_",poll,"_anomaly_from_lockdown.png"))))


    # Version representing anomaly vs average
    (plot_city_vs_stations(ms.city=ms.city %>% filter(poll==!!poll,
                                                      process_id=="anomaly_percent_gbm_lag1_station"),
                           mc.city=mc.city %>% filter(poll==!!poll,
                                                      process_id=="anomaly_percent_gbm_lag1_city_mad"),
                           running_days=day,
                           unit="Anomaly [%]",
                           file=file.path(dir_results_plots_poll,
                                          paste0("plot_",poll,"_anomaly_vs_average_",day,"day.png"))))



    # Version representing anomaly vs counterfactual
    (plot_city_vs_stations(ms.city=ms.city %>%
                             filter(poll==!!poll,
                                    process_id=="anomaly_vs_counterfactual_gbm_lag1_city") %>%
                             mutate(value=value*100),
                           mc.city=mc.city %>%
                             filter(poll==!!poll,
                                    process_id=="anomaly_vs_counterfactual_gbm_lag1_city") %>%
                             mutate(value=value*100),
                           running_days=day,
                           unit="Anomaly [%]",
                           file=file.path(dir_results_plots_poll,
                                          paste0("plot_",poll,"_anomaly_vs_counterfactual_",day,"day.png"))))
  }
}


# Traffic -----------------------------------------------------------------



plot_traffic_poll_tomtom <- function(mc.city, tc, n_day){

  d.plot <- mc.city %>%
    mutate(date=lubridate::date(date),
           value=value/100) %>%
    filter(process_id=="anomaly_percent_gbm_lag1_city_mad",
           poll=="no2") %>%
    dplyr::select(region_id, country, date, no2=value) %>%
    filter(region_id %in% unique(tolower(tc$city))) %>%
    full_join(tc %>%
                mutate(ddate=lubridate::date(date),
                       region_id=tolower(city)) %>%
                dplyr::select(region_id, country, date, traffic=diffRatio),
              by=c("region_id","country","date")) %>%
    utils.add_lockdown() %>%
    mutate(movement=lubridate::date(movement),
           first_measures=lubridate::date(first_measures))%>%
    tidyr::pivot_longer(c(no2, traffic), names_to="indicator")

  d.plot.avg <- d.plot %>%
    rcrea::utils.rolling_average("day", n_day, "value")


  plt <- ggplot(d.plot.avg) +
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
      subtitle=paste0("2020 vs 2019 NO2 concentration and traffic congestion levels\n",
                      n_day,"-day running average"),
      caption="Source: CREA based on DEFRA and TomTom, using weather-corrected values",
      y="Year-on-year variation",
      x=NULL)

  ggsave(file.path(dir_results_plots_traffic, paste0("no2.traffic.tomtom.",n_day,"day.png")),
         plot=plt,
         width=10,
         height=8)

  return(plt)
}

plot_traffic_poll_apple <- function(mc.city, tc, n_day){

  d.plot <- mc.city %>%
    mutate(date=lubridate::date(date),
           value=value/100) %>%
    filter(process_id=="anomaly_percent_gbm_lag1_city_mad",
           poll=="no2") %>%
    dplyr::select(region_id, country, date, no2=value) %>%
    filter(region_id %in% unique(tc$region_id)) %>%
    full_join(tc %>%
                 filter(transportation_type=="driving") %>%
                 mutate(date=lubridate::date(date),
                        traffic=value/100-1) %>%
                 dplyr::select(region_id, country, date, traffic),
               by=c("region_id","country","date")) %>%
    utils.add_lockdown() %>%
    mutate(movement=lubridate::date(movement),
           first_measures=lubridate::date(first_measures))%>%
    tidyr::pivot_longer(c(no2, traffic), names_to="indicator")

  d.plot.avg <- d.plot %>%
    rcrea::utils.rolling_average("day", n_day, "value")

  (plt <- ggplot(d.plot.avg) +
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
      subtitle=paste0("Impact of lockdown on NO2 and traffic levels\n",
                      n_day,"-day running average"),
      caption="Source: CREA based on DEFRA and Apple Mobility, using weather-corrected values",
      y="Year-on-year variation",
      x=NULL))

  ggsave(file.path(dir_results_plots_traffic, paste0("no2.traffic.apple.",n_day,"day.png")),
         plot = plt,
         width=10,
         height=8)

  return(plt)
}

plot_corr_traffic_poll_tomtom <- function(mc.city, tc.tomtom, tc.apple, date_from, date_to){
  t.impact <- table_impact(mc.city, tc.tomtom, tc.apple, n_day=NULL, date_from = date_from, date_to=date_to, save=F)

  plt <- ggplot(t.impact %>% filter(region_id!="edinburgh"),
         aes(x=avg_relative_traffic_tomtom, y=avg_relative_no2)) +
    geom_point() +
    geom_text_repel(aes(label = tools::toTitleCase(region_id)),  show.legend = FALSE,
                  box.padding = unit(0.45, "lines")) +
    stat_smooth(method="lm", col="red") +
    theme_light() +
    scale_x_continuous(label=scales::percent) +
    scale_y_continuous(label=scales::percent) +
    labs(title="Average anomalies during lockdown",
         y="NO2 level",
         x="Traffic congestion",
         caption=paste("Source: CREA based on DEFRA and TomTom"))

  ggsave(file.path(dir_results_plots_traffic, "no2.traffic.corr.tomtom.png"),
    plot = plt,
    width=8,
    height=6)

  return(plt)
}


plot_corr_traffic_poll_apple <- function(mc.city, tc.tomtom, tc.apple, date_from, date_to){

  t.impact <- table_impact(mc.city, tc.tomtom, tc.apple, n_day=NULL, date_from = date_from, date_to=date_to, save=F)

  plt <- ggplot(t.impact %>% filter(region_id!="edinburgh"),
                aes(x=avg_relative_traffic_apple, y=avg_relative_no2)) +
    geom_point() +
    geom_text_repel(aes(label = tools::toTitleCase(region_id)),  show.legend = FALSE,
                    box.padding = unit(0.45, "lines")) +
    stat_smooth(method="lm", col="red") +
    theme_light() +
    scale_x_continuous(label=scales::percent) +
    scale_y_continuous(label=scales::percent) +
    labs(title="Average anomalies during lockdown",
         y="NO2 level",
         x="Driving index",
         caption=paste("Source: CREA based on DEFRA and Apple Mobility"))

  ggsave(file.path(dir_results_plots_traffic, "no2.traffic.corr.apple.png"),
         plot = plt,
         width=8,
         height=6)

  return(plt)
}


plot_anomaly_average <- function(m, process_anomaly="anomaly_lockdown", date_from="2020-03-23", date_to="2020-12-31", filename=paste("plot_anomaly_lockdown.jpg")){
  (plt <- ggplot(m %>%
           filter(process_id==process_anomaly,
                  date>=date_from,
                  date<=date_to) %>%
             mutate(region_id=tools::toTitleCase(region_id)) %>%
           dplyr::group_by(region_id, poll) %>%
           dplyr::summarise(value=mean(value, na.rm=T))) +
    geom_bar(stat = "identity", aes(x=poll, y=value, fill=poll)) +
    facet_wrap(~region_id) +
    theme_crea() +
     scale_fill_brewer(name=NULL, palette="Spectral") +
     labs(subtitle="Impact of lockdown on pollutant levels",
          y="µg/m3",
          x=NULL) +
    geom_hline(yintercept=0))

  ggsave(file.path(dir_results_plots_poll, filename), plot = plt, width=10, height=8)
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

  ggsave(file.path(dir_results_plots_poll, filename), plot = plt, width=10, height=6)
  plt

}
