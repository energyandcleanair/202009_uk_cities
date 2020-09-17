plot_city_vs_stations <- function(ms.city, mc.city, running_days, unit=NULL, file=NULL){

  d.plot <- bind_rows(ms.city %>% mutate(alpha=0),
                      ms.city %>%
                        distinct(city) %>%
                        left_join(mc.city) %>%
                        mutate(alpha=1)) %>%
    filter(!is.na(date)) %>%
    rcrea::utils.rolling_average("day",running_days,"value") %>%
    rcrea::utils.add_lockdown() %>%
    mutate(city=tools::toTitleCase(city))


  # Value at lockdown
  d.lockdown.level <- d.plot %>% filter(lubridate::date(date)==lubridate::date(movement), level=="city")

  unit <- d.plot$unit %>% unique()

  p <- ggplot(d.plot) +
    geom_line(aes(date, value, color=level, group=region_id, size=level, alpha=alpha)) +
    scale_color_brewer(palette="Spectral") +
    scale_size_manual(values=c(0.6,0.3,0.3,0.3)) +
    facet_wrap(~city, scales="free_y") +
    scale_alpha(range = c(0.8, 1), guide='none') +
    geom_hline(data=d.lockdown.level, aes(yintercept = value, linetype="Lockdown value"),
               color=rcrea::CREAtheme.pal_crea['Turquoise']) +
    rcrea::theme_crea() +
    labs(x=NULL,
         y=paste0(unit),
         subtitle=paste0("NO2 weather-corrected values. ", running_days,"-day running average"),
         caption="Source: CREA based on EEA")

  p <- p +
    geom_vline(data=d.plot, aes(xintercept=movement, linetype="Lockdown date"),
               color=rcrea::CREAtheme.pal_crea['Turquoise']) +
    # geom_vline(data=d.plot, aes(xintercept=partial_restriction, linetype="Partial restrictions"),
    #            color=rcrea::CREAtheme.pal_crea['Turquoise']) +
    scale_linetype_manual(values=c("dotted","dashed"), name=NULL)

  if(!is.null(file)){
    ggsave(file, plot=p, width=16, height=12)
  }

  return(p)
}
