plot_cost_causes <- function(hi.detailed, filename="plot_cost_causes.jpg"){

  plt <- ggplot(hi.detailed %>%
                  mutate(region_id=tools::toTitleCase(region_id)) %>%
                  group_by(region_id, Outcome) %>%
                  dplyr::summarise(cost.mlnUSD=sum(cost.mlnUSD, na.rm=T),
                            population=mean(population, na.rm=T))) +
    geom_bar(stat="identity", aes(x=cost.mlnUSD*1E6/population,y=forcats::fct_rev(region_id),fill=Outcome)) +
    labs(title="Avoided health costs from air pollution",
         subtitle="Impact of COVID-19 related lockdown in selected cities",
         y=NULL,
         x="Avoided health cost (USD/capita)") +
    theme_crea() +
    rcrea::CREAtheme.scale_fill_crea_d(palette="dramatic")

  ggsave(file.path("results", "plots", filename), plot = plt, width=10, height=6)
  plt
}


plot_death <- function(hi.detailed, filename="plot_death.jpg"){

  plt <- ggplot(hi.detailed %>%
                  mutate(region_id=tools::toTitleCase(region_id)) %>%
                  group_by(region_id, Outcome) %>%
                  summarise_at("number_central", sum, na.rm=T) %>%
                  filter(Outcome=="deaths")) +
    geom_bar(stat="identity", aes(x=number_central,y=forcats::fct_rev(region_id))) +
    labs(title="Avoided health impacts from air pollution",
         subtitle="Impact of COVID-19 related lockdown in selected cities",
         y=NULL,
         x="Avoided deaths") +
    theme_crea() +
    rcrea::CREAtheme.scale_fill_crea_d(palette="dramatic")

  ggsave(file.path("results", "plots", filename), plot = plt, width=10, height=6)
  plt
}


plot_causes <- function(hi.detailed, filename="plot_causes.jpg"){

  plt <- ggplot(hi.detailed %>%
           mutate_at(c("region_id", "Outcome"), tools::toTitleCase) %>%
           group_by(region_id, Outcome) %>%
           summarise_at("number_central", sum, na.rm=T)) +
    geom_bar(stat="identity", aes(x=number_central,y=forcats::fct_rev(region_id))) +
    labs(title="Avoided health impacts from air pollution",
         subtitle="Impact of COVID-19 related lockdown in selected cities",
         y=NULL,
         x=NULL) +
    facet_wrap(~Outcome, scales="free_x") +
    theme_crea() +
    rcrea::CREAtheme.scale_fill_crea_d(palette="dramatic")

  ggsave(file.path("results", "plots", filename), plot = plt, width=12, height=10, scale=1.5)
  plt

}
