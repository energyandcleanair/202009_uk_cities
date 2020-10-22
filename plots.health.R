plot_cost_causes <- function(hi.detailed, filename="plot_cost_causes.jpg"){

  ggplot(hi.detailed %>% mutate(region_id=tools::toTitleCase(region_id))) +
    geom_bar(stat="identity", aes(x=cost.mlnUSD*1E6/population,y=forcats::fct_rev(region_id),fill=Outcome)) +
    labs(title="Avoided health costs from air pollution",
         subtitle="Impact of COVID-19 related lockdown in selected cities",
         y=NULL,
         x="Avoided health cost (USD/capita)") +
    theme_crea() +
    rcrea::CREAtheme.scale_fill_crea_d(palette="dramatic")

  ggsave(file.path("results", "plots", filename), width=10, height=6)
}


plot_death <- function(hi.detailed, filename="plot_death.jpg"){

  ggplot(hi.detailed %>% mutate(region_id=tools::toTitleCase(region_id)) %>%
           filter(Outcome=="deaths")) +
    geom_bar(stat="identity", aes(x=number_central,y=forcats::fct_rev(region_id))) +
    labs(title="Avoided health impacts from air pollution",
         subtitle="Impact of COVID-19 related lockdown in selected cities",
         y=NULL,
         x="Avoided deaths") +
    theme_crea() +
    rcrea::CREAtheme.scale_fill_crea_d(palette="dramatic")

  ggsave(file.path("results", "plots", filename), width=10, height=6)
}


plot_causes <- function(hi.detailed, filename="plot_causes.jpg"){

  ggplot(hi.detailed %>%
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

  ggsave(file.path("results", "plots", filename), width=12, height=10, scale=1.5)

}
