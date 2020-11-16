require(rcrea)
require(dplyr)
require(ggplot2)
require(rdefra)
require(zoo)
require(revgeo)
require(openxlsx)
require(RColorBrewer)
require(sf)
require(ggrepel)

dir_results <- "results"
dir.create(dir_results, showWarnings = F)

dir_results_plots_poll <- file.path(dir_results, "plots", "poll")
dir.create(dir_results_plots_poll, showWarnings = F, recursive = T)

dir_results_plots_health <- file.path(dir_results, "plots", "health")
dir.create(dir_results_plots_health, showWarnings = F, recursive = T)

dir_results_plots_traffic <- file.path(dir_results, "plots", "traffic")
dir.create(dir_results_plots_traffic, showWarnings = F, recursive = T)

dir_results_data <- file.path(dir_results, "data")
dir.create(dir_results_data, showWarnings = F, recursive = T)

country <- "GB"
city <- c(
  "Aberdeen","Aldershot","Barnsley","Basildon","Belfast","Birkenhead","Birmingham","Blackburn","Blackpool","Bournemouth","Bradford","Brighton","Bristol","Burnley","Cambridge","Cardiff","Chatham","Coventry","Crawley","Derby","Doncaster","Dundee","Edinburgh","Exeter","Glasgow","Gloucester","Huddersfield","Hull","Ipswich","Leeds","Leicester","Liverpool","London","Luton","Manchester","Mansfield","Middlesbrough","Milton Keynes","Newcastle","Newport","Northampton","Norwich","Nottingham","Oxford","Peterborough","Plymouth","Portsmouth","Preston","Reading","Sheffield","Slough","Southampton","Southend","Stoke","Sunderland","Swansea","Swindon","Telford","Wakefield","Warrington","Wigan","Worthing","York"
)

