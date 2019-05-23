purrr::walk(list.files(here::here("R/"), full.names = TRUE),
     source, echo = FALSE, verbose = FALSE)

# User entered information
country <- "Indonesia" # Name country here
illness_outcomes <- c("ili", "sari", "encephalitis") #select illness of interest here (can select 0 through 3)
taxa_outcomes <- c('rodents', 'nhp',  'bats', 'swine',   'poultry',
                   'birds', 'cattle', 'goats_sheep', 'carnivores',
                   'camels', 'pangolins', 'ungulates', 'dogs', 'cats') #select taxa contact of interest (can select 0 through 14)

# get data 
dat <- get_behav(country, download = FALSE)
write_csv(dat, h(paste0("data/raw-behav-", country, ".csv")))

fdat <- get_formatted_dat(dat, taxa_names = taxa_outcomes)
write_csv(fdat, h(paste0("data/formatted-behav-", country, ".csv")))

# MAKE SURE site_name_lookup.csv is updated for your country
site_lookup <- read_csv(h("site-name-lookup.csv")) %>% 
  get_site_names(dat, .)

write_csv(site_lookup, h(paste0("data/site-name-formatted-", country, ".csv")))

# Run reports

## Summary Report
rmarkdown::render(h("scripts/01-summary-report.Rmd"),
                  output_file = paste0(country, "-behav-summary-report.html"),
                  output_dir = h("outputs", "reports"),
                  params = list(country = country))


rmarkdown::render(h("scripts/04-heatmaps.Rmd"),
                  output_file = paste0(country, "-behav-heatmaps.html"),
                  output_dir = h("outputs", "reports"),
                  params = list(country = country))




