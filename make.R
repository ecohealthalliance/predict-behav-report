source(here::here("R/functions.R"))
source(here::here("R/packages.R"))


# User entered information
country <- "Indonesia" # Name country here
illness_outcomes <- c("ili", "sari")
taxa_outcomes <- c("bat", "rodent")

# get raw data 
dat <- get_raw_behav(country, download = FALSE)
write_csv(dat, h(paste0("data/raw-behav-", country, ".csv")))

# MAKE SURE site_name_lookup.csv is updated for your country
site_lookup <- read_csv(h("site-name-lookup.csv")) %>% 
  get_site_names(dat, .)

write_csv(site_lookup, h(paste0("data/site-name-formatted-", country, ".csv")))
