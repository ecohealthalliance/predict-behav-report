# Load packages and functions 
purrr::walk(list.files(here::here("R/"), full.names = TRUE),
     source, echo = FALSE, verbose = FALSE)

# User entered information
country <- "Indonesia" # Name country here
illness_outcomes <- c("ili", "sari", "encephalitis") # Select illness of interest here (`illness_names_clean` object (loaded in environment) to see full list)
taxa_outcomes <- c("rodents", "bats") # Select taxa contact of interest (`taxa_names` object (loaded in environment) to see full list)

# get data 
dat <- get_behav(country, download = FALSE)
write_csv(dat, h(paste0("data/raw-behav-", country, ".csv")))

# get boolean values for non-numeric responses
ldat <- get_logical(dat)
write_csv(ldat, h(paste0("data/logic-behav-", country, ".csv")))

# for analysis, select only taxa and illness outcomes of interest
sdat <- get_outcomes(ldat, taxa_outcomes, illness_outcomes)
write_csv(sdat, h(paste0("data/logic-outcome-behav-", country, ".csv")))

#TODO lasso workflow - have run T/F option and separate script for the running and figures

# MAKE SURE site_name_lookup.csv is updated for your country
site_lookup <- read_csv(h("site-name-lookup.csv")) %>% 
  get_site_names(dat, ., country)

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




