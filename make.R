purrr::walk(list.files(here::here("R/"), full.names = TRUE),
     source, echo = FALSE, verbose = FALSE)

# User entered information
country <- "Indonesia" # Name country here
illness_outcomes <- c("ili", "sari")
taxa_outcomes <- c("bat", "rodent")

# get raw data 
dat <- get_behav(country, download = FALSE)
write_csv(dat, h(paste0("data/raw-behav-", country, ".csv")))

# MAKE SURE site_name_lookup.csv is updated for your country
site_lookup <- read_csv(h("site-name-lookup.csv")) %>% 
  get_site_names(dat, .)

write_csv(site_lookup, h(paste0("data/site-name-formatted-", country, ".csv")))

# Run summary report
rmarkdown::render(h("scripts/01-summary-report.Rmd"),
                  output_file = paste0(country, "-behav-summary-report.html"),
                  output_dir = h("summary-report"),
                  params = list(country = country))




