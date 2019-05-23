#-------------------------------------------------------------
# Load packages and functions 
purrr::walk(list.files(here::here("R/"), full.names = TRUE),
            source, echo = FALSE, verbose = FALSE)
#-------------------------------------------------------------
# User entered information
country <- "Indonesia" # Name country here
illness_outcomes <- c("ili") # Select illness of interest here (`illness_names_clean` object (loaded in environment) to see full list)
taxa_outcomes <- c("rodents", "bats") # Select taxa contact of interest (`taxa_names` object (loaded in environment) to see full list)

age_breaks <- c(0, 18, 41, 61, Inf)
age_labels <- c("under_18", "18_to_40", "41_to_60", "over_60")
crowding_index_breaks <- c(0, 1, 3, 10000, Inf)
crowding_index_labels <- c("less_than_1", "1_to_3", "3_plus", "no_rooms")

download_fresh <- FALSE
run_lasso <- FALSE # running the lasso may be slow
#-------------------------------------------------------------
# Get data (can specify download = TRUE if needed)
dat <- get_behav(country, download = download_fresh)
write_csv(dat, h(paste0("data/raw-behav-", country, ".csv")))

# Format data for analysis
ldat <- dat %>%
  get_logical(.) %>% # Get boolean values for non-numeric responses
  discretize_continuous(., age_breaks, age_labels, crowding_index_breaks, crowding_index_labels)  # Discretize numeric responses (this can be modified above)
write_csv(ldat, h(paste0("data/logic-behav-", country, ".csv")))

# Select only taxa and illness outcomes of interest
# sdat <- ldat %>%
#   select(-participant_id) %>%
#   get_outcomes(., taxa_outcomes, illness_outcomes) 
# write_csv(sdat, h(paste0("data/logic-outcome-behav-", country, ".csv")))

# Load and process site names
site_lookup <- read_csv(h("site-name-lookup.csv")) %>% 
  get_site_names(dat, ., country)
write_csv(site_lookup, h(paste0("data/site-name-formatted-", country, ".csv")))

#-------------------------------------------------------------
# Run reports

# Summary Report
rmarkdown::render(h("scripts/01-summary-report.Rmd"),
                  output_file = paste0(country, "-behav-summary-report.html"),
                  output_dir = h("outputs", "reports"),
                  params = list(country = country))

# rmarkdown::render(h("scripts/03-site-maps-report.Rmd"),
#                   output_file = paste0(country, "-behav-site-maps-report.html"),
#                   output_dir = h("outputs", "reports"),
#                   params = list(country = country))

# Heatmap 
rmarkdown::render(h("scripts/04-heatmaps.Rmd"),
                  output_file = paste0(country, "-behav-heatmaps.html"),
                  output_dir = h("outputs", "reports"),
                  params = list(country = country))

# Lasso
# First, run the lasso for all outcome variables if specified
if(run_lasso){
  lasso <- get_lasso(ldat, illness_outcomes, taxa_outcomes)
}

rmarkdown::render(h("scripts/05-lasso.Rmd"),
                  output_file = paste0(country, "-behav-lasso.html"),
                  output_dir = h("outputs", "reports"),
                  params = list(country = country,
                                illness_outcomes = illness_outcomes,
                                taxa_outcomes = taxa_outcomes))

