#-------------------------------------------------------------
# Load packages and functions 
purrr::walk(list.files(here::here("R/"), full.names = TRUE),
            source, echo = FALSE, verbose = FALSE)
set.seed(99)
#-------------------------------------------------------------
# User entered information
country <- "Indonesia" # Name country here
illness_outcomes <- c("ili") # Select illness of interest here (`illness_names_clean` object (loaded in environment) to see full list)
taxa_outcomes <- c("rodents", "bats") # Select taxa contact of interest (`taxa_names` object (loaded in environment) to see full list)

# Confirm endpoints are valid
assertthat::assert_that(all(illness_outcomes %in% illness_names_clean), msg = "one or more illness_outcomes is not recognized")
assertthat::assert_that(all(taxa_outcomes %in% taxa_names), msg = "one or more taxa_outcomes is not recognized")

# Breaks for discretizing continuous variables
age_breaks <- c(0, 18, 41, 61, Inf)
age_labels <- c("under_18", "18_to_40", "41_to_60", "over_60")
crowding_index_breaks <- c(0, 1, 3, 10000, Inf)
crowding_index_labels <- c("less_than_1", "1_to_3", "3_plus", "no_rooms")

# Download data from EIDITH? (must have local eidith if FALSE)
download_fresh <- FALSE

# Run the lasso analysis?  (will be slow, depending on the number of outcomes, should only be run once or whenever there are changes to the data)
run_lasso <- FALSE 
#-------------------------------------------------------------

#-------------------------------------------------------------
# Get data (can specify download = TRUE if needed)
dat <- get_behav(country, download = download_fresh) 
write_csv(dat, h(paste0("data/raw-behav-", country, ".csv"))) # used in summary report

# Format data for analysis 
ldat <- dat %>%
  get_logical(.) %>% # Get boolean values for non-numeric responses
  discretize_continuous(., age_breaks, age_labels, crowding_index_breaks, crowding_index_labels)  # Discretize numeric responses (this can be modified above)
write_csv(ldat, h(paste0("data/logic-behav-", country, ".csv"))) # used in heat maps and lasso

tdat <- dat %>%
  get_tab(.)
write_csv(tdat, h(paste0("data/tab-behav-", country, ".csv"))) # used in tabular report

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

# Tabular Report
rmarkdown::render(h("scripts/02-tabular-report.Rmd"),
                  output_file = paste0(country, "-behav-tabular-report.html"),
                  output_dir = h("outputs", "reports"),
                  params = list(country = country))

# Site Maps
rmarkdown::render(h("scripts/03-site-maps.Rmd"),
                   output_file = paste0(country, "-behav-site-maps.html"),
                   output_dir = h("outputs", "reports"),
                   params = list(country = country))

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

