# Process Data

# Get data (can specify download = TRUE if needed)
dat <- get_behav(country, download = download_fresh) 
write_csv(dat, h(paste0("data/raw-behav-", country, ".csv"))) # used in summary report and site maps

# Format data for analysis 
ldat <- dat %>%
  get_logical(.) %>% # Get boolean values for non-numeric responses
  discretize_continuous(., age_breaks, age_labels, crowding_index_breaks, crowding_index_labels)  # Discretize numeric responses (this can be modified above)
write_csv(ldat, h(paste0("data/logic-behav-", country, ".csv"))) # used in heat maps and lasso

tdat <- dat %>%
  get_tab(.)
write_csv(tdat, h(paste0("data/tab-behav-", country, ".csv"))) # used in tabular report

# Load and process site names
site_lookup <- read_csv(h("site-name-lookup.csv"), col_types = cols()) %>% 
  get_site_names(dat, ., country)
write_csv(site_lookup, h(paste0("data/site-name-formatted-", country, ".csv")))
