#-------------------------------------------------------------
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
#-------------------------------------------------------------
# Run reports

# Summary Report
if("summary-report" %in% reports){
  rmarkdown::render(h("scripts/01-summary-report.Rmd"),
                    output_file = paste0(country, "-behav-summary-report.html"),
                    output_dir = h("outputs", "reports"),
                    params = list(country = country),
                    quiet = FALSE)
}

# Tabular Report
if("tabular-report" %in% reports){
  rmarkdown::render(h("scripts/02-tabular-report.Rmd"),
                    output_file = paste0(country, "-behav-tabular-report.html"),
                    output_dir = h("outputs", "reports"),
                    params = list(country = country,
                                  illness_outcomes = illness_outcomes,
                                  taxa_outcomes = tabular_lasso_taxa_outcomes),
                    quiet = FALSE)
}

# Site Maps
if("site-maps" %in% reports){
  rmarkdown::render(h("scripts/03-site-maps.Rmd"),
                    output_file = paste0(country, "-behav-site-maps.html"),
                    output_dir = h("outputs", "reports"),
                    params = list(country = country),
                    quiet = FALSE)
}

# Heatmap 
if("heatmaps" %in% reports){
  rmarkdown::render(h("scripts/04-heatmaps.Rmd"),
                    output_file = paste0(country, "-behav-heatmaps.html"),
                    output_dir = h("outputs", "reports"),
                    params = list(country = country,
                                  taxa_outcomes = heatmap_taxa_outcomes),
                    quiet = FALSE)
}

# Lasso
# First, run the script to make sure each outcome has at least 10% prevalence

# Second, run the lasso for all outcome variables if specified
if(run_lasso){
  lasso <- get_lasso(ldat, illness_outcomes, tabular_lasso_taxa_outcomes)
}

# Third, run the report
if("lasso" %in% reports){
  rmarkdown::render(h("scripts/05-lasso.Rmd"),
                    output_file = paste0(country, "-behav-lasso.html"),
                    output_dir = h("outputs", "reports"),
                    params = list(country = country,
                                  illness_outcomes = illness_outcomes,
                                  taxa_outcomes = tabular_lasso_taxa_outcomes),
                    quiet = FALSE)
}
