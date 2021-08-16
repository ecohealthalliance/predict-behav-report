#-------------------------------------------------------------
# Load packages
## Respond "no" to Do you want to install from sources the package which needs compilation? (yes/no/cancel)?"
# remotes::install_deps(upgrade = "always", quiet = TRUE)
#-------------------------------------------------------------
# Load functions

purrr::walk(list.files(here::here("R/"), full.names = TRUE),
            source, echo = FALSE, verbose = FALSE)
set.seed(99)
#-------------------------------------------------------------
# User entered information

country <-  "Ivory Coast"       # Name country here
  ## Can only select ONE country
illness_outcomes <- NULL # Select illness of interest here
  ## (`illness_names_clean` object (loaded in Environment tab) to see full list). 
  ## Accepts NULL.
heatmap_taxa_outcomes <- taxa_names # Select taxa contact of interest for heatmaps 
  ## (`taxa_names` object (loaded in Environment tab) to see full list). 
  ## Does NOT accept NULL.
  ## Current condition (taxa_names) will include all taxa; <- c("bats", "rodents", "swine", "etc") will select specific taxa
tabular_lasso_taxa_outcomes <- c("bats") # Select taxa contact of interest for lasso and tabular reports
  ## (`taxa_names` object (loaded in environment) to see full list). 
  ## Accepts NULL.
#------------------------------------------------------------
# Confirm endpoints are valid

assertthat::assert_that(all(illness_outcomes %in% illness_names_clean), msg = "one or more illness_outcomes is not recognized")
assertthat::assert_that(all(heatmap_taxa_outcomes %in% taxa_names), msg = "one or more heatmap_taxa_outcomes is not recognized")
assertthat::assert_that(all(tabular_lasso_taxa_outcomes %in% tabular_lasso_taxa_outcomes), msg = "one or more heatmap_taxa_outcomes is not recognized")
#------------------------------------------------------------
# Breaks for discretizing continuous variables

age_breaks <- c(0, 18, 41, 61, Inf)
age_labels <- c("under_18", "18_to_40", "41_to_60", "over_60")
crowding_index_breaks <- c(0, 1, 3, 10000, Inf)
crowding_index_labels <- c("less_than_1", "1_to_3", "3_plus", "no_rooms")
#------------------------------------------------------------
# Download data from EIDITH
# import_local_db(database = "global")
#------------------------------------------------------------
# Run the lasso analysis?  
  ## If TRUE, processing will be slow depending on the number of outcomes
  ## Lasso should only be run once per outcome, or whenever there are changes to eidith (human) data

run_lasso <- TRUE
#-------------------------------------------------------------
# Write csv files to use for reports

country_mod <- str_replace(country, ", ", "-") 
source(h("scripts", "make-data.R"))
#-------------------------------------------------------------
# Check prevalence of outcomes --> see html output (country-outcome-prevalence) in outputs/reports folder
## This only needs to be run once, unless there are changes to eidith (human) data

# rmarkdown::render(h("scripts/00-prevalence.Rmd"),
#                   output_file = paste0(country_mod, "-outcome-prevalence.html"),
#                   output_dir = h("outputs", "reports"),
#                   params = list(country = country_mod,
#                                 illness_outcomes = illness_names_clean,
#                                 taxa_outcomes  = taxa_names),
#                   quiet = FALSE, envir = new.env())
  ## If prevalence is less than 10.0%, do NOT run the lasso for that particular outcome
#-------------------------------------------------------------
# Select reports to run
  ## Options are: "summary-report", "tabular-report", "site-maps", "heatmaps", "lasso"

reports <- c("summary-report", "tabular-report")
#-------------------------------------------------------------
# Run reports
  ## html reports can be found in outputs/reports folder

source(h("scripts", "make-reports.R"))
#-------------------------------------------------------------