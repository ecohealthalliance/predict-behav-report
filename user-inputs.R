#-------------------------------------------------------------
# Load packages and functions 
purrr::walk(list.files(here::here("R/"), full.names = TRUE),
            source, echo = FALSE, verbose = FALSE)
set.seed(99)
#-------------------------------------------------------------
# User entered information
country <- "Indonesia" # Name country here
illness_outcomes <- c("ili") # Select illness of interest here (`illness_names_clean` object (loaded in environment) to see full list). Accepts NULL.
heatmap_taxa_outcomes <- taxa_names # Select taxa contact of interest for heatmaps (`taxa_names` object (loaded in environment) to see full list). Does NOT accept NULL.
tabular_lasso_taxa_outcomes <- c("bats") # Select taxa contact of interest for lasso and tabular reports (`taxa_names` object (loaded in environment) to see full list). Accepts NULL.

# Confirm endpoints are valid
assertthat::assert_that(all(illness_outcomes %in% illness_names_clean), msg = "one or more illness_outcomes is not recognized")
assertthat::assert_that(all(heatmap_taxa_outcomes %in% taxa_names), msg = "one or more heatmap_taxa_outcomes is not recognized")
assertthat::assert_that(all(tabular_lasso_taxa_outcomes %in% tabular_lasso_taxa_outcomes), msg = "one or more heatmap_taxa_outcomes is not recognized")

# Breaks for discretizing continuous variables
age_breaks <- c(0, 18, 41, 61, Inf)
age_labels <- c("under_18", "18_to_40", "41_to_60", "over_60")
crowding_index_breaks <- c(0, 1, 3, 10000, Inf)
crowding_index_labels <- c("less_than_1", "1_to_3", "3_plus", "no_rooms")

# Download data from EIDITH? (must have local eidith if FALSE)
download_fresh <- FALSE

# Run the lasso analysis?  (will be slow, depending on the number of outcomes, should only be run once or whenever there are changes to the data)
run_lasso <- FALSE

# Which reports to run? options are: "summary-report", "tabular-report", "site-maps", "heatmaps", "lasso"
reports <- c("summary-report", "tabular-report", "site-maps", "heatmaps", "lasso")
#-------------------------------------------------------------
source(h("scripts", "make.R"))

