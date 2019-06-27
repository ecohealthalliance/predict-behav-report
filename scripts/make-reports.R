# Run reports
# Summary Report
if("summary-report" %in% reports){
  rmarkdown::render(h("scripts/01-summary-report.Rmd"),
                    output_file = paste0(country_mod, "-behav-summary-report.html"),
                    output_dir = h("outputs", "reports"),
                    params = list(country = country_mod),
                    quiet = FALSE, envir = new.env())
}

# Tabular Report
if("tabular-report" %in% reports){
  rmarkdown::render(h("scripts/02-tabular-report.Rmd"),
                    output_file = paste0(country_mod, "-behav-tabular-report.html"),
                    output_dir = h("outputs", "reports"),
                    params = list(country = country_mod,
                                  illness_outcomes = illness_outcomes,
                                  taxa_outcomes = tabular_lasso_taxa_outcomes),
                    quiet = FALSE, envir = new.env())
}

# Site Maps
if("site-maps" %in% reports){
  rmarkdown::render(h("scripts/03-site-maps.Rmd"),
                    output_file = paste0(country_mod, "-behav-site-maps.html"),
                    output_dir = h("outputs", "reports"),
                    params = list(country = country_mod),
                    quiet = FALSE, envir = new.env())
}

# Heatmap 
if("heatmaps" %in% reports){
  rmarkdown::render(h("scripts/04-heatmaps.Rmd"),
                    output_file = paste0(country_mod, "-behav-heatmaps.html"),
                    output_dir = h("outputs", "reports"),
                    params = list(country = country_mod,
                                  taxa_outcomes = heatmap_taxa_outcomes),
                    quiet = FALSE, envir = new.env())
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
                    output_file = paste0(country_mod, "-behav-lasso.html"),
                    output_dir = h("outputs", "reports"),
                    params = list(country = country_mod,
                                  illness_outcomes = illness_outcomes,
                                  taxa_outcomes = tabular_lasso_taxa_outcomes),
                    quiet = FALSE, envir = new.env())
}
