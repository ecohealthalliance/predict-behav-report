# this is a workflow for running lassos, developed feb 2020 for the predict final reports. 
# it is an alternative to the user-inputs.R workflow. 

# Load functions and packages
purrr::walk(list.files(here::here("R/"), full.names = TRUE),
            source, echo = FALSE, verbose = FALSE)
set.seed(99)

# Breaks for discretizing continuous variables
age_breaks <- c(0, 18, 41, 61, Inf)
age_labels <- c("under_18", "18_to_40", "41_to_60", "over_60")
crowding_index_breaks <- c(0, 1, 3, 10000, Inf)
crowding_index_labels <- c("less_than_1", "1_to_3", "3_plus", "no_rooms")

# taxa_outcomes of interest for lasso
taxa_outcomes <- paste0(c("rodents", "bats", "nhp", "swine", "poultry"), "_contact")
illness_outcomes <- illness_names_clean

# all countries
countries <- eha_countries()

# For each country
walk(countries, function(country){
  
  # get logical data
  ldat <- get_behav(country) %>%
    get_logical(.) %>% # Get boolean values for non-numeric responses
    discretize_continuous(., age_breaks, age_labels, crowding_index_breaks, crowding_index_labels)  # Discretize numeric responses (this can be modified above)
  
  # create contact column for each taxa
  for(endpt in taxa_outcomes){
    ldat <- ldat %>%
      mutate(!!endpt := ifelse(!!sym(paste0(endpt, "_none")), FALSE, TRUE)) 
  }
  
  # select endpoints with minimun 10% prevalence
  endpts <- ldat %>%
    select(one_of(c(taxa_outcomes, illness_outcomes))) %>%
    gather(key = "outcome") %>%
    group_by(outcome) %>%
    summarize(prev = sum((value)/nrow(ldat))) %>%
    filter(prev >= 0.1) %>% 
    pull(outcome)
  
  # For each taxa outcome
  walk(endpts, function(endpt){
    
    # remove other taxa outcome
    if(endpt %in% taxa_outcomes){
      ldat_model <- get_outcomes(ldat, taxa_outcomes = str_remove(endpt, "_contact"), illness_outcomes = illness_names_clean) %>%
        select(-starts_with(paste0(endpt, "_"))) %>% 
        select(-participant_id)  %>%
        remove_empty(which = "cols")
    }
    
    if(endpt %in% illness_outcomes){
      ldat_model <- get_outcomes(ldat, taxa_outcomes = taxa_names, illness_outcomes = endpt) %>% 
        select(-participant_id)  %>%
        remove_empty(which = "cols")
    }
    # save copy of pre-lasso data
    write_csv(ldat_model, h("data", "lasso", paste(country, endpt, "lasso_dat.csv", sep = "_")))
    
    # create matrix
    lasso_matrices <- ldat_model %>%
      ehalasso::make_matrix(interactions = TRUE, interaction_vars = c("gender", "highest_education", "highest_education_mother",
                                                                      "length_lived", "age", "concurrent_site"),
                            outcome_var  = endpt, interaction_regex = TRUE)
    
    # remove colinear columns
    lasso_predictor_matrix <- remove_colinear_columns(lasso_matrices$model_matrix)
    
    # fit model
    lasso_fit <- fit_lasso_model(lasso_predictor_matrix$intermediate_matrix,
                                 lasso_matrices$outcome_matrix)
    
    # save copy of model
    write_rds(lasso_fit, h("data", "lasso", paste(country, endpt, "lasso_model.rds", sep = "_")))
    
    # make plot
    plot_lasso_fit(lasso_obj = lasso_fit, dat = ldat2, title = str_replace(endpt, "_", " "))
    ggsave(h("outputs", "lasso-figs", paste0(country, "_", endpt, ".png")), width = 12)
  })
})

  
  
  
})