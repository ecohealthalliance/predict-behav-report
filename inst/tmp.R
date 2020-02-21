dat <- ldat %>% 
    select(-participant_id, -encephalitis)  %>%
    remove_empty(which = "cols")
  
outcome_var = c("ili", "sari")

make_matrix <- function(dat,
                        outcome_var,
                        interaction_vars = NULL,
                        interaction_vars_regex = FALSE #TODO - add regex functionality
                        ) {
  
  assertthat::assert_that(all(purrr::map_lgl(dat, ~is.logical(.))), msg = "all columns in data must be logical")
  
  # Define basic model formula
  form <- as.formula(paste0(outcome_var, ' ~ ', ' + ', '.'))
  
  # Define full interaction formula
  if(!is.null(interaction_vars)){
    form <- as.formula(paste0(outcome_var, ' ~ ', ' + ', '.', ' + ', '(.)^2'))
  }
  
  # Generate initial model matrix and remove intercept
  model_matrix <- stats::model.matrix(form, data = dat)
  model_matrix <- model_matrix[ , -1]
  
  # Clean up matrix column names
  colnames(model_matrix) <- colnames(model_matrix) %>%
    stringr::str_replace_all(., "TRUE", "") %>%
    stringr::str_replace_all(., "`", "")
  
  # Apply interactions
  if(!is.null(interaction_vars)){
    all_interactions <- colnames(model_matrix)[grep(":", colnames(model_matrix))]
    included_interactions <- map_lgl(all_interactions, function(x){
      str_split(x, ":") %>%
        map(., ~. %in% interaction_vars) %>%
        unlist(.) %>%
        any(.)
    }) %>%
      all_interactions[.]
    included_cols <- c(colnames(dat), included_interactions)
    included_index <- which(colnames(model_matrix) %in% included_cols)
    model_matrix <- model_matrix[ , included_index]
  }

  return(model_matrix)
}
