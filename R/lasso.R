## Lasso functions


############# Get exposure response data function ##############
#' Create cleaned, binary response frame for contx_type
#' @param dat raw data frame
#' @param taxa_names character vector of taxa names
#' @return wide data frame with binary exposures of interest
#' @export

get_clean_exposures <- function(dat, taxa_names){
  
  #create final list for plyr join_all
  res <- list()
  #iterate through all taxa_names
  for(taxa in taxa_names){
    
    #create name to use in data pipeline
    contx_type <- paste0(taxa, '_contact')
    no_contx_type <- paste0('no_', contx_type)
    #create single taxa exposure frame
    taxa_dat <- dat %>%
      select(participant_id, !!sym(contx_type)) %>%  #Non-standard eval of name
      ed2_expand_wide(!!sym(contx_type)) %>% #expand into wide frame of binary vars
      #rename n_a in ed2_expand_wide for interpretability
      rename( !!sym(no_contx_type) := n_a) %>% #use special assign := to work with !!sym(var)
      select(-!!sym(contx_type)) #remove original multiresponse from final frame
    
    #add taxa_exposure frame to list
    res[[taxa]] <- taxa_dat
  }
  #create final joined data
  out <- join_all(dfs = res, #join list of data frames
                  by = 'participant_id') #use primary key
  return(out)
}


############## Get covariate data function ################

#' Create and widen covariate data for self reported illness modeling
#' @param dat human data
#' @return wide data set with data types cleaned for regression analysis
#' @export

get_clean_illness_covariates <- function(dat){
  
  d1 <- dat %>%
    #select and widen covariate data
    select(participant_id,
           age,
           gender,
           length_lived,
           matches("dwelling"),
           water_treated,
           shared_water_last_year,
           shared_water_life,
           matches("education"),
           primary_livelihood, #these are to build occupations variable.
           livelihood_groups_other, #use this to disambiguate the other categories
           matches("animal"),
           dedicated_location_for_waste,
           -matches("_notes"), #remove notes cols,
           matches("meat"),
           matches("scratched_bitten"),
           matches("hunted"),
           worried_about_disease,
           risk_open_wound,
           travelled,
           travel_reason,
           treatment,
           had_symptoms_in_last_year,
           had_symptoms_in_last_year_other_people,
           illness_death,
           -matches("_contact")
    ) %>%
    # modify scratched/bitten responses
    ed2_expand_wide(col = scratched_bitten_action)
  scratched_bitten_treated_cols <- syms(names(d1)[names(d1) %in% c("scratched_bitten_action_wash_wound_with_soap_and_water",
                                                                   "scratched_bitten_action_visit_doctor")])
  scratched_bitten_never_cols <- syms(names(d1)[names(d1) %in% c("scratched_bitten_action_n_a",
                                                                 "scratched_bitten_action_never_butcher_or_slaughter")])
  
  d2 <- d1 %>%
    mutate(scratched_bitten_treated = any(!!!scratched_bitten_treated_cols),
           scratched_bitten_never = any(!!!scratched_bitten_never_cols),
           scratched_bitten_untreated = !scratched_bitten_treated & !scratched_bitten_never) %>%
    select(-starts_with("scratched_bitten_action"), -scratched_bitten_life, -scratched_bitten_last_year) %>% # collapsed above
    #mutate character vectors to factors where appropriate
    mutate_at(.vars = vars(gender,
                           length_lived,
                           highest_education,
                           highest_education_mother),
              .funs = funs(factor)
    ) %>%
    mutate(occupation = #create new occupation variable
             if_else( #condition
               condition = str_detect(primary_livelihood, "[Oo]ther"), #if other
               true = livelihood_groups_other, #use other group to map
               false = primary_livelihood #else use primary livelihood
             )
    ) %>%
    mutate(occupation =
             if_else(
               condition = occupation == '', #if occupation is empty
               true = primary_livelihood,
               false = occupation
             )
    ) %>%
    #map other column back to "Other" because the cleaning never ends
    mutate(occupation =
             if_else(condition = str_detect(occupation, '[Oo]ther'),
                     true = 'Other',
                     false = occupation
             )
    ) %>%
    #make occupation a factor
    mutate(occupation = as.factor(occupation)) %>%
    select(-matches('livelihood')) %>%
    mutate_at(
      .vars = vars(age,
                   people_in_dwelling,
                   children_in_dwelling,
                   males_in_dwelling,
                   rooms_in_dwelling),
      .funs = funs(as.numeric)
    ) %>%
    mutate_at(.vars = vars(pet_in_dwelling_last_year,
                           animals_in_dwelling_last_year,
                           shared_water_last_year,
                           handle_animals_last_year,
                           raised_animals_last_year,
                           animal_feces_food_last_year,
                           animals_in_food_last_year,
                           eaten_sick_animal_last_year,
                           eaten_dead_animal_last_year,
                           sold_dead_animal_last_year,
                           hunted_animal_last_year,
                           slaughtered_animal_last_year,
                           cooked_meat_last_year,
                           eaten_raw_meat_last_year,
                           dwelling_permanent_structure,
                           dedicated_location_for_waste,
                           water_treated,
                           water_used_by_animals,
                           pet_in_dwelling_life,
                           animals_in_dwelling_life,
                           shared_water_life,
                           handle_animals_life,
                           raised_animals_life,
                           animal_feces_food_life,
                           animals_in_food_life,
                           eaten_sick_animal_life,
                           eaten_dead_animal_life,
                           sold_dead_animal_life,
                           hunted_animal_life,
                           slaughtered_animal_life,
                           cooked_meat_life,
                           eaten_raw_meat_life,
                           worried_about_disease,
                           risk_open_wound,
                           travelled,
                           had_symptoms_in_last_year,
                           had_symptoms_in_last_year_other_people,
                           illness_death,
    ),
    #map yes to TRUE and all other responses to FALSE
    .funs = funs(
      if_else(str_detect(., '[Yy]es'),
              true = TRUE,
              false = FALSE))
    ) %>%
    #add count for occupation to aggregate rare categories
    add_count(occupation) %>%
    #aggregate rare occupations into 'rare' column
    mutate(occupation = if_else(n < 10, 'rare', as.character(occupation))) %>%
    #reconvert to factor
    mutate(occupation = as.factor(occupation)) %>%
    #expansion of factors to binary categorical outcomes
    ed2_expand_wide(occupation) %>%
    ed2_expand_wide(highest_education) %>%
    ed2_expand_wide(highest_education_mother) %>%
    ed2_expand_wide(gender) %>%
    ed2_expand_wide(length_lived) %>%
    ed2_expand_wide(travel_reason) %>%
    ed2_expand_wide(treatment) %>%
    
    #drop counting and other expanded factor variables
    select(-n,
           -occupation,
           -highest_education,
           -highest_education_mother,
           -gender,
           -length_lived,
           -travel_reason,
           -treatment
    )
  #scale and center numeric variables to mean 0 and sd 1
  #mutate_if(is.numeric, scale())
  d2
}



################# Get symptoms response data function #################

#' Obtain and flatten self reported symptom outcome data
#' @param dat data frame for which to obtain self-reported outcomes
#' @param of_interest logical indicating to only select outcomes of interest as per analysis plan
#' @param lifetime  If TRUE, report symptoms in lifetime, otherwise past year
#' @return data frame (tibble) with self reported illness widened to dichotomous variables


get_self_reports <- function(dat, of_interest = TRUE, lifetime = FALSE) {
  
  dat2 <- dat %>%
    select(participant_id,
           symptoms_life,
           symptoms_in_last_year) %>%
    ed2_expand_wide(col = symptoms_life) %>%
    ed2_expand_wide(col = symptoms_in_last_year) %>%
    # drop other reported diseases that are coerced to NA
    select(-symptoms_life, -symptoms_in_last_year)
  
  if(of_interest) { # if we only want particular disease outcomes of interest
    dat2 <- select(dat2, participant_id, matches('sari|ili|hemorrhagic|encephalitis|diarrhea|fever_with_rash|persistent_rash'))
  }
  
  if(lifetime) {
    dat3 <- select(dat2, -matches('in_last_year')) # remove in_last_year self reports
  } else {
    dat3 <- select(dat2, -matches('life')) # remove lifetime self reports
  }
  
  return(dat3)
}



################## Create formatted dataframe #################

#' Create analysis dataframe
#'
#' @param dat data frame from which to create analysis data set 
#' @param taxa_names vector of taxa types for which exposures will be included
#' @param lifetime  If TRUE, report symptoms and predictors for lifetime, otherwise past year symptoms and all predictors
#' @param combine_illness If TRUE, self-report illness symptoms are combined, otherwise are evaluated separately
#' @param illness_var vector of self-report illness to be included in model
#' @export

get_formatted_dat <- function(dat,
                              taxa_names = c('rodents',
                                             'nhp',
                                             'bats',
                                             'swine',
                                             'poultry',
                                             'birds',
                                             'cattle',
                                             'goats_sheep',
                                             'carnivores',
                                             'camels',
                                             'pangolins',
                                             'ungulates',
                                             'dogs',
                                             'cats'),
                              lifetime = FALSE,
                              combine_illness = FALSE,
                              illness_var = c()) {
  
  # covariate data
  covars <- dat %>%
    get_clean_illness_covariates()
  
  # illness data
  if(length(illness_var)>0){
    
    illness_var_match <- paste(illness_var, collapse = "|")
    illness_var_field <- paste(illness_var, collapse = "_OR_")
    
    if(combine_illness) {
      illness <- dat %>%
        get_self_reports(lifetime = lifetime) %>%
        select(participant_id, matches(illness_var_match)) %>%
        mutate(!!illness_var_field := suppressWarnings(as.factor(apply(select(.,matches(illness_var_match)), 1, any)))) %>%
        select(participant_id, !!illness_var_field)
    } else{illness <- dat %>%
      get_self_reports(lifetime = lifetime) %>%
      select(participant_id, matches(illness_var_match)) %>%
      rename_at(vars(matches(illness_var_match)), ~gsub('.*[_]', '', .))
    }
    
    covars <- left_join(covars, illness)
  }
  
  
  # filter covariates based on whether or not the outcome is for lifetime or only the past year
  if(lifetime == TRUE) {
    
    # remove covariates related to "last_year" timeframe
    covars <- dplyr::select(covars, -(matches("_last_year")))
    analysis_dat <- covars
    
  } else {
    
    # remove covariates related to "_life" timeframe
    covars <- dplyr::select(covars, -(matches("_life")))
    
    # remove most "last_year" covariates since more detailed info on these exposures will
    # come from "get_clean_exposures()", but keep "shared_water_last_year" and
    # "animals_in_food_last_year" as is
    last.year.vars <- grep("_last_year", colnames(covars), value = T)
    last.year.vars.to.keep <- c("shared_water_last_year", "animals_in_food_last_year")
    last.year.vars.to.exclude <- last.year.vars[!(last.year.vars %in% last.year.vars.to.keep)]
    
    covars <- dplyr::select(covars, -last.year.vars.to.exclude)
    
    # add exposure data
    exposures <- dat %>%
      get_clean_exposures(taxa_names = taxa_names)
    
    # generate analysis data set
    analysis_dat <- plyr::join_all(list(covars, exposures),
                                   by = 'participant_id')
  }
  analysis_dat
}


################# Create illness analysis frame function ###############

#' Create illness analysis frame
#'
#' @param dat data frame from which to create analysis data set created from get_self_reports or get_contaxa
#' @param outcome_var outcome variable of interest keyword (sari, ili, encehpalitis or hemorrhagic)
#' @param taxa_names vector of taxa types for which exposures will be included
#' @param lifetime  If TRUE, report symptoms and predictors for lifetime, otherwise past year symptoms and all predictors
#' @export

get_illness_analysis_dat <- function(dat,
                                     outcome_var,
                                     taxa_names = c('rodents',
                                                    'nhp',
                                                    'bats',
                                                    'swine',
                                                    'poultry',
                                                    'birds',
                                                    'cattle',
                                                    'goats_sheep',
                                                    'carnivores',
                                                    'camels',
                                                    'pangolins',
                                                    'ungulates',
                                                    'dogs',
                                                    'cats'),
                                     lifetime = FALSE) {
  
  # outcome var name
  outcome_var_name <- dat %>%
    get_self_reports(lifetime = lifetime) %>%
    select(map(outcome_var,
               matches,
               vars = colnames(.)) %>%
             unlist()) %>%
    colnames()
  
  # create outcome dataframe
  outcome<- dat %>%
    get_self_reports(lifetime = lifetime) %>%
    select(participant_id, outcome_var_name) %>%
    mutate_if(is.logical, as.factor)
  #mutate(!!outcome_var_name := apply(select(.,!!outcome_var_name), 2, function(x)as.factor(sym(x))))
  #mutate(!!sym(outcome_var_name) := as.factor(!!sym(outcome_var_name)))
  
  # covariate data
  covars <- dat %>%
    get_clean_illness_covariates()
  
  # filter covariates based on whether or not the outcome is for lifetime or only the past year
  if(lifetime == TRUE) {
    
    # remove covariates related to "last_year" timeframe
    covars <- dplyr::select(covars, -(matches("_last_year")))
    
    # generate analysis data set (without extra exposures predictor variables which are only
    # relaevant to the past year)
    analysis_dat <- plyr::join_all(list(outcome, covars), by = 'participant_id')
  } else {
    
    # remove covariates related to "_life" timeframe
    covars <- dplyr::select(covars, -(matches("_life")))
    
    # remove most "last_year" covariates since more detailed info on these exposures will
    # come from "get_clean_exposures()", but keep "shared_water_last_year" and
    # "animals_in_food_last_year" as is
    last.year.vars <- grep("_last_year", colnames(covars), value = T)
    last.year.vars.to.keep <- c("shared_water_last_year", "animals_in_food_last_year")
    last.year.vars.to.exclude <- last.year.vars[!(last.year.vars %in% last.year.vars.to.keep)]
    
    covars <- dplyr::select(covars, -last.year.vars.to.exclude)
    
    # add exposure data
    exposures <- dat %>%
      get_clean_exposures(taxa_names = taxa_names)
    
    # generate analysis data set
    analysis_dat <- plyr::join_all(list(outcome, covars, exposures),
                                   by = 'participant_id')
  }
  
  # return analysis dataframe
  if(length(outcome_var)==1){
    colnames(analysis_dat)[2] <- outcome_var
  }else{ #combine outcomes
    outcome_var_field <- paste(outcome_var, collapse = "_OR_")
    analysis_dat <- analysis_dat %>%
      mutate(!!outcome_var_field := suppressWarnings(as.factor(apply(select(.,!!outcome_var_name), 1, any)))) %>%
      select(-outcome_var_name) %>%
      select(1, ncol(.), everything())
  }
  analysis_dat
}



#################### Remove colinear columns #####################

#' Generate a matrix for lasso modeling (following removal of colinear columns)
#' @param dat dataframe generated by get_XXX_analysis_frame
#' @param full_interactions whether to generate a matrix with two-way interactions or not
#' @param restricted_interactions whether to generate a matrix with two-way interactions only between demographic variables and all others
#' @param outcome_var name of outcome column
#' @return a model matrix
#' @export

make_matrix <- function(dat,
                        full_interactions = FALSE,
                        restricted_interactions = TRUE,
                        outcome_var) {
  
  # TODO: test rank reduce algo on full data matrix (with interactions)
  # TODO: allow users to specify interactions
  
  # Define basic model formula
  form <- as.formula(paste0(outcome_var, ' ~ ', ' + ', '.'))
  
  # Define full interaction formula
  if(full_interactions|restricted_interactions) {
    
    form <- as.formula(paste0(outcome_var, ' ~ ', ' + ', '.', ' + ', '(.)^2'))
  }
  
  # Generate initial model matrix and remove intercept term
  model_matrix <- model.matrix(form, data = dat)
  model_matrix <- model_matrix[ , -1]
  
  # If we only want restricted interaction set, trim out unneeded interactions
  if(restricted_interactions) {
    
    # Define demographic variables of interest
    demo.vars <- c("gender", "highest_education", "highest_education_mother",
                   "length_lived", "age")
    
    # Define a string to match demographic variables
    demo.vars.search.string <- paste(paste0("^", demo.vars), collapse = "|")
    
    # Get names of all non-demographic variables
    non.demo.vars <- grep(demo.vars.search.string, colnames(dat),
                          value = TRUE, invert = TRUE)
    
    # Define a string to exclude interactions that contain the non-demographic variables
    # as the second interactor (i.e., we want to keep all interactions of the form
    # "non-demographic variable:demographic_variable")
    interaction.exclusion.string <- paste0(paste0(":", non.demo.vars), collapse = "|")
    
    # Define variable columns to keep and subset the matrix
    indexes.to.keep <- grep(interaction.exclusion.string, colnames(model_matrix),
                            invert = TRUE)
    model_matrix <- model_matrix[ , indexes.to.keep]
    
    # Also need to exclude any "demographic_variable:demographic_variable" interactions
    demo.by.demo.exclusion.string <-
      paste(paste0("^", demo.vars, ".*:", demo.vars), collapse = "|")
    
    indexes.to.keep <- grep(demo.by.demo.exclusion.string, colnames(model_matrix),
                            invert = TRUE)
    model_matrix <- model_matrix[ , indexes.to.keep]
  }
  
  # Clean up matrix column names
  colnames(model_matrix) <- colnames(model_matrix) %>%
    stringr::str_replace_all(., "TRUE", "") %>%
    stringr::str_replace_all(., "`", "")
  
  return(model_matrix)
}


#' Use matrix rank algorithm to remove linearly dependent columns from data
#' @param model_matrix model matrix generated by make_matrix
#' @param verbose whether to print a progress bar
#' @param dups_only if TRUE, skips rank reduction and only removes exact duplicate columns
#' @return named list of removed variable names (removed_vars) and matrix for lasso modeling (data_matrix)
#' @export

remove_colinear_columns <- function(model_matrix,
                                    verbose = interactive(),
                                    dups_only = FALSE) {
  
  #warning('This function requires large amounts of compute power.')
  
  # remove duplicate columns
  intermediate_matrix <- remove_duplicate_columns(model_matrix)
  # get rank of matrix
  mat_rank <- rankMatrix(intermediate_matrix, warn.t = FALSE)
  if(verbose) message('\nMatrix Rank: ', mat_rank)
  
  if(!dups_only) {
    if(verbose) message("Rank reduction round 1")
    pb <- progress_estimated(ncol(intermediate_matrix), min_time=5)
    for(ii in rev(seq_len(ncol(intermediate_matrix)))) {
      pb$tick()$print()
      tmp_matrix <- intermediate_matrix[,-ii]
      tmp_rank <- rankMatrix(tmp_matrix, warn.t = FALSE)
      if(tmp_rank == mat_rank) { #if rank unchanged with removal
        intermediate_matrix <- tmp_matrix
      }
    }
    if(verbose) message("Rank reduction round 2")
    pb <- progress_estimated(ncol(intermediate_matrix), min_time=5)
    for(ii in rev(seq_len(ncol(intermediate_matrix)))) {
      pb$tick()$print()
      tmp_matrix <- intermediate_matrix[,-ii]
      tmp_rank <- rankMatrix(tmp_matrix, warn.t = FALSE)
      if(tmp_rank == mat_rank) { #if rank unchanged with removal
        intermediate_matrix <- tmp_matrix
      }
    }
  }
  
  removed_ii <- (!(colnames(model_matrix) %in% colnames(intermediate_matrix)))
  removed_vars <- colnames(model_matrix)[removed_ii]
  return(list(removed_vars = removed_vars, model_data_matrix = intermediate_matrix))
}


#' De-duplicate matrix columns
#' @param model_matrix from which to remove duplicates
#' @return matrix with duplicated columns removed for lasso and inference
#' @noRd

remove_duplicate_columns <- function(model_matrix){
  model_matrix <- model_matrix[,
                               !duplicated.default(
                                 apply(model_matrix, 2,
                                       function(x) {
                                         if(any(!(x %in% c(0,1))))
                                           return(list(x))
                                         else if(x[1]) {
                                           return(list(as.logical(x)))
                                         } else {
                                           return(list(!x))
                                         }
                                       }))]
  return(model_matrix)
}



# #################LASSO Regularized Logistic Regression ##################

#' Perform cross-validation to get optimal elastic net parameter
#' @param model_matrix model matrix (features, no response) to be used for logistic regression
#' @param reponse vector of response values (binary) for logisitic regression
#' @param cv_folds number of folds for cross_validation
#' @param verbose logical indicating wheter to print lambda to std output
#' @return lambda which minimizes the binomial deviance
#' @noRd

cv_elasticnet_param <- function(alpha_vals, model_matrix, response, cv_folds){
  cat("Performing alpha cross validation\n")
  res <- list()
  for(alpha in alpha_vals){
    lambda.fit <- cv.glmnet(x = model_matrix,
                            y = response ,
                            nfolds = cv_folds, #5-fold CV
                            family = 'binomial',
                            alpha = alpha)
    res[[as.character(alpha)]] <- lambda.fit$cvm
  }
  res <- stack(res)
  #get min alpha
  as.numeric(as.character(res$ind[which.min(res$values)]))
  
}


#' Perform cross-validation to get optimal lambda (shrinkage) parameter
#'
#' @param model_matrix model matrix (features, no response) to be used for logistic regression
#' @param reponse vector of response values (binary) for logisitic regression
#' @param cv_folds number of folds for cross_validation
#' @param verbose logical indicating wheter to print lambda to std output
#' @return lambda which minimizes the binomial deviance
#' @noRd

cv_shrinkage_param <- function(model_matrix, response, cv_folds=20, verbose = TRUE, ...){
  # best_alpha <- cv_elasticnet_param(seq(0.05,1,by=0.05), model_matrix, response, cv_folds)
  # cat("Best alpha by cross validation:", best_alpha)
  if(interactive() && verbose) message("Optimizing shrinkage parameter")
  lambda.fit <- cv.glmnet(x = model_matrix,
                          y = response ,
                          nfolds = cv_folds, #5-fold CV
                          family = 'binomial',
                          type.measure = "deviance",
                          #lambda = 10^-(seq(1, 2, by=0.005)),
                          alpha = 1,
                          standardize = FALSE) #LASSO regularize
  #plot bias variance tradeoff curve
  #print value of minimum lambda
  if(interactive() && verbose){
    message(paste0("Minimum value of lambda by ", cv_folds, "-fold CV:", lambda.fit$lambda.min))
  }
  #select minimum lam
  lam <- lambda.fit$lambda.min
  return(lam)
}


#' Fit glmnet with optimal lambda on training data
#'
#' @param model_matrix model matrix of features (no response column) for logisitc regression
#' @param response response vector (binary)
#' @param seed a seed value to be used for reproducible fitting
#' @param verbose whether to print progress messages
#' @param bootstrap_support if TRUE, run a bootsrap to determine the fraction of
#'   times variables are included in the model (support).  This vector will be
#'   included as an attribute.  Run attr(model, "bootstrap_support") to access it.
#' @return fitted glmnet object
#' @export

fit_lasso_model <- function(model_matrix, response, bootstrap_support = TRUE, seed = 0, verbose= TRUE) {
  
  if(!is.null(seed)) set.seed(seed)
  
  # get optimal lambda
  lam <- cv_shrinkage_param(model_matrix, response, verbose = verbose)
  
  # fit lasso model
  lasso.fit <- glmnet(x = model_matrix,
                      y = response, #fit using logical labels
                      family = 'binomial',
                      lambda = lam, #use CV'd lambda.min
                      alpha = 1,
                      thresh = 1e-8,
                      standardize=FALSE,
                      lower.limits = -1e6, upper.limits = 1e6)
  
  if(bootstrap_support) {
    safe_glmnet <- safely(glmnet, otherwise=NULL, quiet = TRUE)
    
    B = 250
    n = length(response)
    coef_mat <- matrix(NA, nrow = ncol(model_matrix) + 1, ncol = B)
    if(interactive()) message("Calculating bootstrap support")
    pb <- progress_estimated(B, min_time=3)
    rownames(coef_mat) <- c("(Intercept)", colnames(model_matrix))
    for(i in seq_len(B)) {
      ind <- sample(seq_len(n), n, replace = TRUE)
      xboot = model_matrix[ind,]
      yboot = response[ind]
      bootfit <- suppressWarnings(safe_glmnet(xboot, yboot, lambda = lam, family="binomial", alpha=1,
                                              thresh = 1e-8,
                                              standardize=FALSE,
                                              lower.limits = -1e6, upper.limits = 1e8))
      if(!is.null(bootfit$result)) {
        coef_mat[, i] <- as.vector(coef(bootfit$result))
      } else {
        coef_mat[, i] <- NA
      }
      pb$tick()$print()
    }
    rs <- rowSums(coef_mat != 0)
    missed_boots <- sum(is.na(rs))
    if(missed_boots > B/10) {
      warning("More than 10% of bootstrap samples failed to fit, support values may be untrustworthy.")
    }
    support <- rs/(B - missed_boots)
    names(support) <- rownames(coef_mat)
    rm(coef_mat)
    attr(lasso.fit, "bootstrap_support") <- support
  }
  return(lasso.fit)
}


#' Computes misclassification rate of a fitted lasso model
#' @param lasso_fit from fit_lasso_model function
#' @param model_matrix heldout model matrix of features
#' @param response response (y) vector
#' @export

compute_lasso_performance <- function(lasso_fit, model_matrix, response){
  #get class predictions
  pred <- predict(lasso_fit,
                  newx = model_matrix,
                  type = 'class')
  
  pred <- as.logical(pred)
  #get misclassification rate
  lasso.conf.matrix <- table(pred, response)
  cat('Confusion Matrix:\n')
  print(lasso.conf.matrix)
  #get misclassification rate
  misclass_rate <- 1 - ( sum(diag(lasso.conf.matrix)) / sum(lasso.conf.matrix))
  cat("\nMisclassification rate:", misclass_rate)
  #compute sensitivity
  sens <- lasso.conf.matrix[2,2] / (lasso.conf.matrix[2,2] + lasso.conf.matrix[1,2])
  cat("\nFalse negative rate:", 1 - sens)
  #compute specificity
  specif <- lasso.conf.matrix[1,1] / (lasso.conf.matrix[1,1] + lasso.conf.matrix[2,1])
  cat("\nFalse positive rate:", 1 - specif)
}


#' Perform Inference on fitted lasso model
#'
#' @param lasso_fit from fit_lasso_model
#' @param model_matrix model matrix of features
#' @param response vector of response values
#' @return formatted LASSO inference table (variable, mean odds ration, 95% ci, p-values)
#' @export

perform_lasso_inference <- function(lasso_fit, model_matrix, response){
  warning("This function may take a while to execute fully")
  #get lambda and N
  lam <- lasso_fit$lambda
  n <- length(response)
  #get betas
  betas <- coef(lasso_fit,
                s = lam/n) # removes the intercept
  
  #perform inference for glmnet (LASSO) model
  lasso.inference <-
    fixedLassoInf(x = model_matrix,
                  y = as.logical( response )*1 ,
                  beta = betas,
                  lambda = lam,
                  family = 'binomial',
                  #  gridrange = c(1e-3, 1e3),
                  alpha = 0.05,
                  intercept=TRUE,
                  type = "partial",
                  verbose=TRUE)
  
  #variables names for output
  var_names <- names(lasso.inference$vars)
  
  #mean effect sizes
  effects <- lasso.inference$coef0
  
  #lower CI bound
  lower_ci <- lasso.inference$ci[,1]
  upper_ci <- lasso.inference$ci[,2]
  
  #pvalues
  pvalues <- lasso.inference$pv
  
  #create table
  lasso_inference_table <- data_frame(var_names = var_names,
                                      effect_size=effects,
                                      lower=lower_ci,
                                      upper=upper_ci,
                                      pvalues=pvalues)
  
  #annotate significant values
  lasso_inference_table$significant <- (lasso_inference_table$pvalues < 0.05)
  
  #convert to odds ratios
  lasso_inference_table <-
    lasso_inference_table %>%
    mutate_at(.vars = vars(effect_size, lower, upper), .funs = funs(exp)) %>%
    mutate(var_names = str_replace_all(var_names, '_', ' '))
  
  return(lasso_inference_table)
  
}


#' Plot output from perform_lasso_inference
#'
#' @param lasso_inference_table from perform_lasso_inference function
#' @param title desired plot title
#' @return ggplot2 object
#' @import ggplot2
#' @export

plot_lasso_inference <- function(lasso_inference_table, title=NULL){
  #plot pretty things
  out.plot <-
    ggplot(data = lasso_inference_table, aes(x = var_names, y = effect_size)) +
    #geom_point(aes(color = var_names), cex = 4) +
    geom_hline(aes(yintercept = 1), lty=2, color = 'black', alpha = 0.8) +
    geom_errorbar(aes(ymin = lower, ymax=upper, color = var_names), lwd=0.8) +
    geom_text(data= lasso_inference_table  %>% filter(significant==T),
              aes(x=var_names, y=(upper*2), label='*'),
              size=6)+
    ggtitle(title) +
    scale_color_discrete(guide=F) +
    scale_y_continuous(trans = 'log') +
    theme_tufte() +
    coord_flip() +
    xlab('Variables') +
    ylab('Odds Ratio') +
    theme(plot.title = element_text(size=16, hjust=0.5),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size=14))
  return(out.plot)
}



################ Process behavioral variables #####################

#' Create modified behavioral variables related to dwellings
#' @param df a dataframe containing the variables rooms_in_dwelling, people_in_dwelling, and males_in_dwelling
#' @return a modified dataframe
#' @export

modify_dwelling_vars <- function(df) {
  
  df %>%
    # filter out instances where males in dwelling are not less than or equal to the total
    # number of people in the dwelling
    filter(males_in_dwelling <= people_in_dwelling | is.na(males_in_dwelling)) %>%
    # create modified dwelling variables
    mutate(
      people_per_room = people_in_dwelling/rooms_in_dwelling,
      male_female_ratio = males_in_dwelling/(people_in_dwelling - males_in_dwelling)
    ) %>%
    # remove old dwelling variables
    select(-c(people_in_dwelling, males_in_dwelling, rooms_in_dwelling))
}


#' Discretize continuous behavioral variables
#' @param df a dataframe containing the variables age, people_per_room, children_in_dwelling, and male_female_ratio
#' @param age_breaks vector of the desired breakpoints for discretized age bins
#' @param age_labels vector of the desired labels for discretized age bins
#' @param people_per_room_breaks vector of the desired breakpoints for discretized people_per_room bins
#' @param people_per_room_labels vector of the desired labels for discretized people_per_room bins
#' @param children_in_dwelling_breaks vector of the desired breakpoints for discretized children_in_dwelling bins
#' @param children_in_dwelling_labels vector of the desired labels for discretized children_in_dwelling bins
#' @param male_female_ratio_breaks vector of the desired breakpoints for discretized male_female_ratio bins
#' @param male_female_ratio_labels vector of the desired labels for discretized male_female_ratio bins
#' @return a modified dataframe
#' @importFrom arules discretize
#' @export

discretize_behavioral_variables <-
  function(df,
           age_breaks = c(0, 12, 18, 41, 61, Inf),
           age_labels = c("under_12", "12_to_17", "18_to_40", "41_to_60", "over_60"),
           people_per_room_breaks = c(0, 1, 3, 10000, Inf),
           people_per_room_labels = c("less_than_1", "1_to_3", "3_plus", "no_rooms"),
           children_in_dwelling_breaks = c(0, 1, 2, 3, 4, Inf),
           children_in_dwelling_labels = c("0", "1", "2", "3", "4_or_more"),
           male_female_ratio_breaks = c(0, 0.5, 1.5, 10000, Inf),
           male_female_ratio_labels = c("less_than_0.5", "0.5_to_1.5", "1.5_plus", "no_females")) {
    
    df %>%
      # generate discretized behavioral variables
      mutate(
        age_discrete =
          arules::discretize(age, method = "fixed",
                             breaks = age_breaks,
                             labels = age_labels),
        people_per_room_discrete =
          arules::discretize(people_per_room, method = "fixed",
                             breaks = people_per_room_breaks,
                             labels = people_per_room_labels),
        children_in_dwelling_discrete =
          arules::discretize(children_in_dwelling, method = "fixed",
                             breaks = children_in_dwelling_breaks,
                             labels = children_in_dwelling_labels),
        male_female_ratio_discrete =
          arules::discretize(male_female_ratio, method = "fixed",
                             breaks = male_female_ratio_breaks,
                             labels = male_female_ratio_labels)
      ) %>%
      # remove raw behavioral variables
      select(-c(age, people_per_room, children_in_dwelling, male_female_ratio)) %>%
      # generate variables needed for spreading out into wide data
      mutate(
        age_discrete =
          paste0("age_discrete_", age_discrete),
        value_age_discrete = TRUE,
        people_per_room_discrete =
          paste0("people_per_room_discrete_", people_per_room_discrete),
        value_people_per_room_discrete = TRUE,
        children_in_dwelling_discrete =
          paste0("children_in_dwelling_discrete_", children_in_dwelling_discrete),
        value_children_in_dwelling_discrete = TRUE,
        male_female_ratio_discrete =
          paste0("male_female_ratio_discrete_", male_female_ratio_discrete),
        value_male_female_ratio_discrete = TRUE
      ) %>%
      # spread the discretized variables out into wide data
      spread(age_discrete, value_age_discrete, fill = FALSE) %>%
      spread(people_per_room_discrete, value_people_per_room_discrete, fill = FALSE) %>%
      spread(children_in_dwelling_discrete, value_children_in_dwelling_discrete, fill = FALSE) %>%
      spread(male_female_ratio_discrete, value_male_female_ratio_discrete, fill = FALSE)
  }



########### Plot Lasso #############

#' Make a coefficient plot of a fitted lasso model
#' @param lasso_obj the lasso object
#' @param dat model input data
#' @param pretty_names whether to clean up variable names for plotting
#' @param vert_line whether to have vertical line at log-odds = 1
#' @return a ggplot object
#' @export

plot_lasso_fit <- function(lasso_obj, dat, pretty_names = TRUE, vert_line = TRUE) {
  coefs <- coef(lasso_obj) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    rename(effect=s0) %>%
    filter(effect != 0) %>%
    filter(stringi::stri_detect_fixed(variable, "Intercept", negate=TRUE))
  
  in_mod <- coefs$variable
  in_mod_int <- in_mod[grepl(":", in_mod)] #identify interaction terms
  in_mod_int_split <- stringr::str_split(in_mod_int, ":") #split interaction terms
  
  #add interaction columns to data
  for(i in seq_along(in_mod_int)){
    dat %<>%
      mutate(!!in_mod_int[i] := !!as.name(in_mod_int_split[[i]][1]) & !!as.name(in_mod_int_split[[i]][2]))
  }
  
  #summarize n TRUE responses
  dat_sum <-  dat %>%
    select(!!in_mod) %>%
    sapply(., table) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("field") %>%
    mutate(prop_pos = `TRUE`/(`FALSE` + `TRUE`))
  
  if(pretty_names) {
    coefs <- coefs  %>%
      mutate(variable = stringi::stri_replace_all_fixed(variable, ":", " AND ")) %>%
      mutate(variable = stringi::stri_replace_all_fixed(variable, "TRUE", "")) %>%
      mutate(variable = stringi::stri_replace_all_fixed(variable,
                                                        pretty_var_names(),
                                                        names(pretty_var_names()),
                                                        vectorize_all = FALSE))
  }
  
  if(!is.null(attr(lasso_obj, "bootstrap_support"))) {
    support <- attr(lasso_obj, "bootstrap_support")
    support <- support[in_mod]
    n_pos <- dat_sum$`TRUE`
    coefs$variable <- paste0(coefs$variable, "  (s = ", round(support, 2), "; n = ", n_pos, ")")
  }
  
  coefs <- coefs %>%
    arrange(desc(effect)) %>%
    mutate(variable = fct_reorder(variable, effect)) %>%
    mutate(pred = plogis(lasso_obj$a0 + effect),
           positive = effect > 0)
  
  out_plot <- ggplot(coefs, aes(x = variable, y = effect, fill=positive)) +
    geom_point(pch = 21, stroke = 0.75, size = 4) +
    scale_fill_manual(values = viridis::plasma(2, begin = 0.3, end = 0.75)) +
    scale_y_continuous(breaks = log(c(0.1, 0.25, 0.5, .75, 1, 1.5, 2, 4, 10)), labels = exp) +
    coord_flip() +
    theme_fivethirtyeight() +
    theme(legend.position = "none") +
    theme(axis.title = element_text()) +
    ylab("Odds Ratios (log-odds scale)") +
    xlab("")
  
  if(vert_line){
    out_plot <- out_plot + geom_hline(yintercept = 0, lty=2, lwd=1)
  }
  
  out_plot
}


######## Support functions for outcome of interest in lasso plots ###########

if(outcome_var %in% c("ili", "encephalitis", "hemorrhagic_fever", "sari")){
  self_reports <- get_self_reports(dat) %>%
    select(participant_id, ends_with(outcome_var)) %>%
    rename(!!outcome_var := 2)
} else {
  self_reports <- dat %>%
    mutate(contact_any = ifelse(!!sym(outcome_var) == "", FALSE, TRUE)) %>%
    select(participant_id, contact_any) %>%
    rename(!!outcome_var := 2)
}