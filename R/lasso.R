# function to run lasso analysis
get_lasso <- function(dat, illness_outcomes, taxa_outcomes){
  
  outcomes <- c(illness_outcomes, taxa_outcomes)
  
  map(outcomes, function(endpt){
    
    if(endpt %in% illness_outcomes){
      dat <- get_outcomes(dat, taxa_outcomes = taxa_outcomes, illness_outcomes = endpt) 
      endpt_mod <- endpt
    }
    if(endpt %in% taxa_outcomes){
      dat <- get_outcomes(dat, taxa_outcomes = endpt, illness_outcomes = illness_outcomes) %>%
        select(-starts_with(endpt)) %>%
        mutate(!!paste0(endpt, "_contact") := ifelse(!!sym(paste0("no_", endpt, "_contact")), FALSE, TRUE)) %>%
        select(-starts_with("no_"))
      endpt_mod <- paste0(endpt, "_contact")
    }
    
    write_csv(dat, h("lasso", paste(country, endpt, "lasso-dat.csv", sep = "-")))
    
    # create matrix
    lasso_matrix <- dat %>%
      make_matrix(restricted_interactions = TRUE, outcome_var  = endpt_mod) %>%
      remove_colinear_columns()
    
    lasso_fit <- fit_lasso_model(lasso_matrix$model_data_matrix, response = pull(dat, !!endpt_mod))
    
    write_rds(lasso_fit, h("lasso", paste(country, endpt, "lasso-fit.rds", sep = "-")))
    
    coefs <- coef(lasso_fit) %>%
      as.matrix() %>%
      as.data.frame() %>%
      rownames_to_column("variable") %>%
      rename(effect=s0) %>%
      mutate(effect = exp(effect)) %>%
      arrange(-effect)
    
    write_csv(coefs, h("lasso", paste(country, endpt, "lasso-odds-ratios.rds", sep = "-")))
    
  })
  
}

# make matrix for lasso run
make_matrix <- function(dat,
                        full_interactions = FALSE,
                        restricted_interactions = TRUE,
                        outcome_var) {

  # Define basic model formula
  form <- as.formula(paste0(outcome_var, ' ~ ', ' + ', '.'))

  # Define full interaction formula
  if(full_interactions|restricted_interactions) {

    form <- as.formula(paste0(outcome_var, ' ~ ', ' + ', '.', ' + ', '(.)^2'))
  }

  # Generate initial model matrix and remove intercept term
  model_matrix <- stats::model.matrix(form, data = dat)
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

# remove colinear variables before running lasso
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
# Perform cross-validation to get optimal elastic net parameter
# cv_elasticnet_param <- function(alpha_vals, model_matrix, response, cv_folds){
#   cat("Performing alpha cross validation\n")
#   res <- list()
#   for(alpha in alpha_vals){
#     lambda.fit <- cv.glmnet(x = model_matrix,
#                             y = response ,
#                             nfolds = cv_folds, #5-fold CV
#                             family = 'binomial',
#                             alpha = alpha)
#     res[[as.character(alpha)]] <- lambda.fit$cvm
#   }
#   res <- stack(res)
#   #get min alpha
#   as.numeric(as.character(res$ind[which.min(res$values)]))
# 
# }


# Perform cross-validation to get optimal lambda (shrinkage) parameter
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


# Fit glmnet with optimal lambda on training data
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


# Computes misclassification rate of a fitted lasso model
# compute_lasso_performance <- function(lasso_fit, model_matrix, response){
#   #get class predictions
#   pred <- predict(lasso_fit,
#                   newx = model_matrix,
#                   type = 'class')
# 
#   pred <- as.logical(pred)
#   #get misclassification rate
#   lasso.conf.matrix <- table(pred, response)
#   cat('Confusion Matrix:\n')
#   print(lasso.conf.matrix)
#   #get misclassification rate
#   misclass_rate <- 1 - ( sum(diag(lasso.conf.matrix)) / sum(lasso.conf.matrix))
#   cat("\nMisclassification rate:", misclass_rate)
#   #compute sensitivity
#   sens <- lasso.conf.matrix[2,2] / (lasso.conf.matrix[2,2] + lasso.conf.matrix[1,2])
#   cat("\nFalse negative rate:", 1 - sens)
#   #compute specificity
#   specif <- lasso.conf.matrix[1,1] / (lasso.conf.matrix[1,1] + lasso.conf.matrix[2,1])
#   cat("\nFalse positive rate:", 1 - specif)
# }


# Perform Inference on fitted lasso model
# perform_lasso_inference <- function(lasso_fit, model_matrix, response){
#   warning("This function may take a while to execute fully")
#   #get lambda and N
#   lam <- lasso_fit$lambda
#   n <- length(response)
#   #get betas
#   betas <- coef(lasso_fit,
#                 s = lam/n) # removes the intercept
# 
#   #perform inference for glmnet (LASSO) model
#   lasso.inference <-
#     fixedLassoInf(x = model_matrix,
#                   y = as.logical( response )*1 ,
#                   beta = betas,
#                   lambda = lam,
#                   family = 'binomial',
#                   #  gridrange = c(1e-3, 1e3),
#                   alpha = 0.05,
#                   intercept=TRUE,
#                   type = "partial",
#                   verbose=TRUE)
# 
#   #variables names for output
#   var_names <- names(lasso.inference$vars)
# 
#   #mean effect sizes
#   effects <- lasso.inference$coef0
# 
#   #lower CI bound
#   lower_ci <- lasso.inference$ci[,1]
#   upper_ci <- lasso.inference$ci[,2]
# 
#   #pvalues
#   pvalues <- lasso.inference$pv
# 
#   #create table
#   lasso_inference_table <- data_frame(var_names = var_names,
#                                       effect_size=effects,
#                                       lower=lower_ci,
#                                       upper=upper_ci,
#                                       pvalues=pvalues)
# 
#   #annotate significant values
#   lasso_inference_table$significant <- (lasso_inference_table$pvalues < 0.05)
# 
#   #convert to odds ratios
#   lasso_inference_table <-
#     lasso_inference_table %>%
#     mutate_at(.vars = vars(effect_size, lower, upper), .funs = funs(exp)) %>%
#     mutate(var_names = str_replace_all(var_names, '_', ' '))
# 
#   return(lasso_inference_table)
# 
# }


# Plot output from perform_lasso_inference
# plot_lasso_inference <- function(lasso_inference_table, title=NULL){
#   #plot pretty things
#   out.plot <-
#     ggplot(data = lasso_inference_table, aes(x = var_names, y = effect_size)) +
#     #geom_point(aes(color = var_names), cex = 4) +
#     geom_hline(aes(yintercept = 1), lty=2, color = 'black', alpha = 0.8) +
#     geom_errorbar(aes(ymin = lower, ymax=upper, color = var_names), lwd=0.8) +
#     geom_text(data= lasso_inference_table  %>% filter(significant==T),
#               aes(x=var_names, y=(upper*2), label='*'),
#               size=6)+
#     ggtitle(title) +
#     scale_color_discrete(guide=F) +
#     scale_y_continuous(trans = 'log') +
#     theme_tufte() +
#     coord_flip() +
#     xlab('Variables') +
#     ylab('Odds Ratio') +
#     theme(plot.title = element_text(size=16, hjust=0.5),
#           axis.title.y = element_blank(),
#           axis.title.x = element_text(size=14))
#   return(out.plot)
# }



########### Plot Lasso #############
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
    dat <- dat %>%
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
    if(pretty_names) {
      coefs <- coefs  %>%
        mutate(variable = gsub("action_|discrete_", "", variable)) %>%
        mutate(variable = gsub(" to ", "-", variable)) %>%
        mutate(variable = gsub(":", " AND ", variable)) %>%
        mutate(variable = gsub("_", " ", variable))
    }
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
    scale_fill_manual(values = c("#7E03A8FF", "#F89441FF")) +
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
