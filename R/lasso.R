# function to run lasso analysis
get_lasso <- function(dat, illness_outcomes, taxa_outcomes){
  
  outcomes <- c(illness_outcomes, taxa_outcomes)
  
  map(outcomes, function(endpt){
    
    if(endpt %in% illness_outcomes){
      mdat <- get_outcomes(dat, taxa_outcomes = taxa_names, illness_outcomes = endpt) 
      endpt_mod <- endpt
    }
    if(endpt %in% taxa_outcomes){
      endpt_mod <- paste0(endpt, "_contact")
      mdat <- get_outcomes(dat, taxa_outcomes = endpt, illness_outcomes = illness_names_clean) %>%
        mutate(!!endpt_mod := ifelse(!!sym(paste0(endpt_mod, "_none")), FALSE, TRUE)) %>% 
        select(-starts_with(paste0(endpt_mod, "_"))) 
    }
    
    mdat <- mdat %>% 
      select(-participant_id)  %>%
      remove_empty(which = "cols")
    
    write_csv(mdat, h("data", "lasso", paste(country, endpt, "lasso-dat.csv", sep = "-")))
    
    # create matrix
    lasso_matrices <- mdat %>%
      ehalasso::make_matrix(interactions = TRUE, interaction_vars = c("gender", "highest_education", "highest_education_mother",
                                                                      "length_lived", "age", "concurrent_site"),
                            outcome_var  = endpt_mod, interaction_regex = TRUE)
    
    # remove colinear columns
    lasso_predictor_matrix <- remove_colinear_columns(lasso_matrices$model_matrix)
    
    # fit model
    lasso_fit <- fit_lasso_model(lasso_predictor_matrix$intermediate_matrix,
                                 lasso_matrices$outcome_matrix)
    
    write_rds(lasso_fit, h("data/lasso", paste(country, endpt, "lasso-fit.rds", sep = "-")))
    
    coefs <- coef(lasso_fit) %>%
      as.matrix() %>%
      as.data.frame() %>%
      rownames_to_column("variable") %>%
      rename(effect=s0) %>%
      mutate(effect = exp(effect)) %>%
      arrange(-effect)
    
    write_csv(coefs, h("data", "lasso", paste(country, endpt, "lasso-odds-ratios.rds", sep = "-")))
    
  })
 
}

########### Plot Lasso #############
plot_lasso_fit <- function(lasso_obj, dat, pretty_names = TRUE, vert_line = TRUE, title = "", support_threshold = 0.6) {
  
  support <- attr(lasso_obj, "bootstrap_support")
  
  coefs <- coef(lasso_obj) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable") %>%
    rename(effect=s0) %>%
    filter(effect != 0) %>%
    filter(stringi::stri_detect_fixed(variable, "Intercept", negate=TRUE)) %>% 
    mutate(support = support[variable]) %>% 
    filter(support >= support_threshold)
  
  if(!nrow(coefs)){ return(NULL)}

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
    rownames_to_column("variable") %>%
    mutate(prop_pos = `TRUE`/(`FALSE` + `TRUE`))

  coefs <- coefs %>%
    left_join(dat_sum) %>%
    mutate(variable = paste0(variable, "  (s = ", round(support, 2), "; n = ", `TRUE`, ")"))

  if(pretty_names) {
    coefs <- coefs  %>%
      mutate(variable = gsub("action_|discrete_", "", variable)) %>%
      mutate(variable = gsub(" to ", "-", variable)) %>%
      mutate(variable = gsub(":", " AND ", variable)) %>%
      mutate(variable = gsub("_", " ", variable))
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
    labs(y = "Odds Ratios (log-odds scale)", x = "", title = title)

  if(vert_line){
    out_plot <- out_plot + geom_hline(yintercept = 0, lty=2, lwd=1)
  }

  out_plot
}
