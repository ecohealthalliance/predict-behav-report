binom_out <- function(x, n){
  binom.confint(x, n, methods = "wilson") %>%
    mutate_at(.vars = c("mean", "lower", "upper"), ~round(., 2)) %>%
    mutate(tog = paste0(mean, " (", lower, "-", upper, ")")) %>%
    pull(tog)
}

norm_out <- function(x){
  x <- x[!is.na(x)]
  a <-  mean(x)
  s <-  sd(x)
  n <- length(x)
  error <- qt(0.975, df = n-1)*s/sqrt(n)
  low <- a-error
  high <- a+error
  if(n < 3){
    low <- min(x)
    high <- max(x)
  }
  paste0(signif(a, 2), " (", signif(low, 2), "-", signif(high, 2), ")")
}

# function for categorical vars
get_comp_table <- function(dat,
                           outcome_var,
                           outcome_class,
                           pretty_names,
                           group_var,
                           factor_levels=NULL,
                           factor_lab=factor_levels,
                           table_lab = paste(group_var))
{
  if(!group_var %in% colnames(dat)){return()}
  
  if(outcome_class == "contact"){outcome_var <- paste0(outcome_var, "_any")}
  outcome <- ifelse(outcome_var == "ili", "ILI", simple_cap(str_replace_all(outcome_var, "_", " ")))
  
  # Select data
  mdat <- eidith::ed2_expand_long(dat, !!sym(group_var), other_details = TRUE) %>%
    select(participant_id, concurrent_sampling_site, !!paste0(group_var, "_val"), !!outcome_var) %>%
    rename(!!group_var := !!paste0(group_var, "_val")) %>%
    select(-participant_id) %>% 
    gather(key = "outcome", value = "response", -concurrent_sampling_site, -!!group_var) %>%
    mutate(response = ifelse(response=="yes", 1, 0)) 
  
  # Summarize data by site
  sdat <- mdat %>%
    group_by(concurrent_sampling_site, !!sym(group_var)) %>%
    summarize(!!outcome := sum(response),
              `Total Count` = n(),
              `Binomial Probability (95% CI)` = binom_out(x = !!sym(outcome), n = `Total Count`)) %>%
    ungroup() 
  
  # Summarize data all
  sdat_all <- mdat %>%
    group_by(!!sym(group_var)) %>%
    summarize(!!outcome := sum(response),
              `Total Count` = n(),
              `Binomial Probability (95% CI)` = binom_out(x = !!sym(outcome), n = `Total Count`)) %>%
    ungroup() %>%
    mutate(concurrent_sampling_site = "Aggregate")
  
  sdat <- bind_rows(sdat, sdat_all)
  
  # Format for output
  odat <- sdat %>%
    mutate(!!group_var := factor(!!sym(group_var), 
                                 levels=factor_levels, 
                                 labels = factor_lab),
           concurrent_sampling_site = factor(concurrent_sampling_site, 
                                             levels = pretty_names$old, 
                                             labels = pretty_names$new)) %>%
    arrange(concurrent_sampling_site) %>%
    gather(variable, value, -(!!sym(group_var):concurrent_sampling_site)) %>%
    mutate(variable = factor(variable, levels = c(!!outcome, "Total Count",  "Binomial Probability (95% CI)"))) %>%
    mutate(univar = interaction(variable, concurrent_sampling_site), concurrent_sampling_site = NULL, variable = NULL) %>%
    na.omit() %>%
    spread(univar, value) %>%
    arrange(!!sym(group_var)) %>%
    rename(` ` = !!sym(group_var)) 
  
  table_h <- odat[1,] %>%
    mutate_all(function(x)"") %>%
    mutate(` ` = table_lab)
  
  out <- suppressWarnings(bind_rows(table_h, odat))
  out
}

# function for numeric vars
get_comp_table_num <- function(dat,
                               outcome_var,
                               outcome_class,
                               pretty_names,
                               group_var,
                               table_lab = paste(group_var),
                               dist = "normal"){
  
  if(outcome_class == "contact"){outcome_var <- paste0(outcome_var, "_any")}
  outcome <- ifelse(outcome_var == "ili", "ILI", simple_cap(str_replace_all(outcome_var, "_", " ")))
  
    ci_func <- if(dist == "normal"){norm_out}
  
  # Select data
  mdat <- dat %>%
    select(participant_id, concurrent_sampling_site, !!group_var, !!outcome_var) %>%
    select(-participant_id) %>% 
    gather(key = "outcome", value = "response", -concurrent_sampling_site, -!!group_var) %>%
    mutate(response = ifelse(response=="yes", !!outcome, paste("No", !!outcome))) 
  
  # Summarize data by site
  sdat <- mdat %>%
    group_by(concurrent_sampling_site, response) %>%
    summarize("Mean (95% CI)" := ci_func(!!sym(group_var)),
              Count = n()) %>%
    ungroup() 
  
  # Summarize data all
  sdat_all <- mdat %>%
    group_by(response) %>%
    summarize("Mean (95% CI)" := ci_func(!!sym(group_var)),
              Count = n()) %>%
    ungroup() %>%
    mutate(concurrent_sampling_site = "Aggregate")
  
  sdat <- bind_rows(sdat, sdat_all) 
  
  # Format for output
  odat <- sdat %>%
    mutate(concurrent_sampling_site = factor(concurrent_sampling_site, 
                                             levels = pretty_names$old, 
                                             labels = pretty_names$new)) %>%
    gather(variable, value, -(response:concurrent_sampling_site)) %>%
    mutate(variable = factor(variable, levels = c("Count", "Mean (95% CI)"))) %>%
    mutate(univar = interaction(variable, concurrent_sampling_site), concurrent_sampling_site = NULL, variable = NULL) %>%
    spread(univar, value) %>%
    arrange(response) %>%
    rename(` ` = response) 
  
  table_h <- odat[1,] %>%
    mutate_all(function(x)"") %>%
    mutate(` ` = table_lab)
  
  out <- suppressWarnings(bind_rows(table_h, odat))
  out
  
}