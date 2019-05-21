#'  make tabular tables for binomial probabilities and mean comparisons
#'
#' @param dat humans and events datasets combined (e.g., left_join(humans, events, by = "event_name"))
#' @param outcome_var outcome variable of interest (e.g., ili, sari, bats_contact, rodents_contact)
#' @param pretty_name dataframe of old and new place names
#' @param group_var grouping variable
#' @param factor_levels factors within the grouping var
#' @param factor_lab default is same as levels
#' @param table_lab table name - default is group_var


# support functions
simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

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
                           outcome_var = outcome_var,
                           pretty_names,
                           group_var,
                           factor_levels=NULL,
                           factor_lab=factor_levels,
                           table_lab = paste(group_var))
{
  
  # get outcome var
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
  
  outcome <- simple_cap(gsub("_", " ", outcome_var))
  
  # Select data
  mdat <- eidith::ed2_expand_long(dat, !!sym(group_var), other_details = TRUE) %>%
    select(participant_id, concurrent_sampling_site, !!paste0(group_var, "_val")) %>%
    rename(!!group_var := !!paste0(group_var, "_val")) %>%
    left_join(self_reports) %>%
    select(-participant_id) %>% 
    gather(key = "outcome", value = "response", -concurrent_sampling_site, -!!group_var) %>%
    mutate(response = ifelse(response, 1, 0)) 
  
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
                               outcome_var = outcome_var,
                               pretty_names,
                               group_var,
                               table_lab = paste(group_var),
                               dist = "normal"){
  # Get outcome var
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
  
  outcome <- simple_cap(gsub("_", " ", outcome_var))
  ci_func <- if(dist == "normal"){norm_out}
  
  # Select data
  mdat <- dat %>%
    select(participant_id, concurrent_sampling_site, !!group_var) %>%
    left_join(self_reports) %>%
    select(-participant_id) %>% 
    gather(key = "outcome", value = "response", -concurrent_sampling_site, -!!group_var) %>%
    mutate(response = ifelse(response, !!outcome, paste("No", !!outcome))) %>%
    mutate(!!group_var := as.numeric(!!sym(group_var)))
  
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