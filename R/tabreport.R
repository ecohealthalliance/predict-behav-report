binom_out <- function(x, n){
  binom.confint(x, n, methods = "wilson") %>%
    mutate_at(.vars = c("mean", "lower", "upper"), ~round(., 2)) %>%
    mutate(tog = paste0(mean, " (", lower, "-", upper, ")")) %>%
    pull(tog)
}

nonpara_out <- function(x){
  x <- x[!is.na(x)]
  a <-  median(x)
  n <- length(x)
  nboot <- 1000
  ii <- ceiling(n*runif(n*nboot))
  B <- x[ii]
  B <- array(B, c(nboot, n))
  m <- apply(B, 1, median)
  m <- sort(m)
  low <- c(m[25])
  high <- c(m[975])
  if(n < 3){
    low <- min(x)
    high <- max(x)
  }
  paste0(signif(a, 2), " (", signif(low, 2), "-", signif(high, 2), ")")
}

get_bars <- function(x, end){
  x <- x[!is.na(x)]
  a <-  median(x)
  n <- length(x)
  nboot <- 1000
  ii <- ceiling(n*runif(n*nboot))
  B <- x[ii]
  B <- array(B, c(nboot, n))
  m <- apply(B, 1, median)
  m <- sort(m)
  low <- c(m[25])
  high <- c(m[975])
  if(n < 3){
    low <- min(x)
    high <- max(x)
  }
  if(end=="low"){return(low)}
  if(end=="high"){return(high)}
}

# function for categorical vars
get_comp_table <- function(dat,
                           outcome_var,
                           outcome_class,
                           pretty_names,
                           group_var,
                           factor_levels=NULL,
                           factor_lab=factor_levels,
                           table_lab = paste(group_var), 
                           bars_only = FALSE)
{
  if(!group_var %in% colnames(dat)){return()}
  
  if(outcome_class == "contact"){outcome_var <- paste0(outcome_var, "_any")}
  outcome <- ifelse(outcome_var == "ili", "ILI", trimws(simple_cap(str_replace_all(outcome_var, "_|any", " "))))
  
  # Select data
  mdat <- dat %>%
    select(concurrent_sampling_site, !!group_var, !!outcome_var) %>%
    drop_na(!!group_var, !!outcome_var) %>%
    mutate(response = ifelse(!!sym(outcome_var)=="yes", 1, 0))  %>%
    select(-outcome_var)
  
  if(nrow(mdat)==0){return()}
  
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
  
  if(bars_only){
    odat <- sdat %>%
      mutate(mean = binom.confint(x = !!sym(outcome), n = `Total Count`, methods = "wilson")$mean,
             lower = binom.confint(x = !!sym(outcome), n = `Total Count`, methods = "wilson")$lower,
             upper = binom.confint(x = !!sym(outcome), n = `Total Count`, methods = "wilson")$upper,
             outcome = outcome) %>%
      select(-`Binomial Probability (95% CI)`, - `Total Count`) %>%
      mutate(!!group_var := factor(!!sym(group_var), 
                                   levels=factor_levels, 
                                   labels = factor_lab)) %>%
      filter(!is.na(!!sym(group_var))) %>%
      rename(!!sym(table_lab) := !!sym(group_var))
    return(odat)
  }
  
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
                               dist = "percentile bootstrap", 
                               bars_only = FALSE){
  
  if(all(is.na(dat %>% pull(!!group_var)))){return()}
  if(outcome_class == "contact"){outcome_var <- paste0(outcome_var, "_any")}
  outcome <- ifelse(outcome_var == "ili", "ILI", trimws(simple_cap(str_replace_all(outcome_var, "_|any", " "))))
  
  ci_func <- if(dist == "percentile bootstrap"){nonpara_out}
  
  # Select data
  mdat <- dat %>%
    select(concurrent_sampling_site, !!group_var, !!outcome_var) %>%
    drop_na(!!group_var, !!outcome_var) %>%
    gather(key = "outcome", value = "response", -concurrent_sampling_site, -!!group_var) %>%
    mutate(response = ifelse(response=="yes", !!outcome, paste("No", !!outcome))) 
  
  # Summarize data by site
  sdat <- mdat %>%
    group_by(concurrent_sampling_site, response) %>%
    summarize(median := median(!!sym(group_var)),
              lower := get_bars(!!sym(group_var), end="low"),
              upper := get_bars(!!sym(group_var), end="high"),
              #"Mean (95% CI)" := ci_func(!!sym(group_var)),
              "Median (95% CI)" := ci_func(!!sym(group_var)),
              Count = n()) %>%
    ungroup() 
  
  # Summarize data all
  sdat_all <- mdat %>%
    group_by(response) %>%
    summarize(median := median(!!sym(group_var)),
              lower := get_bars(!!sym(group_var), end="low"),
              upper := get_bars(!!sym(group_var), end="high"),
              "Median (95% CI)" := ci_func(!!sym(group_var)),
              Count = n()) %>%
    ungroup() %>%
    mutate(concurrent_sampling_site = "Aggregate")
  
  sdat <- bind_rows(sdat, sdat_all) 
  
  if(bars_only){
    odat <- sdat %>%
      select(-`Median (95% CI)`, -Count)
    return(odat)
  } else {
    sdat <- sdat %>% select(-median, -lower, -upper)
  }
  
  
  # Format for output
  odat <- sdat %>%
    mutate(concurrent_sampling_site = factor(concurrent_sampling_site, 
                                             levels = pretty_names$old, 
                                             labels = pretty_names$new)) %>%
    gather(variable, value, -(response:concurrent_sampling_site)) %>%
    mutate(variable = factor(variable, levels = c("Count", "Median (95% CI)"))) %>%
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
