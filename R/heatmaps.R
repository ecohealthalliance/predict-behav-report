#' Heatmap function with formatted dataset
#' 
#' @param dat humans and events datasets combined (e.g., left_join(humans, events, by = "event_name"))
#' and formatted (e.g., get_formatted_dat)
#' @param taxa_types string of taxa name for stratification
#' @param contx_types string of contact name for stratification
#' @param title plot title
#' @param group_var name of column for facet wrapping (e.g., "gender").  Optional.
#' @return ggplot2 heatmap object

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


# main heatmap function

get_heatmap <- function(dat,
                        taxa_types = c("bats", "nhp", "poultry", "rodents", "swine", "birds",
                                       "cattle", "ungulates", "pangolins", "carnivores",
                                       "goats_sheep", "camels", "dogs", "cats"),
                        contx_types = c('pets',
                                        'handled',
                                        'raised',
                                        'feces in or near food',
                                        'in house',
                                        'cooked/handled',
                                        'eaten raw/undercooked',
                                        'eaten sick',
                                        'found dead/collected',
                                        'scratched/bitten',
                                        'hunted/trapped',
                                        'slaughtered',
                                        'no contact'),
                        title,
                        group_var = ""){
  
  taxa_types_col <- paste(taxa_types, collapse = "|")
  
  fct_levels <- c('pets',
                  'handled',
                  'raised',
                  'feces in or near food',
                  'in house',
                  'cooked handled',
                  'eaten raw undercooked',
                  'eaten sick',
                  'found dead collected',
                  'scratched bitten',
                  'hunted trapped',
                  'slaughtered',
                  'no contact')
  
  cdat <- dat %>%
    group_by_at(vars(suppressWarnings(one_of(!!group_var)))) %>%
    count() %>%
    rename(tot = n)
  
  hdat <- dat %>%
    select(suppressWarnings(one_of(group_var)), matches(taxa_types_col)) %>%
    gather(key = key, value = value, -suppressWarnings(one_of(group_var))) %>%
    filter(value == TRUE) %>%
    group_by_at(vars(suppressWarnings(one_of(group_var)), key)) %>%
    count() %>%
    ungroup() %>%
    mutate(taxa = str_extract(key, taxa_types_col),
           contx = str_replace_all(key, paste0("_|contact|", taxa_types_col), " ") %>%
             trimws(.) %>%
             ifelse(. == "no", "no contact", .) %>%
             factor(., levels = fct_levels,
                    labels = contx_types)) %>%
    select(-key)
  
  if(group_var == ""){
    hdat <- hdat %>%
      complete(contx, nesting(taxa), fill = list(n = 0)) %>%
      mutate(tot = cdat$tot) %>%
      mutate(perc = round(100* n/tot)) %>%
      mutate(lab = paste0(perc, "%", " (", n, ")"))
  }else{
    hdat <- hdat %>%
      complete(contx, nesting(!!sym(group_var), taxa), fill = list(n = 0)) %>%
      left_join(cdat) %>%
      mutate(perc = round(100* n/tot)) %>%
      mutate(lab = paste0(perc, "%", " (", n, ")"))
  }
  
  p <- ggplot(data = hdat, aes(x = taxa, y = contx, fill = n, label = lab)) +
    geom_tile(color = 'gray') +
    geom_label(fontface='bold', fill="white") +
    scale_x_discrete(position = 'bottom') +
    scale_fill_viridis_c(guide = FALSE) +
    theme_fivethirtyeight() +
    theme(plot.title = element_text(hjust=0.5),
          strip.text = element_text(size=14),
          axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=12)) +
    labs(title = title)
  
  if(group_var != ""){
    p <- p + facet_wrap(as.formula(paste("~", group_var)))
  }
  p
}