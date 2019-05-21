# create data frame of site names
get_site_names <- function(dat, site_lookup){ # input dataframe in format of site-name-lookup.csv
  
  n_respondents <- dat %>%
    group_by(concurrent_sampling_site) %>%
    count() %>%
    ungroup() 
  
  site_lookup %>%
    filter(country == !!country,
           old %in% unique(dat$concurrent_sampling_site)) %>%
    left_join(n_respondents, by = c("old" = "concurrent_sampling_site")) %>%
    mutate(new = paste0(new, " (n = ", n, ")")) %>%
    select(-n) %>%
    bind_rows(tibble(new = paste0("Aggregate (n = ", sum(n_respondents$n), ")" ),
                     old = "Aggregate", country = country))
}

# get behavioral data
get_behav <- function(country, download = FALSE){
  
  if(download){
    ed_db_download(verbose = TRUE, p1_tables = NULL, p2_tables = c("Event", "Human"), p1_data = FALSE)
  }
  
  human <- eidith::ed2_human()
  event <- eidith::ed2_events() %>% 
    filter(project == "P2") %>%
    select(event_name, concurrent_sampling_site, country, season,
           human_density_impact, disease_transmission_interfaces,
           veterinarian_care, sampling_area_size, humans_present,
           rodents_present, bats_present, nhp_present, poultry_present,
           pangolins_present, ungulates_present, birds_present,
           cattle_present, goats_present, swine_present, cats_present,
           dogs_present, carnivores_present, camels_present,
           community_engagement, vector_control_measures, insect_vectors,
           average_trip_to_water, toilets_available, drinking_water_shared,
           bathing_water_shared, drinking_water_source) %>%
    rename(drinking_water_source_site = drinking_water_source)
  
  out <- left_join(human, event) %>%
    filter(country == !!country,
           concurrent_sampling_site  != "Not Mapped")
  
  # recode 
  out <- out %>%
    mutate_if(is.character, ~replace_na(., "N/A")) %>%
    mutate(drinking_water_shared = recode(drinking_water_shared, "don't know" = "unknown"),
           bathing_water_shared = recode(bathing_water_shared, "don't know" = "unknown"),
           had_symptoms_in_last_year = recode(had_symptoms_in_last_year, "N/A" = "no"),
           insect_vectors = str_replace_all(tolower(insect_vectors), c("sand fly" = "sand", 
                                                                       "tsetse fly" = "tsetse", 
                                                                       "housefly" = "other",
                                                                       "house fly" = "other",
                                                                       "fly" = "other",
                                                                       "flies" = "other",
                                                                       "cockroach" = "other",
                                                                       "other, other" = "other")),
           treatment_specific = str_replace_all(tolower(treatment), c("clinic/health center" = "clinic", 
                                                                      "hospital" = "clinic", 
                                                                      "community health worker" = "clinic",
                                                                      "mobile clinic" = "clinic",
                                                                      "dispensary or pharmacy" = "dispensary or pharmacy only",
                                                                      "traditional healer" = "traditional healer only")),
           treatment_specific = map_chr(str_split(treatment_specific, "; "), function(x){
             unique(x) %>%
               ifelse(length(.)==1, ., "multiple sources") %>%
               recode(., "clinic" = "clinic, hospital, or community health worker only")}),
           scratched_bitten_action_specific = scratched_bitten_action, 
           scratched_bitten_action = ifelse(str_detect(scratched_bitten_action, "visit|soap"),
                                            "treated", 
                                            ifelse(str_detect(scratched_bitten_action, "someone|bandage|rinse|nothing"),
                                                   "untreated",
                                                   "N/A")),
           risk_open_wound_specific = str_replace_all(risk_open_wound, c("yes, " = "",
                                                                         "but" = "there are risks, but",
                                                                         "^no$" = "N/A",
                                                                         "don't  know" = "N/A")),
           risk_open_wound_specific = ifelse(str_detect(risk_open_wound_specific, "other"), "other", risk_open_wound_specific),
           risk_open_wound = ifelse(str_detect(risk_open_wound, "yes"), "yes",
                                    ifelse(str_detect(risk_open_wound, "other"), "other",
                                           risk_open_wound)),
           occupation = ifelse(str_detect(primary_livelihood, "[Oo]ther"), livelihood_groups_other, primary_livelihood)
    )
  
}


#' Create analysis dataframe
#'
#' Creates data frame ready for random forest
#' @param dat data frame from which to create analysis data set created from get_self_reports or get_contaxa
#' @param taxa_names vector of taxa types for which exposures will be included
#' @param lifetime  If TRUE, report symptoms and predictors for lifetime, otherwise past year symptoms and all predictors
#' @author matteo-V
#' @importFrom dplyr %>% select mutate
#' @importFrom rlang !! sym :=
#' @importFrom plyr join_all
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
                              combine_illness = FALSE,
                              illness_var = c()) {
  
  # select and widen covariate data
  covars <- dat %>%
    select(age,
           gender,
           length_lived,
           matches("dwelling"),
           water_treated,
           shared_water_last_year,
           shared_water_life,
           matches("education"),
           occupation,
           matches("animal"),
           dedicated_location_for_waste,
           matches("meat"),
           scratched_bitten_action,
           matches("hunted"),
           worried_about_disease,
           risk_open_wound,
           travelled,
           travel_reason,
           treatment,
           had_symptoms_in_last_year,
           had_symptoms_in_last_year_other_people,
           illness_death,
           -matches("_contact"),
           -matches("_notes"), 
           -matches("_life"))  %>%
    #mutate character vectors to factors where appropriate
    mutate_at(.vars = vars(gender,
                           occupation,
                           length_lived,
                           highest_education,
                           highest_education_mother,
                           travel_reason, 
                           treatment,
                           scratched_bitten_action),
              .funs = funs(factor)) %>%
    #mutate numeric vectors
    mutate_at(.vars = vars(age,
                           people_in_dwelling,
                           children_in_dwelling,
                           males_in_dwelling,
                           rooms_in_dwelling),
              .funs = funs(as.numeric)) %>%
    #map yes to TRUE and all other responses to FALSE
    mutate_if(.predicate = is.character,
              .funs = funs(
                ifelse(str_detect(., '[Yy]es'), TRUE, FALSE))) %>%
    #expansion of factors to binary categorical outcomes
    ed2_expand_wide(occupation) %>%
    ed2_expand_wide(highest_education) %>%
    ed2_expand_wide(highest_education_mother) %>%
    ed2_expand_wide(gender) %>%
    ed2_expand_wide(length_lived) %>%
    ed2_expand_wide(travel_reason) %>%
    ed2_expand_wide(treatment) %>%
    ed2_expand_wide(scratched_bitten_action) %>%
    select(-occupation,
           -highest_education,
           -highest_education_mother,
           -gender,
           -length_lived,
           -travel_reason,
           -treatment,
           -scratched_bitten_action,
           -ends_with("n_a")) %>%
    mutate(participant_id = dat$participant_id)
  
  # remove most "last_year" covariates since more detailed info on these exposures will
  # come from "get_clean_exposures()", but keep "shared_water_last_year" and
  # "animals_in_food_last_year" as is
  last.year.vars <- grep("_last_year", colnames(covars), value = T)
  last.year.vars.to.keep <- c("shared_water_last_year", "animals_in_food_last_year")
  last.year.vars.to.exclude <- last.year.vars[!(last.year.vars %in% last.year.vars.to.keep)]
  
  covars <- select(covars, -last.year.vars.to.exclude)
  
  # get exposure data
  for(taxa in taxa_names){
    
    contx_type <- paste0(taxa, '_contact')
    no_contx_type <- paste0('no_', contx_type)
    
    exposures <- dat %>%
      select(participant_id, !!sym(contx_type)) %>% 
      ed2_expand_wide(!!sym(contx_type)) %>% #expand into wide frame of binary vars
      rename( !!sym(no_contx_type) := n_a) %>% #use special assign := to work with !!sym(var)
      select(-!!sym(contx_type)) #remove original multiresponse from final frame
    
    covars <- left_join(covars, exposures)
  }
  # illness data
  # if(length(illness_var)>0){
  #   
  #   illness_var_match <- paste(illness_var, collapse = "|")
  #   illness_var_field <- paste(illness_var, collapse = "_OR_")
  #   
  #   if(combine_illness) {
  #     illness <- dat %>%
  #       get_self_reports(lifetime = lifetime) %>%
  #       select(participant_id, matches(illness_var_match)) %>%
  #       mutate(!!illness_var_field := suppressWarnings(as.factor(apply(select(.,matches(illness_var_match)), 1, any)))) %>%
  #       select(participant_id, !!illness_var_field)
  #   } else{illness <- dat %>%
  #     get_self_reports(lifetime = lifetime) %>%
  #     select(participant_id, matches(illness_var_match)) %>%
  #     rename_at(vars(matches(illness_var_match)), ~gsub('.*[_]', '', .))
  #   }
  #   
  #   covars <- left_join(covars, illness)
  # }
  covars
}
