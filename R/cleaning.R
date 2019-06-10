# create data frame of site names
get_site_names <- function(dat, site_lookup, country){ # input dataframe in format of site-name-lookup.csv
  
  n_respondents <- dat %>%
    group_by(concurrent_sampling_site) %>%
    count() %>%
    ungroup() 
  
  site_lookup %>%
    rename(full_site_name = new) %>%
    filter(country == !!country,
           old %in% unique(dat$concurrent_sampling_site)) %>%
    left_join(n_respondents, by = c("old" = "concurrent_sampling_site")) %>%
    mutate(new = paste0(full_site_name, " (n = ", n, ")")) %>%
    bind_rows(tibble(full_site_name = "Aggregate",
                     n = sum(n_respondents$n),
                     new = paste0("Aggregate (n = ", sum(n_respondents$n), ")" ),
                     old = "Aggregate", 
                     country = country))
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
           site_latitude, site_longitude,
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
    mutate_at(.vars = vars(rooms_in_dwelling, people_in_dwelling, children_in_dwelling, males_in_dwelling,
                           site_latitude, site_longitude), .funs = as.numeric) %>%
    mutate(drinking_water_shared = dplyr::recode(drinking_water_shared, "don't know" = "unknown"),
           bathing_water_shared = dplyr::recode(bathing_water_shared, "don't know" = "unknown"),
           had_symptoms_in_last_year = dplyr::recode(had_symptoms_in_last_year, "N/A" = "no"),
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
               dplyr::recode(., "clinic" = "clinic, hospital, or community health worker only")}),
           scratched_bitten_action_specific = scratched_bitten_action, 
           scratched_bitten_action = ifelse(str_detect(scratched_bitten_action, "visit|soap"),
                                            "treated", 
                                            ifelse(str_detect(scratched_bitten_action, "someone|bandage|rinse|nothing"),
                                                   "untreated",
                                                   "N/A")),
           scratched_bitten_treated = ifelse(str_detect("treated", scratched_bitten_action), "yes", 
                                             ifelse(str_detect("untreated", scratched_bitten_action), "no", NA)),
           risk_open_wound_specific = str_replace_all(risk_open_wound, c("yes, " = "",
                                                                         "but" = "there are risks, but",
                                                                         "^no$" = "N/A",
                                                                         "don't  know" = "N/A")),
           risk_open_wound_specific = ifelse(str_detect(risk_open_wound_specific, "other"), "other", risk_open_wound_specific),
           risk_open_wound = ifelse(str_detect(risk_open_wound, "yes"), "yes",
                                    ifelse(str_detect(risk_open_wound, "other"), "other",
                                           risk_open_wound)),
           occupation = ifelse(str_detect(primary_livelihood, "[Oo]ther"), livelihood_groups_other, primary_livelihood),
           people_in_dwelling = people_in_dwelling + 1,
           males_in_dwelling = ifelse(gender=="male", males_in_dwelling + 1, males_in_dwelling),
           rooms_in_dwelling_crowd = ifelse(rooms_in_dwelling == 0, 1, rooms_in_dwelling),
           crowding_index = people_in_dwelling/rooms_in_dwelling_crowd
           
    ) %>%
    select(-rooms_in_dwelling_crowd)
  
}

taxa_names <- c('rodents', 'nhp',  'bats', 'swine',   'poultry',
                'birds', 'cattle', 'goats_sheep', 'carnivores',
                'camels', 'pangolins', 'ungulates', 'dogs', 'cats')

illness_names <- c("ILI", "SARI", "encephalitis", "hemorrhagic fever")
illness_names_clean <-  make_clean_names(illness_names)

# Create analysis dataframe with logical values for all categorical data
get_logical <- function(dat, exclude_last_yr = TRUE, add_contact = TRUE, gender_logical = TRUE, edu_logical = TRUE, include_symp_other_ppl = FALSE) { # input is output of get_behav
  
  # select and widen covariate data
  covars <- dat %>%
    select(participant_id,
           concurrent_sampling_site,
           age,
           gender,
           length_lived,
           crowding_index,
           children_in_dwelling,
           dwelling_permanent_structure,
           water_treated,
           water_used_by_animals,
           occupation,
           dedicated_location_for_waste,
           matches("education"),
           matches("last_year"),
           scratched_bitten_treated,
           scratched_bitten_action,
           worried_about_disease,
           risk_open_wound,
           travelled,
           travel_reason,
           treatment,
           illness_death,
           -symptoms_in_last_year,
           -symptoms_in_last_year_other_people) %>%
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
                           crowding_index),
              .funs = funs(as.numeric)) %>%
    mutate(children_in_dwelling = ifelse(is.na(children_in_dwelling)|children_in_dwelling==0, FALSE, TRUE)) %>%
    #map yes to TRUE and all other responses to FALSE
    mutate_at(.vars = which(map_lgl(., is.character)==TRUE)[-c(1, 2)], #hack to not apply criteria to participant id
              .funs = funs(
                ifelse(str_detect(., '[Yy]es'), TRUE, FALSE))) %>%
    #expansion of factors to binary categorical outcomes
    ed2_expand_wide(occupation) %>%
    ed2_expand_wide(length_lived) %>%
    ed2_expand_wide(travel_reason) %>%
    ed2_expand_wide(treatment) %>%
   ed2_expand_wide(scratched_bitten_action) %>%
    select(-occupation,
           -length_lived,
           -travel_reason,
           -treatment,
           -scratched_bitten_action,
           -ends_with("n_a"))
  
  if(gender_logical){
    covars <- covars %>%
      ed2_expand_wide(gender) %>%
      select(-gender)
  }
  
  if(edu_logical){
    covars <- covars %>%
      ed2_expand_wide(highest_education) %>%
      ed2_expand_wide(highest_education_mother) %>%
      select(-highest_education,
             -highest_education_mother)
  }
  
  if(exclude_last_yr){
    # remove most "last_year" covariates since more detailed info on these exposures will
    # come from exposures below, but keep "shared_water_last_year" and
    # "animals_in_food_last_year" as is
    last.year.vars <- grep("_last_year", colnames(covars), value = T)
    last.year.vars.to.keep <- c("shared_water_last_year", "animals_in_food_last_year")
    last.year.vars.to.exclude <- last.year.vars[!(last.year.vars %in% last.year.vars.to.keep)]
    
    covars <- select(covars, -last.year.vars.to.exclude)
  }
  
  if(add_contact){
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
  }
  
  # get illness data
  illness <- dat %>% 
    ed2_expand_wide(symptoms_in_last_year) %>%
    select(participant_id,
           one_of( "symptoms_in_last_year_fever_with_muscle_aches_cough_or_sore_throat_ili",
                   "symptoms_in_last_year_fever_with_cough_and_shortness_of_breath_or_difficulty_breathing_sari",
                   "symptoms_in_last_year_fever_with_headache_and_severe_fatigue_or_weakness_encephalitis",
                   "symptoms_in_last_year_fever_with_bleeding_or_bruising_not_related_to_injury_hemorrhagic_fever")) %>%
    rename_at(vars(-participant_id), ~str_extract(., paste(illness_names_clean, collapse="|")))

  
  if(include_symp_other_ppl){
    illness_other <- dat %>% 
      ed2_expand_wide(symptoms_in_last_year_other_people) %>%
      select(participant_id,
             one_of("symptoms_in_last_year_other_people_fever_with_muscle_aches_cough_or_sore_throat_ili"),
             one_of("symptoms_in_last_year_other_people_fever_with_cough_and_shortness_of_breath_or_difficulty_breathing_sari"),
             one_of("symptoms_in_last_year_other_people_fever_with_headache_and_severe_fatigue_or_weakness_encephalitis"),
             one_of("symptoms_in_last_year_other_people_fever_with_bleeding_or_bruising_not_related_to_injury_hemorrhagic_fever")) %>%
      rename_at(vars(-participant_id), ~paste0("other_people_", str_extract(., paste(illness_names_clean, collapse="|"))))
    
    illness <- left_join(illness, illness_other)
  }
  
  covars <- left_join(covars, illness) 
  covars
}

# Subset data for outcomes of interest
get_outcomes <- function(dat, taxa_outcomes, illness_outcomes){
  
  taxa_to_exclude <- taxa_names[!taxa_names %in% taxa_outcomes]
  illness_to_exclude <- illness_names_clean[!illness_names_clean %in% illness_outcomes]
  
  out <- dat 
  
  if(length(taxa_to_exclude)>0){
    out <- out %>%
      select(-matches(!!paste(taxa_to_exclude, collapse = "|")))
  }
  
  if(length(illness_to_exclude)>0){
    out <- out %>%
      select(-matches(!!paste(illness_to_exclude, collapse = "|")))
  }
  out
}

# discretize continuous variables
discretize_continuous <- function(dat, age_breaks, age_labels, crowding_index_breaks, crowding_index_labels){
  
  dat %>%
    mutate(age_discrete = arules::discretize(age, method = "fixed",
                                             breaks = age_breaks,
                                             labels = age_labels),
           crowding_index_discrete = arules::discretize(crowding_index, method = "fixed",
                                                        breaks = crowding_index_breaks,
                                                        labels = crowding_index_labels)) %>%
    select(-age, -crowding_index) %>%
    mutate(age_discrete = paste0("age_discrete_", age_discrete),
           value_age_discrete = TRUE, 
           crowding_index_discrete = paste0("crowding_index_discrete_", crowding_index_discrete),
           value_crowding_index_discrete = TRUE) %>%
    spread(age_discrete, value_age_discrete, fill = FALSE) %>%
    spread(crowding_index_discrete, value_crowding_index_discrete, fill = FALSE) 
  
}

# Create analysis dataframe - reshape taxa and illness outcomes
get_tab <- function(dat) { # input is output of get_behav
  
  tabs <- get_logical(dat, exclude_last_yr = FALSE, add_contact = FALSE, gender_logical = FALSE, edu_logical = FALSE, include_symp_other_ppl = TRUE) %>%
    mutate_if(is.logical, ~ifelse(.x == TRUE, "yes", "no")) %>%
    mutate_at(.vars = c("highest_education", "highest_education_mother"), 
              ~recode(.x, 
                      "secondary school" = "secondary school or higher",
                      "college/university/professional" = "secondary school or higher")) 

  # add contact
  for (i in seq_along(taxa_names)){
    contx_type <- paste0(taxa_names[i], "_contact")
    contact_any <- paste0(taxa_names[i], "_contact_any")
    contact_indirect <- paste0(taxa_names[i], '_contact_indirect')
    contact_direct <- paste0(taxa_names[i], '_contact_direct')
    
    exposures <- dat %>%
      select(participant_id, !!sym(contx_type)) %>% 
      mutate(!!contact_any := ifelse(!!sym(contx_type) == "", "no", "yes"),
             !!contact_indirect := ifelse(grepl("feces|house", !!sym(contx_type)), "yes", "no"),
             !!contact_direct := ifelse(grepl("pet|handled|raised|eaten|found|scratched|hunted|slaughtered", !!sym(contx_type)), "yes", "no")) %>%
      select(-!!sym(contx_type))
    
    tabs <- left_join(tabs, exposures)
  }
  tabs
}

# plot theme for site maps

theme_avenir <- function(
  base_family="Avenir", base_size = 11.5,
  plot_title_family=base_family, plot_title_size = 16,
  plot_title_margin = 10,
  plot_title_face = "plain",
  subtitle_family="Avenir",
  subtitle_size = 13,
  subtitle_face = "plain", subtitle_margin = 15,
  strip_text_family = base_family, strip_text_size = 12,
  strip_text_face = "plain",
  caption_family="Avenir",
  caption_size = 10,
  caption_face = "plain", caption_margin = 10,
  caption = NULL,
  axis_text_size = base_size,
  axis_title_family = base_family,
  axis_title_size = 9,
  axis_title_face = "plain",
  axis_title_just = "rt",
  plot_margin = margin(30, 30, 30, 30),
  grid_col = "#cccccc", grid = TRUE,
  axis_col = "#cccccc", axis = FALSE, ticks = FALSE) {
  
  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)
  
  ret <- ret + theme(legend.background=element_blank())
  ret <- ret + theme(legend.key=element_blank())
  
  if (inherits(grid, "character") | grid == TRUE) {
    
    ret <- ret + theme(panel.grid=element_line(color=grid_col, size=0.2))
    ret <- ret + theme(panel.grid.major=element_line(color=grid_col, size=0.2))
    ret <- ret + theme(panel.grid.minor=element_line(color=grid_col, size=0.15))
    
    if (inherits(grid, "character")) {
      if (regexpr("X", grid, ignore.case = TRUE)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid, ignore.case = TRUE)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
    }
    
  } else {
    ret <- ret + theme(panel.grid=element_blank())
  }
  
  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line=element_line(color=axis_col, size=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }
  
  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  
  ret <- ret + theme(axis.text.x=element_text(size=axis_text_size, margin=margin(t=0)))
  ret <- ret + theme(axis.text.y=element_text(size=axis_text_size, margin=margin(r=0)))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y.right=element_text(hjust=yj, size=axis_title_size, angle=90,
                                                     family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                             face=strip_text_face, family=strip_text_family))
  ret <- ret + theme(panel.spacing=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             family=plot_title_family, face=plot_title_face))
  ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                family=subtitle_family, face=subtitle_face))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               family=caption_family, face=caption_face))
  ret <- ret + theme(plot.margin=plot_margin)
  
  ret
  
}

