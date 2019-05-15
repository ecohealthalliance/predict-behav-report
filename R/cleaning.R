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
                                                                       "other, other" = "other"
           )),
           treatment_specific = str_replace_all(tolower(treatment), c("clinic/health center" = "clinic", 
                                                                      "hospital" = "clinic", 
                                                                      "community health worker" = "clinic",
                                                                      "mobile clinic" = "clinic",
                                                                      "dispensary or pharmacy" = "dispensary or pharmacy only",
                                                                      "traditional healer" = "traditional healer only")),
           treatment_specific = map_chr(str_split(treatment_specific, "; "), function(x){
             unique(x) %>%
               ifelse(length(.)==1, ., "multiple sources") %>%
               recode(., "clinic" = "clinic, hospital, or community health worker only")
           }),
           scratched_bitten_action_specific = scratched_bitten_action, 
           scratched_bitten_action = ifelse(str_detect(scratched_bitten_action, "visit|soap"),
                                            "treated", 
                                            ifelse(str_detect(scratched_bitten_action, "someone|bandage|rinse|nothing"),
                                                   "untreated",
                                                   "N/A")),
           risk_open_wound_specific = str_replace_all(risk_open_wound, c("yes, " = "",
                                                                         "but" = "there are risks, but",
                                                                         "^no$" = "N/A",
                                                                         "don't  know" = "N/A"
                                                                         )),
             risk_open_wound_specific = ifelse(str_detect(risk_open_wound_specific, "other"), "other", risk_open_wound_specific),
           risk_open_wound = ifelse(str_detect(risk_open_wound, "yes"), "yes",
                                     ifelse(str_detect(risk_open_wound, "other"), "other",
                                    risk_open_wound))
    )
  
}

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
