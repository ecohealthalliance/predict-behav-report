library(eidith)
library(tidyverse)
# import_local_db(database = "global")

human <-  eidith::ed2_human()
event <- eidith::ed2_events() %>% 
  filter(project == "P2") %>%
  select(event_name, concurrent_sampling_site, country,
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
  filter(country == "Thailand")

# recode 
out <- out %>%
  mutate_if(suppressWarnings(str_detect(., ";")), ~map_chr(str_split(., "; "), function(x){
    if(all(is.na(x))){return(NA)}
    paste(unique(x), collapse = "; ")
  }))  %>% # removes dup responses (warning is empty quotes)
  mutate_all(~(str_replace(., "MISSING", "missing"))) %>% # keep all "missing" lowercase
  mutate_all(~(str_replace(., "NA", "N/A"))) %>% # replace all entered NAs (ie not missing or empty)
  mutate_all(~(str_replace_all(., "\\s+", " "))) %>% # replace double spaces with single spaces
  mutate_at(.vars = vars(rooms_in_dwelling, people_in_dwelling, children_in_dwelling, males_in_dwelling,
                         site_latitude, site_longitude, age), .funs = ~suppressWarnings(as.numeric(.))) %>% # make numeric values (warning is NAs introduced -- ok)
  mutate(age = floor(age), # round down age
         # get whether or not exposed to taxa
         rodents = ifelse(rodents_contact != "none", "rodents", NA), 
         bats = ifelse(bats_contact != "none", "bats", NA),
         nhp = ifelse(nhp_contact != "none", "primates", NA),
         swine = ifelse(swine_contact != "none", "swine", NA),
         poultry = ifelse(poultry_contact != "none", "poultry", NA),
         birds = ifelse(birds_contact != "none", "birds", NA),
         goats_sheep = ifelse(goats_sheep_contact != "none", "goats/sheep", NA),
         cats = ifelse(cats_contact != "none", "cats", NA),
         dogs = ifelse(dogs_contact != "none", "dogs", NA),
         camels = ifelse(camels_contact != "none", "camels", NA),
         carnivores = ifelse(carnivores_contact != "none", "carnivores", NA),
         pangolins = ifelse(pangolins_contact != "none", "pangolins", NA),
         ungulates = ifelse(ungulates_contact != "none", "ungulates", NA),
         cattle = ifelse(cattle_contact != "none", "cattle", NA)) %>%
  unite(contact_all, bats, birds, camels, carnivores, cats, cattle, dogs, goats_sheep,
        nhp, pangolins, poultry, rodents, swine, ungulates, 
        remove=TRUE, sep = "; ") %>%
  mutate(contact_all = gsub('; NA|NA; ', '', contact_all)) %>%
  # misc recode
  mutate(drinking_water_shared = dplyr::recode(drinking_water_shared, "don't know" = "unknown"),
         bathing_water_shared = dplyr::recode(bathing_water_shared, "don't know" = "unknown"),
         had_symptoms_in_last_year = dplyr::recode(had_symptoms_in_last_year, "N/A" = "no"),
         # classify insect vectors
         insect_vectors = str_replace_all(insect_vectors, c("sand fly" = "sand", 
                                                            "tsetse fly" = "tsetse", 
                                                            "housefly" = "other",
                                                            "house fly" = "other",
                                                            "fly" = "other",
                                                            "flies" = "other",
                                                            "cockroach" = "other",
                                                            "other, other" = "other")),
         # classify treatment types (clinic, hospital, or community health worker only, dispensary or pharmacy only, traditional healer only, multiple)
         treatment_specific = str_replace_all(treatment, c("clinic/health center" = "clinic", 
                                                           "hospital" = "clinic", 
                                                           "community health worker" = "clinic",
                                                           "mobile clinic" = "clinic",
                                                           "dispensary or pharmacy" = "dispensary or pharmacy only",
                                                           "traditional healer" = "traditional healer only")),
         treatment_specific = map_chr(str_split(treatment_specific, "; "), function(x){
           unique(x) %>%
             ifelse(length(.)==1, ., "multiple sources") %>%
             dplyr::recode(., "clinic" = "clinic, hospital, or community health worker only")}),
         # classify scratched/bitten action (treated/untreated/N/A)
         scratched_bitten_action_specific = scratched_bitten_action, 
         scratched_bitten_action = ifelse(str_detect(scratched_bitten_action, "visit|soap"),
                                          "treated", 
                                          ifelse(str_detect(scratched_bitten_action, "someone|bandage|rinse|nothing"),
                                                 "untreated", 
                                                 ifelse(str_detect(scratched_bitten_action, "missing"),
                                                        "missing", "N/A"))),
         # classify risk from open wound
         risk_open_wound_specific = str_replace_all(risk_open_wound, c("yes, " = "",
                                                                       "but" = "there are risks, but",
                                                                       "^no$" = "N/A",
                                                                       "^don't know$" = "N/A")),
         risk_open_wound_specific = ifelse(str_detect(risk_open_wound_specific, "other"), "other", risk_open_wound_specific),
         risk_open_wound = ifelse(str_detect(risk_open_wound, "yes"), "yes",
                                  ifelse(str_detect(risk_open_wound, "other"), "other",
                                         risk_open_wound)),
         # make homemaker a primary livelihood
         primary_livelihood = ifelse(str_detect(primary_livelihood,"house|home"), "homemaker",
                                     ifelse(str_detect(primary_livelihood, "[Oo]ther"), "other",
                                            primary_livelihood)),
         # get occupant counts
         people_in_dwelling = people_in_dwelling + 1,
         males_in_dwelling = ifelse(gender=="male", males_in_dwelling + 1, males_in_dwelling),
         rooms_in_dwelling_crowd = ifelse(rooms_in_dwelling == 0, 1, rooms_in_dwelling),
         crowding_index = people_in_dwelling/rooms_in_dwelling_crowd
         
  ) %>%
  select(-rooms_in_dwelling_crowd)

write_csv(out, here::here(paste0("data/raw-behav-Thailand.csv"))) # used in summary report and site maps

# Breaks for discretizing continuous variables

age_breaks <- c(0, 18, 41, 61, Inf)
age_labels <- c("under_18", "18_to_40", "41_to_60", "over_60")
crowding_index_breaks <- c(0, 1, 3, 10000, Inf)
crowding_index_labels <- c("less_than_1", "1_to_3", "3_plus", "no_rooms")


ldat <- out %>%
  get_logical(.) %>% # Get boolean values for non-numeric responses
  discretize_continuous(., age_breaks, age_labels, crowding_index_breaks, crowding_index_labels)  # Discretize numeric responses (this can be modified above)
write_csv(ldat, h(paste0("data/logic-behav-Thailand.csv"))) # used in heat maps and lasso

