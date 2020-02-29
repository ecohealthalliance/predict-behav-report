

purrr::walk(list.files(here::here("R/"), full.names = TRUE),
            source, echo = FALSE, verbose = FALSE)
library("rnaturalearth")
library("rnaturalearthdata")
library(scatterpie)
library(ggrepel)
library(gridExtra)

contact_vars <- c("bats_contact", "rodents_contact", "nhp_contact")

all_maps <- ne_countries(scale = "medium", returnclass = "sf") 

for(country in eha_countries()){
  
  if(country %in% c("Liberia", "South Sudan", "Malaysia, Sabah")){next()}
  
  country_mod <- str_replace(country, ", ", "-") 
  
  # Read in data
  dat <- read_csv(h(paste0("data/raw-behav-", country_mod, ".csv")), col_types = raw_specs) %>% 
    filter(!grepl("Independant|Independent", concurrent_sampling_site))
  
  # Get Site names
  site_names <- read_csv(h(paste0("data/site-name-formatted-", country_mod, ".csv")), col_types = cols())
  
  # Cleaning
  fdat <- dat %>% 
    left_join(site_names, by = c("country" = "country", "concurrent_sampling_site" = "old")) %>%
    group_by(concurrent_sampling_site) %>% 
    mutate(site_latitude = mean(site_latitude), 
           site_longitude = mean(site_longitude),
           site_name = gsub(" ", "-", tolower(concurrent_sampling_site))) %>%  # take average to handle some inconsistencies
    ungroup() %>%
    mutate_at(.vars = contact_vars, ~ifelse(. == "none", "no", "yes")) %>% 
    mutate(concurrent_sampling_site = str_remove(concurrent_sampling_site, "Concurrent ")) 
  
  country2 <-  str_replace_all(country,
                               c("\\-.*" = "", 
                                 "^DR" = "Democratic Republic of the",
                                 "PDR" = "People's Democratic Republic",
                                 "Myanmar" = "Burma",
                                 "Vietnam" = "Viet Nam",
                                 "Tanzania" = "United Republic of Tanzania",
                                 "Ivory Coast" = "Cote d'Ivoire",
                                 "Republic of Congo" = "Congo",
                                 "South Sudan"= "Sudan",
                                 ", Peninsular" = "",
                                 ", Sabah" = ""
                               ))
  iso3c <- countrycode::countrycode(country2, origin = "country.name", destination = "iso3c")
  
  countrymap <- all_maps  %>% 
    filter(iso_a3 == iso3c)
  
  assertthat::assert_that(nrow(countrymap) > 0)
  
  for(cvar in contact_vars){
    
    cdat <- fdat %>% 
      group_by_at(.vars = c("country", "site_latitude", "site_longitude", "full_site_name", "site_name", "concurrent_sampling_site", "n", cvar)) %>% 
      count() %>% 
      ungroup() %>% 
      pivot_wider(names_from = !!cvar, values_from = nn, values_fill = list(nn = 0))
    if(is.null(cdat$yes)) cdat$yes <- 0
    
    max_n <- max(cdat$n)
    
    cdat_sum <- cdat %>%
      arrange(site_name) %>% 
      select(" " = full_site_name, `N total` = n, `N yes` = yes)
    tbl <- tableGrob(cdat_sum, rows=NULL)
    
    bbox <- sf::st_bbox(countrymap)
    
    p <- ggplot(countrymap) + 
      geom_sf() +
      geom_scatterpie(data = cdat,
                      aes(x=site_longitude, y=site_latitude, r =  0.2*sqrt(n)/sqrt(max_n)),
                      cols=c("yes", "no")) +
      scale_fill_manual(breaks=c("yes","no"), values = c("yes" = viridis::viridis_pal()(3)[1], "no" = viridis::viridis_pal()(3)[2])) +
      geom_label_repel(
        data = cdat, aes(x=site_longitude, y=site_latitude, label = concurrent_sampling_site),
        point.padding = 0.5,
        force = 10)+
      labs(fill = str_replace(cvar, "_", " ")) +
      theme_map() +
      theme(legend.position = "top")
    
    g <- arrangeGrob(p, tbl, nrow=2, heights = c(5,1))
    
    ggsave(plot = g,
           filename = h("outputs", "pie-maps", paste0(country, "_", cvar, ".png")), width = 8, height = 8)
    
  }
}

