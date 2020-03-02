library(tidyverse)
library(eidith)
library(rnaturalearth)
library(rnaturalearthdata)
library(scatterpie)
library(ggrepel)
library(gridExtra)
library(raster)
library(ggnewscale)
library(ggthemes)
library(viridis)

h <- here::here

# Taxa contacts
contact_vars <- c("bats_contact", "rodents_contact", "nhp_contact")

# Get full world map
world_map <- ne_countries(scale = "large", returnclass = "sf") 

# PREDICT event data - get all site coordinates
events <- ed2_events() %>% as_tibble()
all_points <- SpatialPointsDataFrame(coords = data_frame(as.numeric(events$event_longitude), as.numeric(events$event_latitude)), data = events)

# Read in HP3 data
hp4_bats <- read_rds(h("data/hp4/pred_bats.rds"))
hp4_nhp <- read_rds(h("data/hp4/pred_primates.rds"))
hp4_rodents <- read_rds(h("data/hp4/pred_rodents.rds"))
hp4_summed <-  read_rds(h("data/hp4/pred_raster.rds"))

# Function to mask raster to country
mask_to_country <- function(r, plotting_mask){
  r <- crop(r, extent(plotting_mask))
  mask(r, plotting_mask) %>%
    as.data.frame(xy = TRUE) %>%
    na.omit()
}

# Get global hp3 values
extracted_predict <- raster::extract(hp4_summed, all_points, sp = TRUE)
allpts_summary <- extracted_predict@data %>%
  group_by(country, concurrent_sampling_site) %>%
  summarize(output = mean(layer, na.rm = T))

# Iterate through each country
for(country in eha_countries()){
  
  if(country %in% c("Liberia", "South Sudan", "Malaysia, Sabah")){next()}
  
  country_mod <- str_replace(country, ", ", "-") 
  
  # Read in PREDICT survey data for this country
  dat <- read_csv(h(paste0("data/raw-behav-", country_mod, ".csv"))) %>% 
    filter(!grepl("Independant|Independent", concurrent_sampling_site))
  
  # Get site names
  site_names <- read_csv(h(paste0("data/site-name-formatted-", country_mod, ".csv")), col_types = cols())
  
  # Cleaning PREDICT data
  dat <- dat %>% 
    left_join(site_names, by = c("country" = "country", "concurrent_sampling_site" = "old")) %>%
    group_by(concurrent_sampling_site) %>% 
    mutate(site_latitude = mean(site_latitude), 
           site_longitude = mean(site_longitude),
           site_name = gsub(" ", "-", tolower(concurrent_sampling_site))) %>%  # take average to handle some inconsistencies
    ungroup() %>%
    mutate_at(.vars = contact_vars, ~ifelse(. == "none", "no", "yes")) %>% 
    mutate(concurrent_sampling_site = str_remove(concurrent_sampling_site, "Concurrent ")) 
  
  # Summarize PREDICT data
  dat_sum <- map(contact_vars, function(cvar){
    dat %>% 
      group_by_at(.vars = c("country", "site_latitude", "site_longitude", "full_site_name", "site_name", "concurrent_sampling_site", "n", cvar)) %>% 
      count() %>% 
      ungroup() %>% 
      pivot_wider(names_from = !!cvar, values_from = nn, values_fill = list(nn = 0)) %>% 
      rename(!!paste0(cvar, "_yes") := yes, !!paste0(cvar, "_no") := no)
  }) %>% reduce(left_join)
  
  # Get country iso3c
  country_cc <-  str_replace_all(country,
                                 c("\\-.*" = "", 
                                   "Ivory Coast" = "Cote d'Ivoire",
                                   "Republic of Congo" = "Congo",
                                   "South Sudan"= "Sudan",
                                   ", Peninsular" = "",
                                   ", Sabah" = ""
                                 ))
  iso3c <- countrycode::countrycode(country_cc, origin = "country.name", destination = "iso3c")
  
  # Filter world map for country
  country_map <- world_map %>% 
    filter(iso_a3 == iso3c)
  assertthat::assert_that(nrow(country_map) > 0)
  
  # Get HP3 summed for country (to get legend breaks and labels)
  masked_hp4 <- mask_to_country(hp4_summed, country_map)
  global_breaks <- c(0, max(masked_hp4[,3], na.rm = T))
  top_label <- round(max(masked_hp4[,3], na.rm = T) / max(allpts_summary$output, na.rm = T) * 100) 
  bottom_label <- 0
  p_label <- "% of PREDICT Maximum"
  top_label <- paste0(top_label, p_label)
  global_labels <- c("0", top_label)
  
  # Generate plots
  plots <- map(contact_vars, function(cvar){
    
    plot_title <- switch(cvar, "bats_contact" = "Bats", "rodents_contact" = "Rodents", "nhp_contact" = "Primates")
    
    # Contact data
    contact_dat <- dat_sum %>% 
      dplyr::select(concurrent_sampling_site, site_latitude, site_longitude, n, starts_with(cvar)) %>% 
      rename(yes := paste0(cvar, "_yes"),
             no := paste0(cvar, "_no"))
    
    if(!"yes" %in% names(contact_dat)){
      contact_dat <- contact_dat %>% 
        mutate(yes = 0)
    }
    
    # Max N for scaling
    max_n <- max(contact_dat$n)
    
    # Get base layer
    masked_contact <- str_remove(cvar, "_contact") %>% 
      paste0("hp4_", .) %>% 
      get(.) %>% 
      mask_to_country(., country_map)
    
    # Plot
    p <- ggplot(country_map) + 
      geom_sf(color = "black") +
      geom_raster(data = masked_contact, aes(x = x, y = y, fill = layer)) +
      # scale_fill_gradient2(low = "white", mid = "gray50",high = "black", 
      scale_fill_viridis(option = "plasma", breaks = global_breaks, limits = global_breaks) +
      new_scale("fill") +
      geom_scatterpie(data = contact_dat, color = "gray20",
                      aes(x = site_longitude, y = site_latitude, r =  0.2*sqrt(n)/sqrt(max_n)),
                      cols=c("yes", "no")) +
      scale_fill_manual(breaks=c("yes", "no"), values = c("yes" = "darkseagreen1", "no" = "lightblue")) +
      geom_label_repel(
        data = contact_dat, aes(x=site_longitude, y=site_latitude, label = concurrent_sampling_site),
        point.padding = 0.5,
        force = 10)+
      labs(fill = str_replace(cvar, "_", " "), title = plot_title) +
      theme_map() +
      theme(legend.position = "none")
    
    p <- cowplot::plot_to_gtable(p)
    
  }) %>% set_names(contact_vars)
  
  # Summary table for plots
  tbl <- dat_sum %>% 
    dplyr::select(" " = full_site_name, `N respondents` = n, ends_with("yes")) %>% 
    rename_all(~str_remove(., "_contact_yes")) %>% 
    rename(Bats = bats, Rodents = rodents, Primates = nhp)
  
  # Generate legend for all plots
  tiles <- data.frame(x = rep(0.5, 100),
                      y = seq(global_breaks[1] - global_breaks[2]*.01, global_breaks[2]+global_breaks[2]*0.01, length.out = 100)) %>% 
    filter(y >= 0, y <= global_breaks[2])
  
  custom_legend <- ggplot() +
    geom_raster(data = tiles, aes(x = x, y = y, fill = y), interpolate = TRUE) +
    scale_fill_viridis(option = "plasma", guide = guide_colorbar(title = paste0("HP3+ Predicted Zoonoses", "\n")), breaks = global_breaks,labels = global_labels, limits = global_breaks) +
  new_scale("fill") +
    geom_scatterpie(data = tibble(yes = 50, no = 50, x = 1, y = 1), color = "gray20",
                    aes(x = x, y = y),
                    cols=c("yes", "no")) +
    scale_fill_manual(breaks=c("yes", "no"), values = c("yes" = "darkseagreen1", "no" = "lightblue")) +
    labs(fill = "Taxa Contact")
    
  legend <- cowplot::get_legend(custom_legend)
  
  # Grobs
  tblg <- tableGrob(tbl, rows = NULL)
  tblg$widths <- unit(c(0.4, 0.2, rep(0.15, 3)), "npc")
  
  layout <-rbind(c(1,2, 5),
                 c(3,4, 4))
  
  g <- arrangeGrob(plots[[1]], plots[[2]], plots[[3]], tblg, legend, nrow = 2, ncol = 3, layout_matrix = layout)
  ggsave(plot = g,
         filename = h("outputs", "pie-maps", paste0(country, "_hp3_pie_charts.png")), width = 8, height = 8)
}

