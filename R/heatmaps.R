#' Main heatmap function
#' 
#' @param dat contaxa dataframe with T/F for taxa contact (get_formatted_dat())
#' @param taxa_types string of taxa name for stratification
#' @param contx_types string of contact name for stratification
#' @param title plot title
#' @param group_var name of column for facet wrapping (e.g., "gender").  Optional.
#' @return ggplot2 heatmap object
#' @export
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
  
  p <- ggplot(data = hdat, aes(x = taxa, y = contx, fill = perc, label = lab)) +
    geom_tile(color = 'gray') +
    geom_label(fontface='bold', fill="white") +
    scale_x_discrete(position = 'bottom') +
    scale_fill_viridis_c(guide = FALSE) +
    theme_fivethirtyeight() +
    theme(plot.title = element_text(hjust=0.5),
          strip.text = element_text(size=10),
          axis.text.y = element_text(size=10),
          axis.text.x = element_text(size=10)) +
    labs(title = title)
  
  if(group_var != ""){
    p <- p + facet_wrap(as.formula(paste("~", group_var)))
  }
  p
}
