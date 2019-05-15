#'  make summary tables of response statistics
#'
#' @param dat humans and events datasets combined (e.g., left_join(humans, events, by = "event_name"))
#' @param pretty_name dataframe of old and new place names
#' @param group_var grouping variable
#' @param factor_levels factors within the grouping var
#' @param factor_lab default is same as levels
#' @param table_lab table name - default is group_var
#' @param asterisk whether to include asterisk on values
get_sum_table <- function(dat,
                          pretty_names,
                          group_var,
                          factor_levels=NULL,
                          factor_lab=factor_levels,
                          table_lab = paste(group_var),
                          asterisk=FALSE){
  
  ast <- ifelse(asterisk, "*", "")
  
  n_respondents <- dat %>%
    group_by(concurrent_sampling_site) %>%
    count() %>%
    ungroup() %>%
    rename(n_respondents = n)
  
  dat <- eidith::ed2_expand_long(dat, !!sym(group_var), other_details = TRUE)
  group_var <- paste0(group_var, "_val")
  
  total <- dat %>%
    group_by(!!sym(group_var)) %>%
    summarise(n = n()) %>%
    mutate(Aggregate = paste0(scales::percent(n/sum(n_respondents$n_respondents), accuracy = 1), " (", n, ")", ast)) %>%
    select(-n)
  
  table <- dat %>%
    left_join(n_respondents, by = "concurrent_sampling_site") %>%
    group_by(concurrent_sampling_site, !!sym(group_var), n_respondents) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    mutate(n = paste0(scales::percent(n/n_respondents, accuracy = 1), " (", n, ")", ast)) %>%
    select(-n_respondents) %>%
    spread(key = concurrent_sampling_site, value= n) %>%
    left_join(total) %>%
    mutate(!!group_var := factor(!!sym(group_var), levels=factor_levels, labels = factor_lab)) %>%
    arrange(!!sym(group_var)) %>%
    mutate_if(is.character, funs(replace(., is.na(.), paste0("0% (0)",ast))))
  
  if(length(pretty_names$old[!pretty_names$old %in% names(table)])>0){
    table <- mutate(table, !!pretty_names$old[!pretty_names$old %in% names(table)] := paste0("0% (0)",ast))
  }
  
  table <- table %>%
    rename_at(pretty_names$old, function(x) paste(pretty_names$new[pretty_names$old == x])) %>%
    rename(` ` = !!sym(group_var)) %>%
    select(" ", pretty_names$new)
  
  
  table_h <- table[1,] %>%
    mutate_all(function(x)"") %>%
    mutate(` ` = table_lab)
  
  table <- suppressWarnings(bind_rows(table_h, table))
  table
}

# format summary tables
format_sum_table <- function(tbs, footer = TRUE){
  
  if(footer){
    footer <- tbs %>% slice(1) %>% mutate_all(., ~paste("*Select all that apply question.  Values are percent of total respondents."))
    tbs <- bind_rows(tbs, footer)
  }
  
  regulartable(tbs, cwidth = 1, cheight = 0.25) %>%
    merge_h(i = nrow(tbs)) %>%
    bold(i = which(tbs[,2]=="" ), j = 1) %>%
    bold(bold = TRUE, part = "header") %>%
    align(i = NULL, j = NULL, align = "center", part = "all") %>%
    align(i = NULL, j = 1, align = "left", part = "body") %>%
    autofit()
  
}
