h <- here::here

header_true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df <- df %>% 
    rownames_to_column() %>%
    slice(-1)
  return(df)
}

simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}