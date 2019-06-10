popupImage3 <- function(x) {
  
  str_replace(leafpop::popupImage(x),
              "<image src='.+'",
              paste0("<image src='", knitr::image_uri(x), "'"))
}

# animal contact figures
popupImage4 <- function(x, width = 300, height = 115) {
  
  stringr::str_replace(leafpop::popupImage(x, width = width, height = height),
                       "<image src='.+'",
                       paste0("<image src='", knitr::image_uri(x), "'"))
}
