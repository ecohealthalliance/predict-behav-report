popup_image_mod <- function(x, width = NULL, height = NULL) {
  
  str_replace(leafpop::popupImage(x, width=width, height=height, src = c("local")),
              "<image src='.+'",
              paste0("<image src='", knitr::image_uri(x), "'"))
}
