# Function to check for installed packages and install them if they are not installed
install <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new.packages))
    install.packages(new.packages, dependencies = TRUE)
  sapply(packages, require, character.only = TRUE)
}

# Install packages
required.packages <- c("nlme", "binom")
install(required.packages)


# Libraries
library(eidith)
library(arules)
library(tidyverse)
library(here)
library(assertthat)
library(knitr)
library(kableExtra)
library(flextable)
library(ggthemes)
library(janitor)
library(glmnet)
library(binom)
library(maptools)
library(mapview)
library(data.table)
library(formattable)
library(leaflet)
library(leafpop)
library(RColorBrewer)
library(rworldmap)
library(assertthat)
library(scales)
library(stringr)
library(htmltools)
library(DT)
library(brew)

