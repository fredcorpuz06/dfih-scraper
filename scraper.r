library(tidyverse)
library(readxl)
library(rvest)

# Read in data
locations <- read_excel("data/locations_tunisia.xlsx")
juridicial <- read_excel("data/juridical_tunisian.xlsx")

##-----------------------
## Functions for scraping
##-----------------------
URL <- locations$URL[20]
URL

get_yearbook_list <- function(.url){
  # "https://dfih.fr/issuers/3583"
  persons_url <- paste0(.url, "/persons")
  read_html(persons_url) %>% 
    html_node(".form-control") %>% 
    html_nodes("option") %>% 
    html_attr("value")
}

years <- get_yearbook_list(URL)
years

get_persons <- function(.url, .yearbook){
  # "https://dfih.fr/issuers/3583"
  # "annuaire_df_1927"
  persons_url <- paste0(.url, "/persons", "?source=", .yearbook)
  read_html(persons_url) %>% 
    html_node("table") %>% 
    html_table() %>% 
    mutate(Year = .yearbook)
  
}

get_persons(URL, years[1])

sum_get_persons <- function(.url){
  # "https://dfih.fr/issuers/3583"
  years <- get_yearbook_list(.url)
  map_dfr(years, get_persons, .url = .url)
 
}

sum_get_persons(URL)

scraper <- function(.df){
  .df %>% 
    group_by(NAME) %>% 
    nest() %>% 
    mutate(person_tables = map(data, function(data){
      sum_get_persons(data$URL[1])
    })) %>% 
    unnest(cols = c(data, person_tables)) %>% 
    ungroup %>% 
    mutate(Year = str_extract(Year, "[0-9]+$"))
}


##---------------------------
## Scrape website for 2 lists
##---------------------------
locations_persons <- scraper(locations)
juridicial_persons <- scraper(juridicial)

write_excel_csv(locations_persons, "output/locations_persons.csv")
write_excel_csv(juridicial_persons, "output/juridicial_persons.csv")




