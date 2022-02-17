

# maps_and_web_scraping ----

# This script shows you how to make both a static as well as an interactive simple map in
# R, and also how to web scrape the information you need in the first place to plot on 
# the map.

# Libraries ----
pacman::p_load(
  tidyverse,
  tidyquant,
  scales,
  leaflet,
  rvest,
  tmap,
  googleway
)


# Custom functions ----
source("00_custom_functions.R")



# 0.0 - Pretend you don't have this ----
school_coords_tbl <- read_rds("00_data/OK_county_high_schools_coords.rds")



# 1.0 - Scrape school data ----
"https://en.wikipedia.org/wiki/List_of_high_schools_in_Oklahoma" %>% 
  my_scrape(61:64,"ul") %>% 
  filter(! (str_detect(value,"^\\,") | str_detect(value,"^\n")) ) -> school_1_tbl

"https://en.wikipedia.org/wiki/List_of_high_schools_in_Oklahoma" %>% 
  my_scrape(394:417,"li") %>% 
  filter(!(str_detect(value,"^\\,"))) -> school_2_tbl

school_1_tbl
school_2_tbl

# Combine the two scraped tibbles into new tibble, then delete them
school_tbl <- bind_rows(school_1_tbl,school_2_tbl)
rm(school_1_tbl,school_2_tbl)

# Add 'Oklahoma' to school names to help Google find them
school_tbl %>% 
  set_names("school") %>% 
  mutate(school = str_c(school,", Oklahoma",sep="")) -> school_tbl

school_tbl



# 2.0 - Geocode schools ----
## WARNING ----
# WARNING: THIS WILL NOT WORK UNLESS YOU HAVE A GOOGLE API KEY!
school_tbl %>% 
  pull(school) %>% 
  map(.f = function(X){
    res <- google_geocode(X,key = "YOUR KEY GOES HERE")
    df <- res$results
    location <- df$geometry$location
    my_tbl <- bind_cols(
      # Col. 1, Col. 2
      location, str_glue("{X}")
    )
    return(my_tbl)
  }) -> school_coords_tbl

school_coords_tbl

## Clean up ----
school_coords_tbl %>% bind_rows() %>% as_tibble() %>% 
  select(lat:...3) %>%       # Select only the columns we want
  rename(school = ...3) %>%  # Rename the column name from '...3' to 'school'
  mutate(                    # Now that we don't need the 'Oklahoma', get rid of it
    school = str_remove(school,"\\, Oklahoma") 
  ) -> school_coords_tbl

# More cleanup - Remove duplicates (e.g. Bethany High)
# Some good ole base R code! (still useful)
school_coords_tbl %>% 
  .[-which(duplicated(school_coords_tbl$school)),] -> school_coords_tbl



# 3.0 - Map stuff ----
## html labels for map ----
school_coords_tbl$labels <- pmap(
  list(school_coords_tbl$school),
  function(first){
    htmltools::HTML(str_glue("{first}"))
  }
)

## Leaflet map! ----
school_coords_tbl %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Voyager") %>% 
  setView(lng = -97.5,lat = 35.5, zoom = 10) %>% 
  addCircleMarkers(
    label = ~labels,
    lat = ~lat,
    lng = ~lng,
    radius = 7
  )

## Jitter some points ----
# (Some coordinate pairs are literally identical to each other.)
school_coords_tbl %>% my_jitter_coords(sd=.001) -> school_jitter_tbl

### Map again ----
school_jitter_tbl %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Voyager") %>% 
  setView(lng = -97.5,lat = 35.5, zoom = 10) %>% 
  addCircleMarkers(
    label = ~labels,
    lat = ~lat,
    lng = ~lng,
    radius = 7
  )



# 4.0 - SAVE your geocoded data! ----
# (It makes it so you don't have to geocode again!)
# Obviously, this step has already been done (the data is already in 00_data/).
# school_coords_tbl %>% write_rds("00_data/OK_county_high_schools_coords.rds")



# . ----
# ROUND 2 ----
# Scrape Enhance Dental Locations ----
pacman::p_load(
  tidyverse,
  rvest,
  googleway,
  leaflet
)

# You can load this already-scraped data and skip until the end
# where the map is generated until you get a google maps key.
# SAVED enhance_coords_tbl ----
# enhance_coords_tbl %>% write_rds("00_data/Enhance_Dental_locations.rds")
enhance_coords_tbl <- read_rds("00_data/Enhance_Dental_locations.rds")

"https://www.enhancedds.com/" %>% 
  read_html() %>%
  html_nodes("body") %>% 
  html_text() %>% 
  as.character() %>% 
  str_remove_all("\\\t|\\\n") %>% 
  str_extract("(?<=Enhance Dental Practices).*") %>% 
  str_split("OK|UT") %>% 
  unlist() %>%
  as_tibble() %>% 
  mutate(value = str_squish(value)) -> enhance_tbl

enhance_tbl %>% slice(1:30) -> enhance_tbl


## Prep (with fake clairvoyance) ----
enhance_tbl %>% 
  mutate(
    value = str_replace_all(value,"([a-z])([A-Z])","\\1 \\2"),
    value = str_remove(value,"\\,$")
  ) %>% 
  mutate(value = case_when(
    str_detect(value,"Sand") ~ "Sand Springs OK",
    str_detect(value,"Quail Springs") ~ "Quail Springs OKC",
    TRUE ~ value
  )) -> enhance_tbl #%>% filter(str_detect(value,"Sand"))

enhance_tbl %>% filter(str_detect(value,"Quail|Sand"))


## Geocode ----
enhance_tbl %>% pull(value) %>% 
  map(.f = function(X){
    res <- google_geocode(X,key = "YOUR KEY GOES HERE")
    df <- res$results
    location <- df$geometry$location
    my_tbl <- bind_cols(location,str_glue("{X}"))
    return(my_tbl)
  }) %>% bind_rows() %>% as_tibble() -> enhance_coords_tbl

enhance_coords_tbl

### It missed some ... ----
enhance_coords_tbl %>% 
  rename(missed = ...1,practice = ...3) %>% 
  mutate(practice = case_when(
    !is.na(missed) ~ missed,
    TRUE ~ practice
  )) %>% 
  select(-missed) -> enhance_coords_tbl

enhance_coords_tbl

enhance_coords_tbl %>% filter(is.na(lat))

### ... so geocode those too ----
enhance_coords_tbl %>% 
  filter(is.na(lat)) %>% 
  filter(!str_detect(practice,"Centers")) %>% 
  
  mutate(
    practice = str_extract(practice,"\\w+$"),
    practice = str_c(practice,", OK")
  ) %>% 
  pull(practice) %>% 
  
  map(.f = function(X){
    res <- google_geocode(X,key = "YOUR KEY GOES HERE")
    df <- res$results
    location <- df$geometry$location
    my_tbl <- bind_cols(location,str_glue("{X}"))
    return(my_tbl)
  }) %>% bind_rows() %>% as_tibble() -> more_coords_tbl

more_coords_tbl %>% rename(practice = ...3) -> more_coords_tbl

enhance_coords_tbl %>% bind_rows(more_coords_tbl) -> enhance_coords_tbl

enhance_coords_tbl


## html labels ----
enhance_coords_tbl$labels <- pmap(
  list(enhance_coords_tbl$practice),
  function(first){
    htmltools::HTML(str_glue("{first}"))
  }
)


## Map ----
enhance_coords_tbl %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Voyager") %>% 
  setView(lng = -97.5,lat = 35.5, zoom = 8) %>% 
  addCircleMarkers(
    label = ~labels,
    lat = ~lat,
    lng = ~lng,
    radius = 7
  )



