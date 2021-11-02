

# Importing many files to be stacked vertically together.

# Libraries ----
pacman::p_load(
  tidyverse
)


# How to import up to thousands of files in one small chunk of code. ----
covid_deaths_tbl <- read_csv(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
)

# DONE: Write 10 csv files! ----
# covid_deaths_tbl %>%
#   select(-c(1:4,8:11)) %>%
#   filter(Province_State == "Oklahoma") %>%
# 
#   pivot_longer(cols = `1/22/20`:last_col()) %>%
# 
#   filter(
#     Admin2 %in% c(
#       covid_deaths_tbl %>%
#         filter(Province_State == "Oklahoma") %>%
#         distinct(Admin2) %>% slice(1:10) %>% pull(Admin2)
#     )
#   ) %>%
# 
#   group_split(Admin2) %>%
#   map(
#     .f = function(data){
#       write_csv(data,str_c("00_data/covid_",unique(data$Admin2),".csv",sep=""))
#     }
#   )

## Import using fs::dir_info() ----
fs::dir_info("00_data/") %>% 
  select(path) %>% 
  filter(str_detect(path,"covid_")) %>% 
  mutate(
    data = path %>% map(read_csv)
  ) -> covid_data

## unnest() ----
covid_data %>% unnest(data) %>% select(-path) -> covid_tbl

## Take a look ----
covid_tbl

## Convert to date ----
covid_tbl %>% 
  mutate(name = mdy(name)) %>% 
  rename(date = name,deaths = value) %>% 
  janitor::clean_names() -> covid_tbl

