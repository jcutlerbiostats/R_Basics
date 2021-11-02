

# Importing many files to be stacked vertically together.

# Libraries ----
pacman::p_load(
  tidyverse
)


# How to import up to thousands of files in one small chunk of code. ----
# covid_deaths_tbl <- read_csv(
#   "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
# )

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



# . ----
# BRFSS ----
# Variables
# STATE
# EDUCA 
# BMI
# DIABETE
# SEX

## Import ----
# brfss_tbl <- data.table::fread("/Users/jamescutler/Desktop/misc_data/BRFSS_2018_Entire_DT.csv") %>% as_tibble()

## Filter to OK & CO ----
brfss_tbl %>% #distinct(X_STATE) %>% pull(X_STATE)
  select(X_STATE,EDUCA,X_BMI5,DIABETE3,SEX1) %>% 
  left_join(
    state.name %>% as_tibble() %>% bind_rows(tibble(value="District of Columbia")) %>% arrange(value) %>% 
      mutate(X_STATE = c(brfss_tbl %>% distinct(X_STATE) %>% pull(X_STATE))[1:51]),
    by = "X_STATE"
  ) %>% 
  janitor::clean_names() %>% 
  select(-x_state) %>% 
  rename(state = value) %>% 
  filter(state %in% c("Oklahoma","Colorado")) %>% 
  mutate(x_bmi5 = x_bmi5/100) %>% 
  mutate(
    diabete3 = case_when(diabete3 %in% c(1,2) ~ "Yes",
                         diabete3 %in% c(3,4) ~ "No",
                         TRUE ~ "Unknown"),
    sex1     = case_when(sex1 == 1 ~ "Male",
                         sex1 == 2 ~ "Female",
                         TRUE ~ "Unknown"),
    educa    = case_when(educa == 1 ~ "No School",
                         educa == 2 ~ "Elementary",
                         educa == 3 ~ "Some High School",
                         educa == 4 ~ "High School Grad",
                         educa == 5 ~ "Some College",
                         educa == 6 ~ "College Grad",
                         TRUE ~ "Unknown")
  ) -> ok_co_tbl

## More data prep ----
ok_co_tbl %>% 
  mutate(
    educa    = factor(educa,c("No School","Elementary","Some High School","High School Grad","Some College","College Grad","Unknown")),
    diabete3 = factor(diabete3,c("Yes","No","Unknown")),
    sex1     = factor(sex1,c("Male","Female","Unknown"))
  ) -> ok_co_tbl

ok_co_tbl$educa

## Variable labels ----
library(sjlabelled)
ok_co_tbl %>% 
  var_labels(
    educa    = "Education",
    x_bmi5   = "BMI",
    diabete3 = "Diabetes",
    sex1     = "Gender",
    state    = "State"
  ) -> ok_co_tbl

## Make cross-table ----
library(gtsummary)
ok_co_tbl %>% 
  tbl_summary(
    by = "state",
    percent = "column",
    missing_text = "(Missing)"
  ) %>% 
  add_p() %>% 
  bold_labels() %>% 
  add_overall() %>% 
  modify_spanning_header(c("stat_1","stat_2") ~ "**Home State of Respondent**") %>% 
  modify_caption(caption = md("**A cross-table of the demographic/health variables stratified by state**"))

## SAVE ok_co_tbl ----
# (.rds will preserve the variable labels and factor orders)
# ok_co_tbl %>% write_rds("00_data/OK_CO_BRFSS.rds")





