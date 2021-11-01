

# BLS data practice ----

# Get data wrangling practice on the BLS data.

# Libraries ----
pacman::p_load(
  tidyverse,
  tidyquant,
  scales,
  plotly
)


# Import data ----
oes_tbl <- readxl::read_excel("00_data/state_M2020_dl.xlsx",sheet = 1) %>% 
  janitor::clean_names()

oes_dict <- readxl::read_excel("00_data/state_M2020_dl.xlsx",sheet = 2,skip = 8) %>%
  janitor::clean_names() %>% select(1:2) %>% slice(1:31)


# 1) Clean data ----
oes_tbl %>% select(matches("h_"))

# Narrow it down to just the columns we want
oes_tbl %>% 
  mutate(across(matches("h_"),as.numeric)) %>% 
  select(area_title,prim_state,occ_title,matches("h_")) %>% 
  rename(state_name = area_title) -> oes_2_tbl

oes_2_tbl

## Plot which states we have ----
# Plot states to see if we're missing any - Looks like we're not!
usmap::plot_usmap(include = oes_2_tbl %>% 
                    distinct(prim_state) %>% 
                    slice(1:51) %>% 
                    pull(prim_state),
                  fill = "gray70") 

## Get list of occupations ----
oes_2_tbl %>% distinct(occ_title) %>% pull(occ_title) -> occupations

matrix(
  NA,
  nrow = 1,
  ncol = length(occupations)
) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  set_names(occupations) -> occupations_tbl

# Easy to look up occupations this way
occupations_tbl$`Dentists, General`
occupations_tbl$`Physicians, All Other; and Ophthalmologists, Except Pediatric`
occupations_tbl$`Physician Assistants`
occupations_tbl$`Dental Hygienists`
occupations_tbl$`Nurse Practitioners`
occupations_tbl$`Management Occupations`
occupations_tbl$`Sales Managers`
occupations_tbl$`Chief Executives`
occupations_tbl$Legislators
occupations_tbl$Epidemiologists
occupations_tbl$Statisticians
occupations_tbl$`Software Developers and Software Quality Assurance Analysts and Testers`

selected_occupations <- c(
  "Dentists, General",
  "Physicians, All Other; and Ophthalmologists, Except Pediatric",
  "Physician Assistants",
  "Dental Hygienists",
  "Nurse Practitioners",
  "Management Occupations",
  "Sales Managers",
  "Chief Executives",
  "Legislators",
  "Epidemiologists",
  "Statisticians",
  "Software Developers and Software Quality Assurance Analysts and Testers"
)



# 2) Plot wages ----
oes_2_tbl %>% 
  filter(prim_state %in% c("OK","NY","TX","CA","UT","MA")) %>% 
  filter(occ_title %in% selected_occupations) %>% 
  filter(!(str_detect(occ_title,"Legisl"))) %>% 
  
  # Pivot longer
  pivot_longer(cols = h_mean:h_pct90) %>% 
  
  # Trim occupation title
  mutate(occ_title = str_sub(occ_title,end = 15)) %>% 
  
  # Plot
  ggplot(aes(name,value,color=occ_title,group=occ_title)) + 
  facet_wrap(~state_name) + 
  geom_point(size = 3) + 
  geom_line(size = 1.3) + 
  theme_tq() + scale_color_tq() + 
  labs(title = "Hourly Wages by Occupation by State",
       x = "Percentiles",y = "Dollars per Hour") -> g_wages

ggplotly(g_wages)
  
  

# 3) One occ., all states ----
# Let's look at just one occupation - dentists - across all 50 states.
dentist_median <- oes_2_tbl %>% filter(occ_title == "Dentists, General") %>% pull(h_mean) %>% median(na.rm=T)

oes_2_tbl %>% 
  filter(occ_title == "Dentists, General") %>% 
  mutate(prim_state = factor(prim_state) %>% fct_reorder(h_mean)) %>% 
  
  ggplot(aes(h_mean,prim_state)) + 
  
  geom_segment(
    aes(x = 0,xend = h_mean,y = prim_state,yend = prim_state),
    color = "skyblue"
  ) + 
  geom_point(color = "skyblue") + 
  
  hrbrthemes::theme_ipsum(grid = "X") + 
  geom_vline(xintercept = dentist_median,lty = 2, color = "red")

# Try it out with the horizontal lollipop plot function
source("00_custom_functions.R")

oes_2_tbl %>% 
  filter(occ_title == "Dentists, General") %>% 
  mutate(prim_state = factor(prim_state) %>% fct_reorder(h_mean)) %>% 
  
  horizontal_lollipop(h_mean,prim_state)











