

# Biostats Core Methods Practice ----

# Run through the model-fitting for the various research questions.

# Libraries ----
pacman::p_load(
  tidyverse,
  tidymodels,
  tidyquant,
  gt,
  gtsummary,
  scales
)


# Call data ----
# Just command+enter the 'mpg' line below, and tidymodels will call it forth for you!
mpg

# See a description of the dataset here: https://ggplot2.tidyverse.org/reference/mpg.html
# Or just type this
?mpg

# Now assign mpg to an object (to have it stored in your environment)
mpg_tbl <- mpg



# 1) Biostats II ----
# Predict highway gas mileage (a continuous outcome)

# First "skim" the data (to get a sense for what you're dealing with)
mpg_tbl %>% skimr::skim()

# What are the fuel types?
mpg_tbl %>% distinct(fl)

## Prep for model fit ----
mpg_tbl %>% 
  mutate(
    year = factor(year),
    manufacturer = factor(manufacturer),
    manufacturer = relevel(manufacturer,ref = "ford")
  ) -> mpg_tbl

## Model fit ----
fit_lm <- lm(hwy ~ fl + drv + cyl + year + manufacturer,
             data = mpg_tbl)

# My almost good enough way
fit_lm %>% tidied_model_lm() %>% gt() %>% my_bold_plus()

# Sjoberg's better way
fit_lm %>% tbl_regression()
  
# But my way is needed for the dot whisker plot
fit_lm %>% tidied_model_lm() %>% 
  plot_dotwhisker(
    my_title = "Highway Miles Per Gallon Adjusted Linear Relationships"
  )

fit_lm %>% tidied_model_lm() %>% 
  plot_dotwhisker(
    my_title = "Highway Miles Per Gallon Adjusted Linear Relationships",
    labels = F
  )



# 2) Logistic regression ----
prostate_tbl <- read_csv("00_data/Prostate_Frequency_Class.csv") %>% select(-c(1:2))
prostate_tbl

prostate_tbl %>% 
  mutate(
    capsule = factor(capsule),
    capsule = relevel(capsule,ref = "0")
  ) -> prostate_tbl

fit_glm <- glm(
  capsule ~ dpros + psa,
  data = prostate_tbl,
  family = binomial
)

fit_glm %>% tbl_regression(exponentiate = T)



# 3) Survival ----
ovarian_tbl %>% distinct(ca125_level)







