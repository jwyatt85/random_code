

require(purrr, quietly = T)
require(dplyr, quietly = T)
require(blme, quietly = T)

### Section: Setting up Data Sets ####
state_FL <- list(
  age = c("18-29", "30-44", "45-64", "65+"),
  gender = c("Male", "Female"),
  race = c("white", "non-white")
)

state_stratas <- state_FL %>%
  cross_d() %>% 
  data.frame() %>% 
  mutate(
    state = "Florida",
    Obama12 = .5120,
    unemployment = .08,
    percent_pop = 1/NROW(.)
  ) %>% 
  map_if(is.factor, as.character) %>% 
  tbl_df()

state_grouping_vars <- list(
  state = c("Florida", "Ohio", "Georgia"),
  Obama12 = c(.5120, .5023, .45949),
  unemployment = c(.08, .10, .09)
) %>% 
  bind_rows() %>% 
  tbl_df

suppressWarnings(
  survey_df <- data.frame(
    age = replicate(10000, sample(c("18-29", "30-44", "45-64", "65+"), prob = c(.25, .30, .25, .20), size = 1)),
    gender = replicate(10000, sample(c("Male", "Female"), prob = c(.48, .52), size = 1)),
    race = replicate(10000, sample(c("white", "non-white"), prob = c(.80, .20), size = 1)),
    dependent_variable = replicate(10000, sample(c(1, 0), prob = c(.60, .40), size = 1)),
    state = as.character(replicate(10000, sample(c("Florida", "Ohio", "Georgia"), prob = c(.60, .20, .20), size = 1)))
  ) %>% 
    left_join(state_grouping_vars, by = "state") %>% 
    map_if(is.factor, as.character) %>% 
    tbl_df
)

#### Section: Multilevel Modeling ####

fm1 <- suppressWarnings(blmer(dependent_variable ~ (1|age) 
             + (1|gender) 
             + (1|race)
             + Obama12
             + unemployment, data = survey_df))

# survey_df$predict <- predict(fm1, type="response")

state_stratas$predicted <- predict(fm1, newdata = state_stratas, type = "response")

state_stratas %>% 
  mutate(
    sum_me = percent_pop * predicted
  ) %>% 
  summarise(FL_MRP = sum(sum_me)) %>% 
  map_dbl(as.numeric)
  




