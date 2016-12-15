
suppressPackageStartupMessages({
  require(purrr, quietly = T)
  require(dplyr, quietly = T)
  require(blme, quietly = T)
})

set.seed(12345)

### Section: Setting up Data Sets ####
state_loop <- c(state.abb[1:9], "DC", state.abb[10:length(state.abb)])
state_list <- lapply(state_loop, function(i){
  list(
    age = c("18-29", "30-44", "45-64", "65+"),
    gender = c("Male", "Female"),
    race = c("white", "non-white"),
    state = i
  )
})

names(state_list) <- state_loop

state_list <- state_list %>% 
  map(function(x){
    x %>% 
      cross_d() %>% 
      data.frame() %>% 
      mutate(
        Obama12 = runif(1, .30, .80),
        unemployment = runif(1, .05, .10),
        percent_pop = 1/NROW(.)
      )
  })

state_grouping_vars <- state_list %>% 
  map_df(function(x){
    x %>% 
      select(state, Obama12, unemployment) %>% 
      unique()
  })

total_reps <- 20000

suppressWarnings(
  survey_df <- data.frame(
    age = replicate(total_reps, sample(c("18-29", "30-44", "45-64", "65+"), prob = c(.25, .30, .25, .20), size = 1)),
    gender = replicate(total_reps, sample(c("Male", "Female"), prob = c(.48, .52), size = 1)),
    race = replicate(total_reps, sample(c("white", "non-white"), prob = c(.80, .20), size = 1)),
    dependent_variable = replicate(total_reps, sample(c(1, 0), prob = c(.50, .50), size = 1)),
    state = as.character(replicate(total_reps, sample(state_loop, size = 1)))
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

### Predict and summarise ####
state_pred <- state_list %>% 
  map(function(x){
    x %>% 
      mutate(
        strat_pred = predict(fm1, newdata = ., type = "response"),
        state_pred = strat_pred * percent_pop
      ) %>% 
      summarise(state_total = sum(state_pred))
  }) %>% 
  bind_rows(.id = "state") 

# readr::write_csv(state_pred, "~/Desktop/state_predictMRP.csv")

# #### Advanced #####
options(scipen=999)
library(vcrpart)
data(movie)
model.24.1 <- olmm(as.factor(critic) ~ movie + re(1|review),
                   data = movie, family = adjacent())

summary(model.24.1)

predict(model.24.1, newdata = movie, type = "response")


model.24.1 <- olmm(rating ~ ce(contact) + ce(temp) + re(1|judge),data = wine)
data2 <- wine %>% 
  select(-rating) %>% 
  dplyr::sample_n(30)

predict(model.24.1, newdata = data2, type = "response")

library(ordinal)
data(wine)

fm1 <- clmm2(rating ~ contact + temp, random = judge, data=wine, link  = 'logistic')
summary(fm1)
cbind(wine, pred = predict(fm1, newdata = wine, type = "response"))
# 
# 
# #### use Mixcat, not sure I like thought ####
library(mixcat)
data(schizo)
attach(schizo)

test1 <- npmlt(y ~ trt + wk,
      # formula.npo=~trt,
      random=~1+trt,
      # id=id,
      k=2,
      EB=FALSE)

test1


