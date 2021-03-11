 # load packages ----------------------------------------------------------------

# https://emilhvitfeldt.github.io/useR2020-text-modeling-tutorial/#133
  
library(tidyverse)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(here)

# set theme for plots to minimal -----------------------------------------------

theme_set(theme_minimal())

# set color --------------------------------------------------------------------

scotblue <- "#0065BF"
ukred <- "#D00C27"

# read data --------------------------------------------------------------------

covid_speeches_scot <- read_rds(here::here("data/covid-speeches-scot.rds")) %>%
  mutate(origin = "Scotland") %>%
  select(-location, -title, -abstract) %>%
  rownames_to_column(var = "speech_id")
  
covid_speeches_uk <- read_rds(here::here("data/covid-speeches-uk.rds")) %>%
  mutate(origin = "UK") %>%
  select(-title, -abstract) %>%
  rownames_to_column(var = "speech_id")

covid_speeches <- bind_rows(covid_speeches_scot, covid_speeches_uk) %>%
  mutate(origin = as.factor(origin))

# separate into sentences ------------------------------------------------------

covid_speeches_sentences <- covid_speeches %>%
  unnest_tokens(sentence, text, token = "sentences")

# check balance ----------------------------------------------------------------

# not very balanced, could be an issue

ggplot(covid_speeches_sentences, aes(x = origin)) +
  geom_bar()

# train / test -----------------------------------------------------------------

set.seed(1234)
covid_split <- initial_split(covid_speeches_sentences, strata = origin)
covid_train <- training(covid_split)
covid_test  <- testing(covid_split)

# specify model ----------------------------------------------------------------

lasso_mod <- logistic_reg(penalty = 0.005, mixture = 1) %>%
  set_engine("glmnet")

# build recipe -----------------------------------------------------------------

covid_rec <- recipe(origin ~ sentence, data = covid_train) %>%
  # tokenize into words
  step_tokenize(sentence, token = "words") %>%
  # filter out stop words
  step_stopwords(sentence) %>%
  # all the 1-grams followed by all the 2-grams followed by all the 3-grams
  step_ngram(sentence, num_tokens = 3, min_num_tokens = 1) %>%
  # keep the 500 most frequent words to avoid creating too many variables 
  step_tokenfilter(sentence, max_tokens = 500) %>%
  # calculate tf-idf
  step_tfidf(sentence)

# build workflow ---------------------------------------------------------------

covid_wflow <- workflow() %>%
  add_model(lasso_mod) %>%
  add_recipe(covid_rec)

# cv ---------------------------------------------------------------------------

set.seed(1234)
covid_folds <- vfold_cv(covid_train, v = 10, strata = origin)

# fit resamples ----------------------------------------------------------------

covid_fit_rs <- covid_wflow %>%
  fit_resamples(
    covid_folds,
    control = control_resamples(save_pred = TRUE)
  )

covid_train_metrics <- collect_metrics(covid_fit_rs)
covid_train_pred <- collect_predictions(covid_fit_rs)

covid_train_pred %>%
  group_by(id) %>%
  roc_curve(truth = origin, .pred_Scotland) %>%
  autoplot() +
  labs(
    title = "ROC curve for Scotland & UK COVID speeches",
    subtitle = "Each resample fold is shown in a different color"
  )

# make predictions for test data -----------------------------------------------

covid_fit <- covid_wflow %>%
  fit(data = covid_train)

covid_test_pred <- predict(covid_fit, new_data = covid_test, type = "prob") %>%
  bind_cols(covid_test %>% select(origin, speech_id, sentence))

covid_test_pred %>%
  roc_curve(truth = origin, .pred_Scotland) %>%
  autoplot()

covid_test_pred %>%
  roc_auc(truth = origin, .pred_Scotland)

covid_test_pred %>% 
  filter(origin == "Scotland", .pred_UK > 0.5)

# what decisions did we enforce? -----------------------------------------------

# step_tokenfilter(sentence, max_tokens = 500) -- why 500 for max_tokens?
# logistic_reg(penalty = 0.005, mixture = 1) -- why 0.005 for penalty?

# tune -------------------------------------------------------------------------

# specify model 

lasso_mod_tune <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>% 
  set_mode("classification")

# build recipe 

covid_rec_tune <- recipe(origin ~ sentence, data = covid_train) %>%
  step_tokenize(sentence, token = "words") %>%
  step_stopwords(sentence) %>%
  step_ngram(sentence, num_tokens = 3, min_num_tokens = 1) %>%
  # keep the ?? most frequent words to avoid creating too many variables 
  step_tokenfilter(sentence, max_tokens = tune(), min_times = 5) %>%
  step_tfidf(sentence)

# build workflow 

covid_wflow_tune <- workflow() %>%
  add_model(lasso_mod_tune) %>%
  add_recipe(covid_rec_tune)

# grid of possible hyperparameters

param_grid <- grid_regular(
  penalty(range = c(-4, 0)),
  max_tokens(range = c(500, 2000)),
  levels = 5
)

# train models with all possible values of tuning parameters
#set.seed(24)
#covid_fit_rs_tune <- tune_grid(
#  covid_wflow_tune,
#  resamples = covid_folds,
#  grid = param_grid, 
#  control = control_grid(save_pred = TRUE)
#)

write_rds(covid_fit_rs_tune, here::here("model-output", "covid_fit_rs_tune.rds"), compress = "bz2")

covid_fit_rs_tune <- read_rds(here::here("model-output", "covid_fit_rs_tune.rds"))

collect_metrics(covid_fit_rs_tune)

autoplot(covid_fit_rs_tune)

covid_fit_rs_tune %>%
  show_best("roc_auc")

best_roc_auc <- select_best(covid_fit_rs_tune, "roc_auc")

best_roc_auc

# evaluate best model ----------------------------------------------------------

collect_predictions(covid_fit_rs_tune, parameters = best_roc_auc) %>%
  group_by(id) %>%
  roc_curve(truth = origin, .pred_Scotland) %>%
  autoplot() +
  labs(
    title = "ROC curve for Scotland & UK COVID speeches",
    subtitle = "Each resample fold is shown in a different color"
  )

covid_wflow_final <- finalize_workflow(covid_wflow_tune, best_roc_auc)

# variable importance ----------------------------------------------------------

library(vip)

#vi_data <- covid_wflow_final %>%
#  fit(covid_train) %>%
#  pull_workflow_fit() %>%
#  vi(lambda = best_roc_auc$penalty) %>%
#  mutate(Variable = str_remove_all(Variable, "tfidf_sentence_")) %>%
#  filter(Importance != 0)

write_rds(vi_data, here::here("model-output", "vi_data.rds"), compress = "bz2")

vi_data <- read_rds(here::here("model-output", "vi_data.rds"))

vi_data %>%
  mutate(
    Importance = abs(Importance)
  ) %>%
  filter(Importance != 0) %>%
  group_by(Sign) %>%
  slice_head(n = 20) %>%
  ungroup() %>%
  mutate(pred_origin = if_else(Sign == "POS", "UK", "Scotland")) %>% 
  ggplot(aes(
    x = Importance,
    y = fct_reorder(Variable, Importance),
    fill = pred_origin
  )) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c(scotblue, ukred)) +
  facet_wrap(~pred_origin, scales = "free") +
  labs(
    y = NULL
  )

# final fit --------------------------------------------------------------------

#covid_fit_final <- last_fit(
#  covid_wflow_final, 
#  covid_split
#)
#
#write_rds(covid_fit_final, here::here("model-output", "covid_fit_final.rds"), compress = "bz2")

covid_fit_final <- read_rds(here::here("model-output", "covid_fit_final.rds"))

covid_fit_final %>%
  collect_metrics()

covid_fit_final %>%
  collect_predictions() %>%
  roc_curve(truth = origin, .pred_Scotland) %>%
  autoplot()

# predict ----------------------------------------------------------------------

scot_sentence <- covid_train %>%
  filter(origin == "Scotland", str_detect(sentence, "physical")) %>%
  slice(2)

scot_sentence$sentence

scot_sentence %>%
  tidytext::unnest_tokens(words, sentence) %>%
  left_join(vi_data, by = c("words" = "Variable")) %>%
  mutate(pred_origin = if_else(Sign == "NEG", "Scotland", "UK")) %>%
  select(-url) %>%
  print(n = 25)

uk_sentence <- covid_train %>%
  filter(origin == "UK", str_detect(sentence, "scotland")) %>%
  slice(2)

uk_sentence$sentence

uk_sentence %>%
  tidytext::unnest_tokens(words, sentence) %>%
  left_join(vi_data, by = c("words" = "Variable")) %>%
  mutate(pred_origin = if_else(Sign == "NEG", "Scotland", "UK")) %>%
  select(-url) %>%
  print(n = 25)

