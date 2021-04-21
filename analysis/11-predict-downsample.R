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

covid_rec_ds <- recipe(origin ~ sentence, data = covid_train) %>%
  # downsample
  themis::step_downsample(origin) %>%
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

covid_wflow_ds <- workflow() %>%
  add_model(lasso_mod) %>%
  add_recipe(covid_rec_ds)

# cv ---------------------------------------------------------------------------

set.seed(1234)
covid_folds <- vfold_cv(covid_train, v = 10, strata = origin)

# fit resamples ----------------------------------------------------------------

covid_fit_rs_ds <- covid_wflow_ds %>%
  fit_resamples(
    covid_folds,
    control = control_resamples(save_pred = TRUE)
  )

covid_train_metrics_ds <- collect_metrics(covid_fit_rs_ds)
covid_train_pred_ds <- collect_predictions(covid_fit_rs_ds)

covid_train_pred_ds %>%
  group_by(id) %>%
  roc_curve(truth = origin, .pred_Scotland) %>%
  autoplot() +
  labs(
    title = "ROC curve for Scotland & UK COVID speeches, with downsampling",
    subtitle = "Each resample fold is shown in a different color"
  )

# make predictions for test data -----------------------------------------------

covid_fit_ds <- covid_wflow_ds %>%
  fit(data = covid_train)

covid_test_pred_ds <- predict(covid_fit_ds, new_data = covid_test, type = "prob") %>%
  bind_cols(covid_test %>% select(origin, speech_id, sentence))

covid_test_pred_ds %>%
  roc_curve(truth = origin, .pred_Scotland) %>%
  autoplot()

covid_test_pred_ds %>%
  roc_auc(truth = origin, .pred_Scotland)

covid_test_pred_ds %>% 
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

covid_rec_tune_ds <- recipe(origin ~ sentence, data = covid_train) %>%
  themis::step_downsample(origin) %>%
  step_tokenize(sentence, token = "words") %>%
  step_stopwords(sentence) %>%
  step_ngram(sentence, num_tokens = 3, min_num_tokens = 1) %>%
  # keep the ?? most frequent words to avoid creating too many variables 
  step_tokenfilter(sentence, max_tokens = tune(), min_times = 5) %>%
  step_tfidf(sentence)

# build workflow 

covid_wflow_tune_ds <- workflow() %>%
  add_model(lasso_mod_tune) %>%
  add_recipe(covid_rec_tune_ds)

# grid of possible hyperparameters

param_grid <- grid_regular(
  penalty(range = c(-4, 0)),
  max_tokens(range = c(500, 1500)), # use lower upper limit than not downsampling approach
  levels = 5
)

# train models with all possible values of tuning parameters
#set.seed(24)
#covid_fit_rs_tune_ds <- tune_grid(
#  covid_wflow_tune_ds,
#  resamples = covid_folds,
#  grid = param_grid, 
#  control = control_grid(save_pred = TRUE)
#)
#
#write_rds(covid_fit_rs_tune_ds, here::here("model-output", "covid_fit_rs_tune_ds.rds"), compress = "xz")

covid_fit_rs_tune_ds <- read_rds(here::here("model-output", "covid_fit_rs_tune_ds.rds"))

collect_metrics(covid_fit_rs_tune_ds)

autoplot(covid_fit_rs_tune_ds)

covid_fit_rs_tune_ds %>%
  show_best("roc_auc")

best_roc_auc_ds <- select_best(covid_fit_rs_tune_ds, "roc_auc")

best_roc_auc_ds

# evaluate best model ----------------------------------------------------------

collect_predictions(covid_fit_rs_tune_ds, parameters = best_roc_auc_ds) %>%
  group_by(id) %>%
  roc_curve(truth = origin, .pred_Scotland) %>%
  autoplot() +
  labs(
    title = "ROC curve for Scotland & UK COVID speeches",
    subtitle = "Each resample fold is shown in a different color"
  )

covid_wflow_final_ds <- finalize_workflow(covid_wflow_tune_ds, best_roc_auc_ds)

# variable importance ----------------------------------------------------------

library(vip)

#vi_data_ds <- covid_wflow_final_ds %>%
#  fit(covid_train) %>%
#  pull_workflow_fit() %>%
#  vi(lambda = best_roc_auc_ds$penalty) %>%
#  mutate(Variable = str_remove_all(Variable, "tfidf_sentence_")) %>%
#  filter(Importance != 0)
#
#write_rds(vi_data_ds, here::here("model-output", "vi_data_ds.rds"), compress = "bz2")

vi_data_ds <- read_rds(here::here("model-output", "vi_data_ds.rds"))

vi_data_ds %>%
  mutate(
    Importance = abs(Importance)
  ) %>%
  filter(Importance != 0) %>%
  group_by(Sign) %>%
  slice_head(n = 40) %>%
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

#covid_fit_final_ds <- last_fit(
#  covid_wflow_final_ds, 
#  covid_split
#)
#
#write_rds(covid_fit_final_ds, here::here("model-output", "covid_fit_final_ds.rds"), compress = "bz2")

covid_fit_final_ds <- read_rds(here::here("model-output", "covid_fit_final_ds.rds"))

covid_fit_final_ds %>%
  collect_metrics()

covid_fit_final_ds %>%
  collect_predictions() %>%
  roc_curve(truth = origin, .pred_Scotland) %>%
  autoplot()

# predict ----------------------------------------------------------------------

scot_sentence <- covid_test %>%
  filter(origin == "Scotland", str_detect(sentence, "physical")) %>%
  slice(2)

scot_sentence$sentence

scot_sentence %>%
  tidytext::unnest_tokens(words, sentence) %>%
  left_join(vi_data_ds, by = c("words" = "Variable")) %>%
  mutate(pred_origin = if_else(Sign == "NEG", "Scotland", "UK")) %>%
  select(-url) %>%
  filter(!is.na(Sign))

uk_sentence <- covid_test %>%
  filter(origin == "UK", str_detect(sentence, "scotland")) %>%
  slice(2)

uk_sentence$sentence

uk_sentence %>%
  tidytext::unnest_tokens(words, sentence) %>%
  left_join(vi_data_ds, by = c("words" = "Variable")) %>%
  mutate(pred_origin = if_else(Sign == "NEG", "Scotland", "UK")) %>%
  select(-url) %>%
  filter(!is.na(Sign))

scot_sentence_disease <- covid_test %>%
  filter(origin == "Scotland", str_detect(sentence, "disease")) %>%
  slice(1)

scot_sentence_disease$sentence

scot_sentence_disease %>%
  tidytext::unnest_tokens(words, sentence) %>%
  left_join(vi_data_ds, by = c("words" = "Variable")) %>%
  mutate(pred_origin = if_else(Sign == "NEG", "Scotland", "UK")) %>%
  select(-url) %>%
  filter(!is.na(Sign))

scot_sentence_freedom <- covid_test %>%
  filter(origin == "Scotland", str_detect(sentence, "freedom")) %>%
  slice(1)

scot_sentence_freedom$sentence

scot_sentence_freedom %>%
  tidytext::unnest_tokens(words, sentence) %>%
  left_join(vi_data_ds, by = c("words" = "Variable")) %>%
  mutate(pred_origin = if_else(Sign == "NEG", "Scotland", "UK")) %>%
  select(-url) %>%
  filter(!is.na(Sign))
