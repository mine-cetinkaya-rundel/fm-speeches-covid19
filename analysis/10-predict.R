# load packages ----------------------------------------------------------------

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
  mutate(origin = "Scotland")
  
covid_speeches_uk <- read_rds(here::here("data/covid-speeches-uk.rds")) %>%
  mutate(origin = "UK")

covid_speeches <- bind_rows(covid_speeches_scot, covid_speeches_uk)

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

# recipe -----------------------------------------------------------------------

covid_rec <- recipe(origin ~ sentence, data = covid_train) %>%
  step_tokenize(sentence, token = "words") %>%
  step_stopwords(sentence) %>%
  step_ngram(sentence, num_tokens = 3, min_num_tokens = 1) %>%
  step_tokenfilter(sentence, max_tokens = tune(), min_times = 5) %>%
  step_tfidf(sentence)


# model ------------------------------------------------------------------------

lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

# cv ---------------------------------------------------------------------------

covid_folds <- vfold_cv(covid_train, v = 10, strata = origin)

# workflow ---------------------------------------------------------------------

covid_wflow <- workflow() %>%
  add_recipe(covid_rec) %>%
  add_model(lasso_spec)

# tune -------------------------------------------------------------------------

# A grid of possible hyperparameters
param_grid <- grid_regular(
  penalty(range = c(-4, 0)),
  max_tokens(range = c(500, 2000)),
  levels = 6
)

set.seed(42)
lasso_rs <- tune_grid(
  covid_wflow,
  resamples = covid_folds,
  grid = param_grid, 
  control = control_grid(save_pred = TRUE)
)

#write_rds(lasso_rs, here::here("model-output", "lasso_rs.rds"), compress = "bz2")

lasso_rs <- read_rds(here::here("model-output", "lasso_rs.rds"))

collect_metrics(lasso_rs)

autoplot(lasso_rs)

lasso_rs %>%
  show_best("roc_auc")

best_roc_auc <- select_best(lasso_rs, "roc_auc")

best_roc_auc

# evaluate best model ----------------------------------------------------------

collect_predictions(lasso_rs, parameters = best_roc_auc) %>%
  group_by(id) %>%
  roc_curve(truth = origin, .pred_Scotland) %>%
  autoplot() +
  labs(
    title = "ROC curve for Scotland & UK COVID speeches",
    subtitle = "Each resample fold is shown in a different color"
  )

wflow_spec_final <- finalize_workflow(covid_wflow, best_roc_auc)

# variable importance ----------------------------------------------------------

library(vip)

vi_data <- wflow_spec_final %>%
  fit(covid_train) %>%
  pull_workflow_fit() %>%
  vi(lambda = best_roc_auc$penalty) %>%
  mutate(Variable = str_remove_all(Variable, "tfidf_sentence_")) %>%
  filter(Importance != 0)

#write_rds(vi_data, here::here("model-output", "vi_data.rds"), compress = "bz2")

vi_data <- read_rds(here::here("model-output", "vi_data.rds"))

vi_data %>%
  mutate(
    Importance = abs(Importance)
  ) %>%
  filter(Importance != 0) %>%
  group_by(Sign) %>%
  top_n(20, Importance) %>%
  ungroup() %>%
  mutate(Sign = factor(Sign, c("POS", "NEG"), c("UK", "Scotland"))) %>%
  ggplot(aes(
    x = Importance,
    y = fct_reorder(Variable, Importance),
    fill = Sign
  )) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c(ukred, scotblue)) +
  facet_wrap(~Sign, scales = "free") +
  labs(
    y = NULL
  )

# sample sentences -------------------------------------------------------------

max_imp <- log(max(abs(vi_data$Importance)))
log_neg <- function(x) {
  sign(x) * log(abs(x))
}
range01 <- function(x) {
  (log_neg(x) + (max_imp)) / (max_imp + max_imp)
}
color_fun <- scales::colour_ramp(colors = c(scotblue, ukred))
highlighter <- function(x, sign) {
  if(is.na(sign)) {
    htmltools::span(x)
  } else {
    htmltools::span(htmltools::tags$em(x), style = glue::glue('color:{color_fun(range01(sign))};'))
  }
}

# Scotland 1

covid_train %>%
  filter(origin == "Scotland", nchar(sentence) < 800 & nchar(sentence) > 400) %>%
  slice(1) %>%
  tidytext::unnest_tokens(words, sentence) %>%
  left_join(vi_data, by = c("words" = "Variable")) %>%
  mutate(words = map2(words, Importance, highlighter)) %>%
  pull(words) %>%
  htmltools::div()

# Scotland 2

covid_train %>%
  filter(origin == "Scotland", nchar(sentence) < 800 & nchar(sentence) > 400) %>%
  slice(2) %>%
  tidytext::unnest_tokens(words, sentence) %>%
  left_join(vi_data, by = c("words" = "Variable")) %>%
  mutate(words = map2(words, Importance, highlighter)) %>%
  pull(words) %>%
  htmltools::div()

# UK 1

covid_train %>%
  filter(origin == "UK", nchar(sentence) < 800 & nchar(sentence) > 400) %>%
  slice(1) %>%
  tidytext::unnest_tokens(words, sentence) %>%
  left_join(vi_data, by = c("words" = "Variable")) %>%
  mutate(words = map2(words, Importance, highlighter)) %>%
  pull(words) %>%
  htmltools::div()

# UK 2

covid_train %>%
  filter(origin == "UK", nchar(sentence) < 800 & nchar(sentence) > 400) %>%
  slice(2) %>%
  tidytext::unnest_tokens(words, sentence) %>%
  left_join(vi_data, by = c("words" = "Variable")) %>%
  mutate(words = map2(words, Importance, highlighter)) %>%
  pull(words) %>%
  htmltools::div()

# final fit --------------------------------------------------------------------

final_fit <- last_fit(
  wflow_spec_final, 
  covid_split
)

#write_rds(final_fit, here::here("model-output", "final_fit.rds"), compress = "bz2")

final_fit <- read_rds(here::here("model-output", "final_fit.rds"))


final_fit %>%
  collect_metrics()

final_fit %>%
  collect_predictions() %>%
  roc_curve(truth = origin, .pred_Scotland) %>%
  autoplot()

