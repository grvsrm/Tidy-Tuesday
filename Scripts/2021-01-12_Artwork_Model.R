# Pre requisites
library(tidyverse)
library(here)
library(tidymodels)
library(tidytext)
library(textrecipes)

theme_set(theme_light())

# Load the data
artwork <- read_rds(here("data", "artwork_raw.rds"))


artwork %>%
    ggplot(aes(year)) +
    geom_histogram()



tate_df <- artwork %>% 
    filter(year > 1750) %>% 
    select(year, medium) %>% 
    drop_na()


tate_df %>% 
    unnest_tokens(word, medium) %>% 
    count(word, sort = T)


# Data Split
set.seed(123)

art_split <- tate_df %>% 
    initial_split(strata = year)

art_train <- art_split %>% 
    training()
art_test <- art_split %>% 
    testing()

# Resamples
set.seed(234)
art_folds <- art_train %>% 
    vfold_cv(strata = year)

# Recipe

art_rec <- art_train %>% 
    recipe(year~medium) %>% 
    step_tokenize(medium) %>% 
    step_stopwords(medium) %>% 
    step_tokenfilter(medium, max_tokens = 500) %>% 
    step_tfidf(medium)

art_rec %>% 
    prep() %>% juice()

# Model
lasso_reg <- linear_reg(penalty = tune(),
           mixture = 1) %>% 
    set_engine("glmnet") %>% 
    set_mode("regression")


# Workflow

sparse_bp <- hardhat::default_recipe_blueprint(composition = "dgCMatrix")

art_wf <- workflow() %>% 
    add_recipe(art_rec, blueprint = sparse_bp) %>% 
    add_model(lasso_reg)

# Tuning
lambda_grid <- grid_regular(penalty(range = c(-3,0)), levels = 20)

# Fit
doParallel::registerDoParallel()

lasso_rs <- tune_grid(art_wf,
          resamples = art_folds,
          grid = lambda_grid)


# Results
lasso_rs %>% 
    show_best()

# Save 
best_rse <- lasso_rs %>% 
    select_best("rmse")

finla_lasso <- art_wf %>% 
    finalize_workflow(best_rse)

final_fit <- finla_lasso %>% 
    last_fit(art_split)

final_fit %>% 
    collect_metrics()


# Analysis of Results
library(vip)
art_vi <- final_fit$.workflow[[1]] %>%
    pull_workflow_fit() %>% 
    vi()

art_vi %>% 
    group_by(Sign) %>% 
    slice_max(abs(Importance), n = 20) %>% 
    mutate(Variable = str_remove(Variable, "tfidf_medium_"),
           Importance = abs(Importance),
           Variable = fct_reorder(Variable, Importance),
           Remark = if_else(Sign == "POS", "Popular in recent times", "Popular in Old Times")) %>%
    ggplot(aes(Importance, Variable, fill = Remark)) +
    geom_col(show.legend = F) +
    facet_wrap(~Remark, scales = "free")


# End of Script