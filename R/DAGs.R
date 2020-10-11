#' ---
#' title: DAGs.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.10.09"
#' purpose:
#' ---
library(magrittr)
library(tidyverse)
library(lavaan)
library(dagitty)
library(ggdag)
library(glue)

# ----------------------------  FUNCTIONS  -------------------------------------
draw_dag <- function(dag) {
  ggdag::ggdag(dag,
               node_size = 21,
               text_size = 3) +
    ggdag::theme_dag_blank()
}
create_dag <- function(model) {
  positions <- get_dag_positions(model)
  nodes <- get_dag_nodes(model)
  dag <- paste0("dag{\n", positions, "\n\n", nodes, "\n}")
  return(dag)
}
get_dag_nodes <- function(model) {
  model <- filter(lavaanify(model),  op == "~")
  nodes <- glue("{model$lhs} <- {model$rhs}") %>%
    glue_collapse(sep = "\n")
 return(nodes)
}


get_dag_positions <- function(model) {
  model <- filter(lavaanify(model),  op == "~")
  terms <- unique(c(model$lhs, model$rhs))

  positions <- tibble(term = "", txt="") %>%
    add_row(term = "dose", txt = 'dose [exposure, pos = "-10,0"]') %>%
    add_row(term = "activity", txt = 'activity [pos = "0,0"]') %>%
    add_row(term = "phq9", txt = 'phq9 [outcome, pos = "10,0"]') %>%
    add_row(term = "weight", txt = 'weight [pos = "0,10"]') %>%
    add_row(term = "sunlight", txt = 'sunlight [pos = "0, -5"]') %>%
    add_row(term = "attention", txt = 'attention [pos = "0,-10"]') %>%
    filter(term %in% terms)

  positions <- glue_collapse(positions$txt, sep = "\n")

  return(positions)
}

rescale_vars <- function(df) {
  scale_tbl <- tibble(name="", mean=0,  sd=0) %>%
    add_row(name="dose",       mean = 250, sd = 25) %>%
    add_row(name="activity",   mean = 50,  sd = 10) %>%
    add_row(name="phq9",       mean = 16,  sd = 2) %>%
    add_row(name="attention",  mean = 75,  sd = 4) %>%
    add_row(name="sunlight",   mean = 45,  sd = 3) %>%
    add_row(name="weight",     mean = 150, sd = 20)

  it <- df %>%
    pivot_longer(cols = everything(),
                 names_to = "name",
                 values_to = "value") %>%
    arrange(name) %>%
    group_by(name) %>%
    mutate(id = 1:n()) %>%
    ungroup() %>%
    left_join(scale_tbl, by = "name") %>%
    mutate(.x = mean + sd*value) %>%
    select(id, name, .x) %>%
    pivot_wider(names_from = "name",
                values_from = ".x") %>%
    select(-id)

  return(it)
}

unscale_vars <- function(df) {
  it <- mutate(df, across(-c("phq9"), ~(.x - mean(.x)) / sd(.x)))
 return(it)
}

# ----------------------------  SCRIPT  ----------------------------------------

fork <- '
  dose ~ 0.6*weight
  phq9 ~ -0.6*weight + -0.5*dose
'
pipe <- '
  activity ~ 0.4*dose
  phq9 ~ -0.8*activity
'

collider <- '
  attention ~ -0.6*dose + -0.4*phq9
  phq9 ~ -0.6*dose
'
descendant <- '
  activity ~ 0.8*dose
  sunlight ~ 0.8*activity
  phq9 ~ -0.6*activity
'

all_confounds <- '
  dose ~ 0.6*weight
  activity ~ 0.4*dose
  phq9 ~ -0.6*weight + -0.8*activity
  attention ~ -0.6*dose + -0.4*phq9
  sunlight ~ 0.8*activity
'

models <- list(fork = fork,
               pipe = pipe,
               collider = collider,
               descendant = descendant,
               all_confounds = all_confounds)
dag <- map(models, create_dag)
adjustment_sets <- map(dag, adjustmentSets)
implied_independence <- map(dag, impliedConditionalIndependencies)
map(dag, draw_dag)

data <- map(models, lavaan::simulateData, sample.nobs = 100)
data <- map(data, as_tibble)
data <- map(data, rescale_vars)
data$all_confounds %>% write_csv("data/dose_phq_DAG.csv")
data <- map(data, unscale_vars)


# The fork (condition on weight!)
lm(phq9 ~ 1 + dose, data=data$fork) %>% summary()
lm(phq9 ~ 1 + weight, data=data$fork) %>% summary()
lm(phq9 ~ 1 + dose + weight, data=data$fork) %>% summary()

# The pipe (don't condition on activity!)
lm(phq9 ~ 1 + dose, data=data$pipe) %>% summary()
lm(phq9 ~ 1 + activity, data=data$pipe) %>% summary()
lm(phq9 ~ 1 + dose + activity, data=data$pipe) %>% summary()

# The collider (don't condition on attention!)
lm(phq9 ~ 1 + dose, data=data$collider) %>% summary()
lm(phq9 ~ 1 + attention, data=data$collider) %>% summary()
lm(phq9 ~ 1 + dose + attention, data=data$collider) %>% summary()

# The descendent (don't condition on sunlight!)
lm(phq9 ~ 1 + dose, data=data$descendant) %>% summary()
lm(phq9 ~ 1 + sunlight, data=data$descendant) %>% summary()
lm(phq9 ~ 1 + dose + sunlight, data=data$descendant) %>% summary()

# All confounds (only condition on age)
lm(phq9 ~ 1 + dose, data=data$all_confounds) %>% summary()
lm(phq9 ~ 1 + dose + weight + activity + sunlight + attention, data=data$all_confounds) %>% summary()
lm(phq9 ~ 1 + dose + weight, data=data$all_confounds) %>% summary() # CORRECT

# Conditinal independences all look good
# [x] actv _||_ age | dose
lm(activity ~ 1 + weight + dose, data=data$all_confounds) %>% summary()
# [x] actv _||_ attn | dose, phq9
lm(activity ~ 1 + attention + dose + phq9, data=data$all_confounds) %>% summary()
# [x] age _||_ attn | dose, phq9
lm(weight ~ 1 + attention + dose + phq9, data=data$all_confounds) %>% summary()
# [x] age _||_ snlg | actv
lm(weight ~ 1 + sunlight + activity, data=data$all_confounds) %>% summary()
# [x] age _||_ snlg | dose
lm(weight ~ 1 + sunlight + dose, data=data$all_confounds) %>% summary()
# [x] attn _||_ snlg | actv
lm(attention ~ 1 + sunlight + activity, data=data$all_confounds) %>% summary()
# [x] attn _||_ snlg | dose, phq9
lm(attention ~ 1 + sunlight + dose + phq9, data=data$all_confounds) %>% summary()
# [x] dose _||_ phq9 | actv, age
lm(dose ~ 1 + phq9 + activity + weight, data=data$all_confounds) %>% summary()
# [x] dose _||_ snlg | actv
lm(dose ~ 1 + sunlight + activity, data=data$all_confounds) %>% summary()
# [x] phq9 _||_ snlg | actv
lm(phq9 ~ 1 + sunlight + activity, data=data$all_confounds) %>% summary()

