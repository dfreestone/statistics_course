#' ---
#' title: create_yoga_example.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.08.19"
#' purpose:
#' ---

library(tidyverse)

n <- 50
condition <- rep(c("yoga", "neutral"), n/2)
condition <- sample(condition)
condition <- factor(condition, levels = c("neutral", "yoga"))

b0 <- 17
b1 <- -2
sigma <- 2

mu_pre <- b0
mu_post <- b0 + b1*(condition == "yoga")

y_pre <- rnorm(n, mu_pre, sigma)
y_post <- rnorm(n, mu_post, sigma)

y_pre <- round(y_pre)
y_post <- round(y_post)

data <- tibble(condition = condition,
               pre_GAD7 = y_pre,
               post_GAD7 = y_post)

fit <- lm(post_GAD7 ~ 1 + condition,
          data = data)
summary(fit)

data %>% write_csv("data/yoga_anxiety.csv")
