#' ---
#' title: create_yoga_example.R
#' author: David M. Freestone (freestoned@wpunj.edu)
#' date: "2020.08.19"
#' purpose:
#' ---

library(tidyverse)

n <- 30
condition <- rep(c("yoga", "neutral"), n/2)
condition <- sample(condition)
condition <- factor(condition, levels = c("neutral", "yoga"))

b0 <- 18
b1 <- -3
sigma <- 2

y <- b0 + b1*(condition == "yoga")
y <- rnorm(n, y, sigma)
y <- round(y)

data <- tibble(condition = condition,
               GAD7 = y)

fit <- lm(GAD7 ~ 0 + condition,
          data = data)
summary(fit)

data %>% write_csv("data/yoga_anxiety.csv")
